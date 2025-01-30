(uiop:define-package #:sento.bench
  (:use #:cl)
  (:import-from #:org.shirakumo.trivial-benchmark
                #:*default-samplers*
                #:define-sampler
                #:with-timing)
  (:import-from #:serapeum
                #:eval-always
                #:defvar-unbound)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:sento.queue
                #:queue-full-error))
(in-package #:sento.bench)


(defun actor-queue-size (a)
  (let* ((msgbox (sento.actor-cell:msgbox a))
         (queue (slot-value msgbox
                            'sento.messageb::queue)))
    (sento.queue:queued-count queue)))


(defun total-queues-size (system)
  (+ (loop :for actor :in (sento.actor-system::%all-actors system :user)
           :summing (actor-queue-size actor))
     (loop :for actor :in (sento.actor-system::%all-actors system :internal)
           :summing (actor-queue-size actor))))


(defvar-unbound *num-processed-messages*
  "We will bind this varible during a benchmark.")


(eval-always
  (define-sampler messages-per-second (test-duration processed-messages-count)
    (:measure (form)
              (with-gensyms (test-started-at)
                `(let ((*num-processed-messages* 0)
                       (,test-started-at (get-internal-real-time)))
                  
                   ;; Benchmark code will be called here:
                   ,form
                  
                   (setf ,test-duration
                         (- (get-internal-real-time)
                            ,test-started-at))
                   (setf ,processed-messages-count
                         *num-processed-messages*))))
    (:commit (commit-fn)
             `(,commit-fn messages-per-second
                          (/ (float ,processed-messages-count 0d0)
                             (/ ,test-duration
                                internal-time-units-per-second))))))


(eval-always
  (defparameter *samplers*
    (list* 'messages-per-second
           *default-samplers*)))


;; Use only our message counter.
;; *default-samplers* includes different system
;; metrics similar to metrics TIME macro collects.
;; (eval-always
;;   (defparameter *samplers*
;;     (list 'messages-per-second)))


(defun run-benchmark (&key
                      (dispatcher :pinned)
                      (with-reply-p nil)
                      (async-ask-p nil)
                      (num-shared-workers 8)
                      ;; When queue-size is given, then Actor will be created
                      ;; with bound-queue. Otherwise, queue will be unbound.
                      ;; To prevent unbound-queue grow, set wait-if-queue-larger-than
                      ;; argument to some value.
                      (queue-size nil queue-size-given-p)
                      ;; When actor's goes abover this value,
                      ;; generator threads will stop and wait while
                      ;; actor will process some messages from the queue.
                      ;; Can be turned off if set to NIL, but this could
                      ;; lead to a high memory consumption and probably
                      ;; program failure.

                      ;; This setting applies some kind of backpressure,
                      ;; when queue-size is 0 and no other way
                      ;; to keep generators from filling all the memory
                      ;; with messages.
                      (wait-if-queue-larger-than 10000 wait-if-queue-larger-than-given-p)
                      (duration 10)
                      (num-iterations 60)
                      (load-threads 8)
                      (time-out nil))
  
  (log:config :warn)

  (check-type dispatcher (member :shared :pinned))

  ;; Leave only one default
  (when (and queue-size-given-p
             (not wait-if-queue-larger-than-given-p))
    (setf wait-if-queue-larger-than nil))

  (when (and (not queue-size-given-p)
             wait-if-queue-larger-than-given-p)
    (setf queue-size nil))

  (when (and queue-size
             (not (zerop queue-size))
             wait-if-queue-larger-than)
    (error "Argument WAIT-IF-QUEUE-LARGE-THAN does not makes sense when QUEUE-SIZE is not zero."))
  
  (when (and async-ask-p
             (not with-reply-p))
    (error "Argument ASYNC-ASK-P should be given together with WITH-REPLY-P argument."))

  ;; It is useful to save benchmark results along with all params
  ;; used to run the benchmark.
  (format t "~2&Results for benchmark: ~S~%"
          (list :dispatcher dispatcher
                :with-reply-p with-reply-p
                :async-ask-p  async-ask-p 
                :num-shared-workers num-shared-workers
                :queue-size queue-size
                :wait-if-queue-larger-than wait-if-queue-larger-than
                :time-out time-out))
  (force-output)
  
  (with-timing (num-iterations
                :samplers *samplers*)
    (let ((counter 0)
          (stop-at (+ (get-internal-real-time)
                      (* duration internal-time-units-per-second))))
      (flet ((receiver (msg)
               (declare (ignore msg))
               (incf counter)))
        (let* ((system (asys:make-actor-system `(:dispatchers (:shared (:workers ,num-shared-workers)))))
               (actor (ac:actor-of system
                                   :receive #'receiver
                                   :dispatcher dispatcher
                                   :queue-size queue-size)))
          (flet ((sender ()
                   (loop :with check-every = 1000
                         :for iteration :upfrom 0
                         :while (< (get-internal-real-time)
                                  stop-at)
                         :do (cond
                              ((and wait-if-queue-larger-than
                                    ;; Calling queue-size function
                                    ;; requires lock acquisition which hits performance
                                    ;; and makes message generation up to 10 times slower
                                    ;; depending on generator threads cound.
                                    ;; That is why each thread checks this count only
                                    ;; at some iterations:
                                    (zerop
                                     (mod iteration
                                          check-every))
                                    (< wait-if-queue-larger-than
                                       (actor-queue-size actor)))
                               (sleep (random 0.1)))
                              (t
                               (if with-reply-p
                                 (if async-ask-p
                                   (act:ask actor :foo)
                                   (act:ask-s actor :foo))
                                 (handler-case
                                     (act:tell actor :foo)
                                   (queue-full-error ()
                                     ;; For this test it is ok to just sleep a little
                                     ;; before the next attempt to send message
                                     (sleep (random 0.1))))))))))

            (unwind-protect
                 (progn
                   (let ((threads
                           (loop :for thread-id :from 1 :upto load-threads
                                 :for thread-name := (format nil "thread-~a" thread-id)
                                 :collect (bt2:make-thread #'sender
                                                           :name thread-name))))
                     
                     (unwind-protect (mapc #'bt2:join-thread threads)
                       ;; If user will interrupt execution while we are waiting for threads,
                       ;; we need to clean rest threads:
                       (loop :for thread :in threads
                             :when (bt2:thread-alive-p thread)
                               :do (bt2:destroy-thread thread))))
                    
                   ;; Wait while receiver will process all messages in the queue
                   (miscutils:assert-cond
                    (lambda ()
                      (zerop (total-queues-size system)))
                    60)

                   (trivial-garbage:gc :full t)
                    
                   ;; To make trivial-benchmark collector see our counter.
                   (setf *num-processed-messages*
                         counter))
              (ac:shutdown system))))))))

(defun run-all (&key
                (num-iterations 60)
                (duration 10)
                (queue-size 100)
                (time-out 3)
                &aux (started-at (get-internal-real-time)))
  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p nil
                 :async-ask-p nil)

  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p t
                 :async-ask-p nil)
  
  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p t
                 :async-ask-p t)

  
  (format t "With queue size limited to ~A:~2%"
          queue-size)
  
  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p nil
                 :async-ask-p nil
                 :queue-size queue-size)
  
  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p t
                 :async-ask-p nil
                 :queue-size queue-size)
  
  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p t
                 :async-ask-p t
                 :queue-size queue-size)

  
  (format t "With time-out ~A:~2%"
          time-out)
  
  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p nil
                 :async-ask-p nil
                 ;; This should not make sense for with-reply-p = nil
                 :time-out time-out)

  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p t
                 :async-ask-p nil
                 :time-out time-out)
  
  (run-benchmark :num-iterations num-iterations
                 :duration duration
                 :with-reply-p t
                 :async-ask-p t
                 :time-out time-out)

  (format t "All tests are performed in ~,2f seconds.~%"
          (/ (- (get-internal-real-time) started-at)
             internal-time-units-per-second)))


;; ----------------------------------
;; old runner stuff
;; ----------------------------------

;; (defun runner-lp ()
;;   (setf *msgbox* (make-instance 'sento.messageb::message-box-lsr))
;;   (setf lparallel:*kernel* (lparallel:make-kernel +threads+))
;;   (setf *counter* 0)

;;   (unwind-protect
;;        (time
;;         (let ((chan (lparallel:make-channel)))
;;           (dotimes (n (max-loop))
;;             (lparallel:submit-task chan #'msg-submit))
;;           (dotimes (n (max-loop))
;;             (lparallel:receive-result chan))))
;;     (format t "Counter: ~a~%" *counter*)
;;     (lparallel:end-kernel)
;;     (sento.messageb::stop *msgbox*)))

;; (defun runner-lp2 ()
;;   (setf *msgbox* (make-instance 'sento.messageb::message-box-lsr))
;;   (setf lparallel:*kernel* (lparallel:make-kernel +threads+))
;;   (setf *counter* 0)

;;   (unwind-protect
;;        (time
;;         (progn
;;           (map nil #'lparallel:force
;;                (mapcar (lambda (x)
;;                          (lparallel:future
;;                            (dotimes (n *per-thread*)
;;                              (msg-submit))))
;;                        (mapcar (lambda (n) (format nil "thread-~a" n))
;;                                (loop for n from 1 to +threads+ collect n))))
;;           (format t "Counter: ~a~%" *counter*)
;;           (assert-cond (lambda () (= *counter* (max-loop))) 5)))
;;     (format t "Counter: ~a~%" *counter*)
;;     (lparallel:end-kernel)
;;     (sento.messageb::stop *msgbox*)))

;; (defun runner-lp3 ()
;;   (setf *msgbox* (make-instance 'sento.messageb::message-box-lsr))
;;   (setf lparallel:*kernel* (lparallel:make-kernel +threads+))
;;   (setf *counter* 0)

;;   (unwind-protect
;;        (time
;;         (lparallel:pmap nil (lambda (per-thread)
;;                               (dotimes (n per-thread)
;;                                 (msg-submit)))
;;                         :parts 1
;;                         (loop repeat +threads+ collect *per-thread*)))
;;     (format t "Counter: ~a~%" *counter*)
;;     (lparallel:end-kernel)
;;     (sento.messageb::stop *msgbox*)))

