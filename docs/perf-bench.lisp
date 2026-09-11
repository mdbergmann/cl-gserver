;;; Throughput benchmark behind the numbers in the README "Benchmarks" section.
;;;
;;; Load sento, then this file, then call (sento.perf-bench:run).
;;; It only needs sento and bordeaux-threads, so it runs on every
;;; implementation sento runs on.
;;;
;;; Each cell has `*senders*' threads send `*messages-per-sender*' messages
;;; (1M altogether by default) to one actor whose receive function only counts.
;;; The time is taken until the actor has processed all of them. The cells
;;; are `tell', `ask-s' and `ask' against an actor on the `:pinned' and on the
;;; `:shared' dispatcher. Every cell is run `*rounds*' times; the reported
;;; figure is the median.

(defpackage :sento.perf-bench
  (:use :cl)
  (:export #:run
           #:run-cell
           #:*senders*
           #:*messages-per-sender*
           #:*workers*
           #:*rounds*))
(in-package :sento.perf-bench)

(defparameter *senders* 8
  "Number of threads that send messages concurrently.")
(defparameter *messages-per-sender* 125000
  "Messages each sender thread sends per cell.")
(defparameter *workers* 8
  "Workers of the shared dispatcher.")
(defparameter *rounds* 3
  "Rounds per cell; the median is reported.")

(defun run-cell (dispatcher mode)
  "Sends `*senders*' x `*messages-per-sender*' messages with MODE (:tell, :ask-s
or :ask) to one actor on DISPATCHER (:pinned or :shared).
Returns messages per second and the elapsed seconds."
  (let* ((total (* *senders* *messages-per-sender*))
         (counter 0)
         (system (asys:make-actor-system
                  `(:dispatchers (:shared (:workers ,*workers*)))))
         (actor (ac:actor-of system
                             :receive (lambda (msg)
                                        (declare (ignore msg))
                                        (incf counter))
                             :dispatcher dispatcher)))
    (unwind-protect
         (let* ((send (ecase mode
                        (:tell (lambda () (act:tell actor :foo)))
                        (:ask-s (lambda () (act:ask-s actor :foo)))
                        (:ask (lambda () (act:ask actor :foo)))))
                (start (get-internal-real-time))
                (threads (loop :repeat *senders*
                               :collect (bt2:make-thread
                                         (lambda ()
                                           (dotimes (i *messages-per-sender*)
                                             (funcall send)))
                                         :name "perf-sender"))))
           (mapc #'bt2:join-thread threads)
           (loop :with deadline = (+ (get-internal-real-time)
                                     (* 600 internal-time-units-per-second))
                 :until (>= counter total)
                 :do (when (> (get-internal-real-time) deadline)
                       (error "Actor processed only ~a of ~a messages after 600s"
                              counter total))
                     (sleep 0.001))
           (let ((elapsed (/ (- (get-internal-real-time) start)
                             internal-time-units-per-second)))
             (values (round total (max elapsed 1/1000))
                     (float elapsed))))
      (ac:shutdown system))))

(defun median (numbers)
  (let ((sorted (sort (copy-list numbers) #'<))
        (n (length numbers)))
    (if (oddp n)
        (nth (floor n 2) sorted)
        (round (+ (nth (1- (floor n 2)) sorted) (nth (floor n 2) sorted)) 2))))

(defun run ()
  "Runs all six cells `*rounds*' times each and prints one RESULT line per cell."
  (format t "~&~a ~a~%" (lisp-implementation-type) (lisp-implementation-version))
  (format t "~a x ~a messages per cell, ~a shared workers, ~a rounds~%"
          *senders* *messages-per-sender* *workers* *rounds*)
  (force-output)
  (dolist (dispatcher '(:pinned :shared))
    (dolist (mode '(:tell :ask-s :ask))
      (let ((rates (loop :repeat *rounds*
                         :collect (multiple-value-bind (rate elapsed)
                                      (run-cell dispatcher mode)
                                    (format t "~&  ~a ~a: ~,2fs, ~:d msg/s~%"
                                            dispatcher mode elapsed rate)
                                    (force-output)
                                    rate))))
        (format t "~&RESULT ~a ~a ~:d msg/s (median of ~{~:d~^, ~})~%"
                dispatcher mode (median rates) rates)
        (force-output)))))
