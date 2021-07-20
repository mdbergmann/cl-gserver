(defpackage :cl-gserver.agent.array
  (:use :cl)
  (:nicknames :agtarray)
  (:export #:make-array-agent
           #:agent-elt
           #:agent-push
           #:agent-pop)
  )

(in-package :cl-gserver.agent.array)


(defun make-array-agent (context &key
                                   initial-array
                                   (dispatcher-id :shared))
  "Creates an agent that wraps a CL array/vector.  
`context`: something implementing `ac:actor-context` protocol like `asys:actor-system`. Specifying `nil` here creates an agent outside of an actor system. The user has to take care of that himself.  
`initial-array`: specify an initial array/vector.  
`dispatcher-id`: a dispatcher. defaults to `:shared`."
  (check-type initial-array array)
  (agt:make-agent (lambda () initial-array)
                  context dispatcher-id))

(defun agent-elt (index array-agent)
  "Retrieves the value of the specified index of the array.  
`index`: the index to retrieve.  
`array-agent`: the array agent instance."
  (agt:agent-get array-agent
                 (lambda (array)
                   (elt array index))))

(defun agent-push (item array-agent)
  "Pushes a value to the array/vector. Internally uses `vector-push-extend`, so the array must have a `fill-pointer`.  
`item`: item to push.  
`array-agent`: the array agent instance."
  (agt:agent-update array-agent
                    (lambda (array)
                      (vector-push-extend item array)
                      array)))

(defun agent-pop (array-agent)
  "Pops from array and returns the popped value. Internally uses `vector-pop`, so the array must have a `fill-pointer`.  
`array-agent`: the array agent instance."
  (agt:agent-get array-agent
                 (lambda (array)
                   (vector-pop array))))
