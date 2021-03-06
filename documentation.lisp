
(in-package :asys)
(pax:defsection @actor-system (:title "Actor-System")
  (asys:actor-system class)
  ;;(asys:dispatchers (pax:reader actor-system))
  (asys:make-actor-system function)
  ;; ac protocol
  (ac:actor-of (pax:method () (asys:actor-system t)))
  (ac:find-actors (pax:method () (asys:actor-system t)))
  (ac:find-actor-by-name (pax:method () (asys:actor-system t)))
  (ac:all-actors (pax:method () (asys:actor-system)))
  (ac:stop (pax:method () (asys:actor-system t)))
  (ac:shutdown (pax:method () (asys:actor-system)))
  )

(in-package :ac)
(pax:defsection @ac-protocol (:title "Actor-Context protocol")
  (ac:actor-of generic-function)
  (ac:find-actors generic-function)
  (ac:find-actor-by-name generic-function)
  (ac:all-actors generic-function)
  (ac:stop generic-function)
  (ac:shutdown generic-function))

(pax:defsection @actor-context (:title "Actor-Context")
  (ac:actor-context class)
  (ac:make-actor-context function)

  (ac:actor-of (pax:method () (ac:actor-context t)))
  (ac:find-actors (pax:method () (ac:actor-context t)))
  (ac:find-actor-by-name (pax:method () (ac:actor-context t)))
  (ac:all-actors (pax:method () (ac:actor-context)))
  (ac:stop (pax:method () (ac:actor-context t)))
  (ac:shutdown (pax:method () (ac:actor-context)))

  (ac:notify generic-function)

  (ac:system (pax:reader actor-context))
  (ac:id (pax:reader actor-context))
  (ac:actor-name-exists condition)

  (@ac-protocol pax:section))

(in-package :act-cell)
(pax:defsection @actor-cell (:title "Actor-Cell")
  (act-cell:actor-cell class)
  (act-cell:name (pax:reader actor-cell))
  (act-cell:state (pax:reader actor-cell))
  (act-cell:msgbox (pax:accessor actor-cell))
  (act-cell:msgbox (pax:accessor actor-cell))
  (act-cell:*sender* variable)
  (act-cell:handle-call generic-function)
  (act-cell:handle-cast generic-function)
  (act-cell:pre-start generic-function)
  (act-cell:after-stop generic-function)
  (act-cell:stop generic-function)
  (act-cell:call function)
  (act-cell:cast function)
  (act-cell:running-p function))

(in-package :act)
(pax:defsection @actor (:title "Actor")
  (act:actor class)
  (act:make-actor generic-function)
  (act:actor-of pax:macro)
  (act:tell generic-function)
  (act:ask-s generic-function)
  (act:ask generic-function)
  (act:become generic-function)
  (act:unbecome generic-function)
  (act:context generic-function)
  (act:path generic-function)
  (act:watch generic-function)
  (act:unwatch generic-function)
  (act:watchers generic-function)

  (act-cell:@actor-cell pax:section))

(in-package :agt)
(pax:defsection @agent (:title "Agent")
  (agt:agent class)
  (agt:make-agent function)
  (agt:agent-get function)
  (agt:agent-update function)
  (agt:agent-stop function))

(in-package :disp)
(pax:defsection @dispatcher (:title "Dispatcher")
  (disp:dispatcher-base class)
  (disp:shared-dispatcher class)
  (disp:make-dispatcher function)
  (disp:dispatch generic-function)
  (disp:dispatch-async generic-function)
  (disp:shutdown generic-function)
  (disp:workers generic-function)
  (disp:dispatch-worker class)
  (disp:make-dispatcher-worker function))

(in-package :router)
(pax:defsection @router (:title "Router")
  (router:router class)
  (router:make-router function)
  (router:add-routee function)
  (router:stop function)
  (router:routees function)
  (router:tell generic-function)
  (router:ask-s generic-function)
  (router:ask generic-function))

(defpackage :cl-gserver.docs)
(in-package :cl-gserver.docs)

(defmacro defsection-extmd (name path
                            (&key
                               title
                               (package '*package*)
                               (readtable '*readtable*)))
  (alexandria:with-gensyms (file-content entries)
    `(progn
       (let* ((,file-content (str:from-file ,path))
              (,entries (list ,file-content)))
         (pax::export-some-symbols ',name ,entries ,package)
         (defparameter ,name
           (make-instance 'pax:section
                          :name ',name
                          :package ,package
                          :readtable ,readtable
                          :title ,title
                          :entries (pax::transform-entries ,entries)))))))

(defsection-extmd @readme #P"README.md" (:title "Introduction"))

(pax:defsection @api (:title "API documentation")
  (asys:@actor-system pax:section)
  (ac:@actor-context pax:section)
  (act:@actor pax:section)
  (agt:@agent pax:section)
  (disp:@dispatcher pax:section)
  (router:@router pax:section))

(pax:defsection @cl-gserver (:title "cl-gserver documentation")
  (@readme pax:section)
  (@api pax:section))
