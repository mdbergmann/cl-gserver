
(defpackage :cl-gserver.docs)
(in-package :cl-gserver.docs)

(pax:defsection @cl-gserver (:title "cl-gserver documentation")
  "The main documentation of cl-gserver"

  (asys:@actor-system pax:section)
  (ac:@actor-context pax:section)
  (act:@actor pax:section)
  (disp:@dispatcher pax:section)
  (router:@router pax:section)
  )

(in-package :asys)
(pax:defsection @actor-system (:title "Actor-System")
  "Description of Actor-System."
  (asys:actor-system class)
  ;;(asys:dispatchers (pax:reader actor-system))
  (asys:make-actor-system function))

(in-package :ac)
(pax:defsection @actor-context (:title "Actor-Context")
  "Description of Actor-Context."
  (ac:actor-context class)
  (ac:make-actor-context function)
  (ac:actor-of generic-function)
  (ac:find-actors generic-function)
  (ac:find-by-name generic-function)
  (ac:all-actors generic-function)
  (ac:stop generic-function)
  (ac:notify generic-function)
  (ac:shutdown generic-function)
  (ac:system generic-function)
  (ac:id generic-function)
  (ac:actor-name-exists condition))

(in-package :act)
(pax:defsection @actor (:title "Actor")
  "This is a description for the actor api."
  (act:actor class)
  (act:make-actor generic-function)
  (actor-of pax:macro)
  (act:tell generic-function)
  (act:ask-s generic-function)
  (act:ask generic-function)
  (act:become generic-function)
  (act:unbecome generic-function)
  (act:context generic-function)
  (act:path generic-function)
  (act:watch generic-function)
  (act:unwatch generic-function)
  (act:watchers generic-function))

(in-package :disp)
(pax:defsection @dispatcher (:title "Dispatcher")
  "This is a description for the dispatcher api."
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
  "This is a description for the router api."
  (router:router class)
  (router:make-router function)
  (router:add-routee function)
  (router:stop function)
  (router:routees function)
  (router:tell generic-function)
  (router:ask-s generic-function)
  (router:ask generic-function))

