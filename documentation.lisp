
(in-package :asys)
(pax:defsection @actor-system (:title "Actor-System")
  (asys:actor-system class)
  (asys:make-actor-system function)
  (asys:*default-config* variable)
  (asys:register-dispatcher function)
  (asys:register-new-dispatcher function)
  (asys:evstream (pax:reader actor-system))
  ;; ac protocol
  (ac:actor-of (pax:method () (asys:actor-system)))
  (ac:find-actors (pax:method () (asys:actor-system t)))
  (ac:all-actors (pax:method () (asys:actor-system)))
  (ac:stop (pax:method () (asys:actor-system t)))
  (ac:shutdown (pax:method () (asys:actor-system)))
  )

(in-package :ac)
(pax:defsection @ac-protocol (:title "Actor-Context protocol")
  (ac:actor-of generic-function)
  (ac:find-actors generic-function)
  (ac:all-actors generic-function)
  (ac:stop generic-function)
  (ac:shutdown generic-function))

(pax:defsection @actor-context (:title "Actor-Context")
  (ac:actor-context class)
  (ac:make-actor-context function)

  (ac:actor-of (pax:method () (ac:actor-context)))
  (ac:find-actors (pax:method () (ac:actor-context t)))
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
  (act-cell:*sender* variable)
  (act-cell:handle-call generic-function)
  (act-cell:handle-cast generic-function)
  (act-cell:pre-start generic-function)
  (act-cell:after-stop generic-function)
  (act-cell:stop generic-function)
  (act-cell:call function)
  (act-cell:cast function)
  (act-cell:running-p function))

(in-package :mesgb)
(pax:defsection @message-box-base (:title "Message-box base class")
  (mesgb::message-box-base class)
  (mesgb::name (pax:reader mesgb::message-box-base))
  (mesgb::max-queue-size (pax:reader mesgb::message-box-base))
  (mesgb:submit generic-function)
  (mesgb:stop generic-function)
  (mesgb:stop (pax:method () (mesgb::message-box-base)))
  (mesgb:with-submit-handler pax:macro))

(pax:defsection @message-box/bt (:title "Message-box threaded")
  (mesgb:message-box/bt class)
  (mesgb:submit (pax:method () (mesgb:message-box/bt t t t t))))

(pax:defsection @message-box/dp (:title "Message-box dispatched")
  (mesgb:message-box/dp class)
  (mesgb:submit (pax:method () (mesgb:message-box/dp t t t t))))

(in-package :future)
(pax:defsection @future (:title "Future (delayed-computation)")
  (future:future class)
  (future:make-future function)
  (future:complete-p function)
  (future:on-completed function)
  (future:get-result function))

(in-package :act)
(pax:defsection @actor (:title "Actor")
  (act:actor class)
  (act:make-actor generic-function)
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
  ;; event stream protocol
  (ev:subscribe (pax:method () (act:actor act:actor)))
  (ev:unsubscribe (pax:method () (act:actor act:actor)))
  (ev:publish (pax:method () (act:actor t)))
  ;; actor-context protocol
  (ac:find-actors (pax:method () (act:actor t)))
  (ac:all-actors (pax:method () (act:actor)))
  (ac:actor-of (pax:method () (act:actor)))
  
  (act-cell:@actor-cell pax:section)
  (mesgb:@message-box-base pax:section)
  (mesgb:@message-box/bt pax:section)
  (mesgb:@message-box/dp pax:section)
  (future:@future pax:section))

(in-package :agthash)
(pax:defsection @hash-agent (:title "Hash-table agent")
  (agthash:make-hash-agent function)
  (agthash:agent-gethash function)
  (agthash:agent-remhash function)
  (agthash:agent-clrhash function)
  (agthash:agent-dohash function))

(in-package :agtarray)
(pax:defsection @array-agent (:title "Array/Vector agent")
  (agtarray:make-array-agent function)
  (agtarray:agent-elt function)
  (agtarray:agent-push function)
  (agtarray:agent-push-and-getidx function)
  (agtarray:agent-pop function)
  (agtarray:agent-delete function)
  (agtarray:agent-doarray function))

(in-package :agt)
(pax:defsection @agent (:title "Agent")
  (agt:agent class)
  (agt:make-agent function)
  (agt:agent-get function)
  (agt:agent-update function)
  (agt:agent-update-and-get function)
  (agt:agent-stop function)

  (agthash:@hash-agent pax:section)
  (agtarray:@array-agent pax:section))

(in-package :disp)
(pax:defsection @shared-dispatcher (:title "Shared dispatcher")
  (disp:shared-dispatcher class))

(pax:defsection @dispatcher (:title "Dispatcher")
  (disp:dispatcher-base class)
  (disp:identifier (pax:reader disp:dispatcher-base))
  (disp:make-dispatcher function)
  (disp:dispatch generic-function)
  (disp:dispatch-async generic-function)
  (disp:stop generic-function)
  (disp:workers generic-function)
  (disp:dispatch-worker class)
  (disp:make-dispatcher-worker function)

  (disp:@shared-dispatcher pax:section))

(in-package :router)
(pax:defsection @router (:title "Router")
  (router:router class)
  (router:make-router function)
  (router:add-routee function)
  (router:stop function)
  (router:routees function)
  (router:tell (pax:method () (router:router t)))
  (router:ask-s (pax:method () (router:router t)))
  (router:ask (pax:method () (router:router t))))

(in-package :ev)
(pax:defsection @eventstream (:title "Eventstream")
  (ev:eventstream class)
  (ev:make-eventstream function)

  (ev:subscribe generic-function)
  (ev:unsubscribe generic-function)
  (ev:publish generic-function)

  (ev:subscribe (pax:method () (ev:eventstream act:actor)))
  (ev:subscribe (pax:method () (asys:actor-system act:actor)))
  (ev:subscribe (pax:method () (act:actor act:actor)))
  (ev:unsubscribe (pax:method () (ev:eventstream act:actor)))
  (ev:unsubscribe (pax:method () (asys:actor-system act:actor)))
  (ev:unsubscribe (pax:method () (act:actor act:actor)))
  (ev:publish (pax:method () (ev:eventstream t)))
  (ev:publish (pax:method () (asys:actor-system t)))
  (ev:publish (pax:method () (act:actor t)))
  )

(in-package :tasks)
(pax:defsection @tasks (:title "Tasks")
  (tasks:with-context pax:macro)
  (tasks:*task-context* variable)
  (tasks:*task-dispatcher* variable)
  (tasks:task-yield function)
  (tasks:task-start function)
  (tasks:task-async function)
  (tasks:task-await function)
  (tasks:task-shutdown function)
  (tasks:task-async-stream function))

(in-package :config)
(pax:defsection @config (:title "Config")
  (config:config-from function)
  (config:retrieve-section function)
  (config:retrieve-value function)
  (config:retrieve-keys function)
  (config:merge-config function))

(defpackage :sento.docs)
(in-package :sento.docs)

(pax:defsection @readme (:title "Introduction")
  (README.md (pax:include #.(asdf:system-relative-pathname :sento "README.md"))))

(pax:defsection @api (:title "API documentation")
  (asys:@actor-system pax:section)
  (ac:@actor-context pax:section)
  (act:@actor pax:section)
  (agt:@agent pax:section)
  (disp:@dispatcher pax:section)
  (router:@router pax:section)
  (ev:@eventstream pax:section)
  (tasks:@tasks pax:section)
  (config:@config pax:section))

(pax:defsection @sento (:title "sento documentation")
  (@readme pax:section)
  (@api pax:section))
