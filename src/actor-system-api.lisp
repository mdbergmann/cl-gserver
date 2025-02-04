(defpackage :sento.actor-system
  (:use :cl)
  (:nicknames :asys)
  (:export #:make-actor-system
           #:actor-system
           #:dispatchers
           #:evstream
           #:scheduler
           #:config
           #:register-dispatcher
           #:register-new-dispatcher
           #:*default-config*))

(in-package :sento.actor-system)

(defparameter *default-config*
  '(:dispatchers
    (:shared (:workers 4 :strategy :random :mbox-type mesgb:message-box/dp))
    :timeout-timer
    (:resolution 100 :max-size 500)
    :eventstream
    (:dispatcher-id :shared)
    :scheduler
    (:enabled :true :resolution 100 :max-size 500))
  "The default config used when creating an `asys:actor-system`.
The actor-system constructor allows to provide custom config options that override the defaults.

The constructor provided configuration does not need to specify all parts of the config,
rather, what is provided is merged with `*default-config*`.  
That means e.g. specifying an additional dispatcher, one just has to provide this to the constructor:

```
'(:dispatchers
    (:my-disp (:workers 4)))
```

For all other parameters the defaults will be used, even `:workers` does not need to be there.
The defaults, when omitted, are:  
- workers = 2
- strategy = :random
- mbox-type = 'mesgb:message-box/dp'

If you want to just modify parts of the config, i.e. the strategy, then one can do:

```
'(:dispatchers
    (:shared (:strategy :round-robin)))
```

This will just change the strategy to `:round-robin`.

Note that `mbox-type` must be a subtype of `mesgb:message-box/dp`.
")
