# Getting started

```
ros run.lisp
```

or

```
(load "aesthetic.asd")
(load "cyberartist.asd")
(load "cybercritic.asd")
(load "cybercollector.asd")
(load "cybernetic.asd")

(asdf:load-system 'cybernetic)

(defvar artworld (cybernetic:make-cybernetic))

(cybernetic:run-cybernetic artworld)

(cybernetic:run-cybernetic-until-collector-collects artworld)

```
