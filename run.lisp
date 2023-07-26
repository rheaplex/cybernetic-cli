(load "aesthetic.asd")
(load "cyberartist.asd")
(load "cybercritic.asd")
(load "cybercollector.asd")
(load "cybernetic.asd")

(asdf:load-system 'cybernetic)

(defun main ()
  (setf *random-state* (make-random-state t))
  (let ((artworld (cybernetic:make-cybernetic)))
    (cybernetic:run-cybernetic-until-collector-collects artworld)))

(main)
