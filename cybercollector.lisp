;; cybercollector.lisp -  The main collect-and-blog code.
;; Copyright (C) 2009  Rhea Myers rhea@myers.studio
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cybercollector)

(defclass <cybercollector> ()
  ())

(defun make-cybercollector ()
  (make-instance '<cybercollector>))

(defun masterpiece-p (description)
  "Is the subject of the description a masterpiece?"
  (search "masterpiece" description))

(defmethod collect-art ((collector <cybercollector>)
                        (artworks sequence)
                        (critiques sequence))
  (declare (ignore collector))
  (remove nil
          (map 'vector
               #'(lambda (artwork critique)
                   (when (masterpiece-p critique)
                     (format nil "I just bought \"~a\"!" artwork)))
               artworks
               critiques)))
