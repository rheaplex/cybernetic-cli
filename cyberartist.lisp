;; cyberartist.lisp -  The main generate-and-blog code.
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

(in-package :cyberartist)

(defclass <cyberartist> ()
  ((aesthetic :accessor aesthetic
              :initarg :aesthetic
              :documentation "The artist's aesthetic.")))

(defun make-cyberartist ()
  (make-instance '<cyberartist> :aesthetic (make-aesthetic)))

(defmethod create-art ((o <cyberartist>))
  (generate-description))
