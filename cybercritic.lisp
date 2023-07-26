;; cybercritic.lisp -  The main generate-and-blog code.
;; Copyright (C) 2009 Rhea Myers rhea@myers.studio
;; Copyright (C) 2023 Myers Studio, Ltd.
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

(in-package :cybercritic)

(defclass <cybercritic> ()
  ((aesthetic :initarg :aesthetic
              :accessor aesthetic
              :documentation "The critic's aesthetic.")))

(defun make-cybercritic ()
  (make-instance '<cybercritic> :aesthetic (make-aesthetic)))

(defmethod print-aesthetic ((critic <cybercritic>))
  "Post a description of the critic's aesthetic"
  (multiple-value-bind (good bad)
      (aesthetic:describe-aesthetic (aesthetic critic))
    (format t "    ~a~%    ~a~%" good bad)))

(defmethod update ((critic <cybercritic>))
  "Update the aesthetic and print it."
  (aesthetic:update-aesthetic (aesthetic critic)))

(defmethod critique-art ((critic <cybercritic>) (art string))
  "Critique an artwork."
  (critique-artwork art (aesthetic critic) art))

(defmethod critique-art ((critic <cybercritic>) (art array))
  "Critique a vector of artworks."
  (map 'vector
       #'(lambda (artwork) (critique-artwork artwork
                                             (aesthetic critic)
                                             artwork))
       art))
