;; aesthetic-package.lisp -  The package definition(s) for aesthetic.
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


(defpackage aesthetic
  (:documentation
   "Aesthetic generation, mutation and evaluation.")
  (:use #:cl)
  (:export make-properties
           update-properties
           describe-properties
           evaluate-properties
           make-aesthetic
           serialise-aesthetic
           deserialise-aesthetic
           update-aesthetic
           describe-aesthetic
           generate-description
           critique-artwork))
