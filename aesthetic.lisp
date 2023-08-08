;; aesthetic.lisp -  Generate, mutate, describe and evaluate aesthetics.
;; Copyright (C) 2009, 2023 Rhea Myers rhea@myers.studio
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

(in-package :aesthetic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe (fun &key (probability 0.5) (default nil))
  "Call fun with aqrgs if random(0..1) is less than probability."
  (if (< (random 1.0) probability)
      (funcall fun)
    default))

(defun choose-randomly (choices)
  "Choose one of the parameters randomly."
  (nth (random (list-length choices)) 
       choices))

(defun plus-or-minus-one ()
  "Choose either one or minus one."
  (choose-randomly '(1.0 -1.0)))

(defun random-range (from to)
  "Choose a random number from from to to."
  (+ (random (- to from))
     from))

(defun choose-one-of (possibilities)
  "Choose one or none of the option"
  (nth (random (length possibilities)) possibilities))

(defun choose-n-of (n choice-list)
  "Choose n different entries from choice-list."
  (assert (<= n (length choice-list)))
  (let ((choices choice-list)
        (chosen '()))
    (dotimes (i n)
      (let ((choice (choose-one-of choices)))
        (setf chosen (cons choice chosen))
        (setf choices (remove choice choices))))
    chosen))

(defun concatenate-string (&rest strings)
  "Concatenate a list of strings with an optional given prefix, separator and suffix."
  (let ((all (car strings)))
    (dolist (s (cdr strings))
      (when (not (equal s ""))
        (setf all (concatenate 'string all
                               (if (equal all "")
                                   ""
                                   " ") 
                               s))))
    all))

(defun pluralise (object plurality)
  "Make a word plural if necessary."
  (if (equal plurality "A")
      object
    (concatenate 'string object "s")))

(defun make-stretchy-vector (element-type)
  "Make an empty stretchy vector of element-type."
  (make-array 0
          :element-type element-type
          :fill-pointer 0
          :adjustable t))

(defun vector-empty-p (vec)
  "Is the vector empty?"
  (= (length vec) 0))

(defun make-char-stretchy-vector ()
  "Make an empty stretchy character vector."
  (make-stretchy-vector 'character))

(defun make-string-stretchy-vector ()
  "Make an empty stretchy string vector."
  (make-stretchy-vector 'string))

(defun tokenize-string (source separators)
  "Tokenize string to produce words separated by runs of separators."
  (let ((words (make-string-stretchy-vector))
    (chars (make-char-stretchy-vector)))
    (loop for char across source  
      do (if (find char separators :test #'char-equal)
         (when (not (vector-empty-p chars))
           (vector-push-extend chars words)
           (setf chars (make-char-stretchy-vector)))
           (vector-push-extend char chars)))
    ;; If a word is at the end of the string without a terminator, add it
    (when (not (vector-empty-p chars))
      (vector-push-extend chars words))
    words))

(defun list-to-items (l v)
  "Make an extended alist of l consed with the default value v and nil."
  (map 'list
       #'(lambda (item) (list item v nil))
       l))

(defun positive-items (items)
  "Fetch a list of the entries in the list with values > 0."
  (loop for item in items
        when (> (weight item) 0)
          collect item))

(defun items-total-value (items)
  "Sum the values of the entries in an alist."
  (loop for items in items
        sum (weight item)))

(defun choose-positive-item (items)
  "Fetch the items with positive values, sum those values to represent
   the probability of each being chosen, then choose one."
  (let* ((candidates (positive-items items))
         (total (items-total-value candidates))
         (i (random total)))
    (loop for item in items
          sum (weight item) into prob-so-far
          when (>= prob-so-far i)
            return item)))

(defun choose-positive-item-deep (items)
  "Fetch the items with positive values, sum those values to represent
   the probability of each being chosen, then choose one."
  (let* ((candidates (positive-items items))
         (total (items-total-value candidates))
         (i (random total)))
    (loop for item in items
          sum (weight item) into prob-so-far
          when (>= prob-so-far i)
            do (if (not (consp item))
                   (name item)
                   (choose-positive-item-deep (children item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use a flat three-layer structure.

(defparameter +properties+
  '((amounts
     (singular ("A"))
     (plural ("A pair of" "Some" "Many")))
    (colours
     (monochromes ("black" "grey" "white"))
     (hues ("red" "orange" "yellow" "green" "blue" "purple"))
     (colours ("magenta" "cyan" "brown" "pink" "turquoise" "mauve"))
     (metals ("gold" "silver" "bronze" "platinum" "copper" 
              "rust-coloured"))
     (fabrics ("khaki" "cotton-coloured" "denim blue" 
               "suede-coloured"))
     (naturals ("sky blue" "leaf green" "sea green" "sunset red"))
     (artificials ("neon blue" "sunset yellow" "shocking pink" 
                   "non-repro blue" "blue-screen blue")))
    (tones
     (brightness ("pale" "rich"))
     (saturation ("bright" "dark")))
    (textures
     (rough ("halftoned" "crosshatched" "scumbled" "sketchy"))
     (smooth ("glazed" "smooth")))
    (shapes
     (geometric ("circle" "triangle" "square" "pentagon" "hexagon" "octagon"))
     (abstract ("organic shape" "spiky shape" "irregular shape"))
     (figurative ("house" "skyscraper"
                  "car" "aeroplane" "ship"
                  "bird" "cat" "dog" "horse")))
    (sizes
     (small ("tiny" "small"))
     (medium (""))
     (large ("large" "massive")))
    (treatments
     (abstracting ("silhouetted" "outlined" "abstracted"))
     (realistic ("realistic" "photorealistic" "naturalistic")))))

(defparameter +min-properties+ 4)
(defparameter +max-properties+ 12)
(defparameter +max-properties-to-delete+ 2)
(defparameter +max-properties-to-mutate+ 2)
(defparameter +max-properties-to-add+ 2)

(defun make-aesthetic (aesthetic default)
  "Turn the three-level aesthetic spec into a weighted tree of properties."
  (loop for category in aesthetic
        collect (list (car category)
                      (loop for subcat in (cdr category)
                            collect (list (car subcat)
                                          default
                                          (loop for property in (cadr subcat)
                                                collect (list property
                                                              default
                                                              nil)))))))

(defun name (item)
  "Get the name of the triple."
  (car item))

(defun weight (item)
  "Get the weight of the triple."
  (cadr item))

(defun children (item)
  "Get the child list, if any, of the triple."
  (caddr item))

(defun child-weight-total (item)
  "Sum the weights of the item's children."
  (reduce #'+ (children item) (lambda (child) (weight child))))

(defun child-weight-positive (item)
  "Sum the positive weights of the item's children."
  (reduce #'+ 
          (map 'list
               #'(lambda (child) (let ((w (weight child))) (if (> w 0) w 0)))
               children)))

(defun ensure-positive-child-paths (item)
  "Make sure that there is a path to a non-zero/negative item from each root."
  (let ((items (children item)))
    ;; nil is a list, so consp rather than listp.
    (when (consp items)
      ;; Make sure this level has one or more positive property.
      (loop while (= (child-weight-positive items) 0)
            do (mutate-aesthetic-properties items))
      ;; Recurse to check that positive properties have a positive child item.
      (dolist (item children)
        (when (> (weight item) 0)
          (ensure-positive-child-paths item))))))

(defun list-aesthetic (item)
  "Get all the leaves as a flat list, ignoring parent weights."
  (cond ((null item) nil)
        ((consp (children item)))
        (t (mapcan #'flatten item))))

(defun aesthetic-opinions (aesthetic)
  "Sort the properties into likes and dislikes."
  (let ((likes '())
        (dislikes '()))
    (dolist #'(lambda (key val)
                 (if (>= val 0)
                     (push key likes)
                     (push key dislikes))) 
             aesthetic)
    (values likes dislikes)))

(defun describe-aesthetic (aesthetic)
  "Describe the current likes and dislikes."
  ;;FIXME - Replace the final comma with an and or ampersand.
  (multiple-value-bind (likes dislikes) (aesthetic-opinions aesthetic)
    (let ((likes-string (when likes 
              (format nil "I like~{ ~A~^,~}. " likes)))
      (dislikes-string (when dislikes
                         (format nil "I dislike~{ ~A~^,~}." dislikes))))
      (values likes-string dislikes-string))))

;; (defun aesthetic-size (aesthetic)
;;   "Get the current size of *aesthetic*."
;;   (hash-table-count aesthetic))

;; (defun make-property ()
;;   "Choose a new property."
;;   (funcall (choose-one-of (list #'shape #'colour #'texture))))

;; (defun make-properties (count)
;;   "Choose n properties."
;;   (loop for i below count
;;        collect (make-property)))

;; (defun add-properties (properties)
;;   "Add zero or more properties."
;;   (append properties 
;;       (make-properties (min (max +min-properties+ 
;;                      (random +max-properties-to-add+))
;;                 (- +max-properties+ (length properties))))))

;; (defun delete-properties (properties)
;;   "Delete 0+ properties, don't reduce properties below +min-properties+."
;;   (subseq properties 0 (max (- (length properties)
;;                    (random +max-properties-to-mutate+))
;;                 +min-properties+)))  

;; (defun update-properties (properties)
;;   "Add some properties, delete some properties"
;;   (add-properties (delete-properties properties)))

;; (defun describe-properties (properties)
;;   "List the properties in a comma-delimited string"
;;   (format nil "~{ ~A~^,~}" properties))

;; (defun evaluate-properties (properties1 properties2)
;;   "Find how many properties match"
;;   (length (intersection properties1 properties2)))

;; (defun set-aesthetic-property (aesthetic prop)
;;   "Set valenced property."
;;     (setf (gethash prop aesthetic)
;;       (plus-or-minus-one)))

;; (defun set-aesthetic-properties (aesthetic props)
;;   "Set valenced properties."
;;   (dolist (prop props)
;;     (set-aesthetic-property aesthetic prop))
;;   aesthetic)

;; (defun make-aesthetic ()
;;   "Generate an initial set of properties."
;;   (set-aesthetic-properties (make-hash-table :test 'equal)
;;           (make-properties (random-range +min-properties+
;;                          +max-properties+))))

;; (defun delete-aesthetic-properties (aesthetic)
;;   "Delete 0+ properties, don't reduce properties below +min-properties+."
;;   ;;FIXME - Set the correct number here rather than checking with when
;;   (dolist (prop (choose-n-of (random +max-properties-to-mutate+)
;;                  (list-aesthetic aesthetic)))
;;     (when (> (aesthetic-size aesthetic) +min-properties+) 
;;            (remhash prop aesthetic)))
;;   aesthetic)

;; (defun add-aesthetic-properties (aesthetic)
;;   "Add zero or more properties."
;;   (loop with remaining = (min (max +min-properties+ 
;;                                    (random +max-properties-to-add+))
;;                               (- +max-properties+ (aesthetic-size aesthetic)))
;;     while (> remaining 0)
;;     do (let ((prop (make-property)))
;;          (when (not (gethash prop aesthetic))
;;            (set-aesthetic-property aesthetic prop)
;;            (decf remaining))))
;;   aesthetic)

;; (defun mutate-aesthetic-properties (aesthetic)
;;   "Mutate zero or more properties."
;;   (dolist (prop (choose-n-of (random +max-properties-to-mutate+)
;;                  (list-aesthetic aesthetic)))
;;     (setf (gethash prop aesthetic)
;;       (- (gethash prop aesthetic))))
;;   aesthetic)

(defun update-aesthetic (aesthetic)
  "Update the aesthetic."
  (add-aesthetic-properties 
   (mutate-aesthetic-properties
    (delete-aesthetic-properties aesthetic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quantity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun amount (aesthetic)
  "Generate a quantity description."
  (choose-positive-item 'amounts aesthetic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun colour (aesthetic)
  "Choose a flat colour from a palette"
  (choose-positive-item-deep (assoc 'colours aesthetic)))

(defun colour-description (aesthetic)
  "Generate a colour description."
  (concatenate-string (choose-positive-item (assoc 'tones aesthetic))
                      (colour aesthetic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Texture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun texture (aesthetic)
  "Choose a texture."
  (choose-positive-item (assoc 'textures aesthetic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun appearance (aesthetic)
  "Generate the appearance of a figure."
  (concatenate-string (texture aesthetic)
                      (colour-description aesthetic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun size (aesthetic)
  (choose-positive-item (assoc 'sizes aesthetic)))

(defun shape (aesthetic)
  (choose-positive-item-deep (assoc 'shapes aesthetic)))

(defun shape-description (aesthetic plural)
  "Generate a shape description."
  (concatenate-string (size aesthetic)
                      (appearance aesthetic)
                      (pluralize (shape aesthetic) plural)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ground (aesthetic)
  "Generate a simple ground description."
  (appearance aesthetic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Descriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-description (aesthetic)
  "Describe a single (set of) figure(s) on a single ground."
  (let ((plural (amount aesthetic)))
    (concatenate-string plural (shape-description aesthetic plural)
                        "on a" (ground aesthetic) "ground")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Critique
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; How to handle ambiguities? yellow vs sunset yellow?
;; Sort keys by length
;; Find longest first
;; Remove from string
;; But what if that creates bogus sequences? Break into substrings?
;; Just always hypenate multi-word sequences?

#|(defun score-artwork (artwork aesthetic)
  "Process the string description of the artwork to generate a score."
  (let ((clean-artwork (string-downcase artwork)))
    (loop for prop being each hash-key of *aesthetic*
       if (search prop clean-artwork)
       sum (gethash prop *aesthetic* 0))))

(defun evaluate-artwork (artwork)
  "Set range to max of +/-1, probably less."
  (/ (score-artwork artwork)
     (aesthetic-size)))

(defun describe-artwork-evaluation (score)
  "Turn the -1.0..1.0 score into a verbal description."
  (cond
    ((< score -0.6) "terrible")
    ((< score -0.1) "bad")
    ((< score 0.1) "ok")
    ((< score 0.6) "good")
    (t "excellent")))
|#

(defun positive-aesthetic-value (description aesthetic)
  "Sum the positive value of the work under the aesthetic"
  (let ((clean-artwork (string-downcase description))
    (positive-value 0))
    (maphash #'(lambda (key val) 
         (when (and (search key clean-artwork)
                (> val 0))
           (incf positive-value val)))
         aesthetic)
    positive-value))

(defun negative-aesthetic-value (description aesthetic)
  "Sum the negative value of the work under the aesthetic"
  (let ((clean-artwork (string-downcase description))
        (negative-value 0))
    (maphash #'(lambda (key val) 
         (when (and (search key clean-artwork)
                (< val 0))
           (decf negative-value val)))
         aesthetic)
    negative-value))

(defun total-aesthetic-values (description aesthetic)
  "Sum the values of the work under the aesthetic"
  (let ((clean-artwork (string-downcase description))
    (+total 0)
    (-total 0))
    (maphash #'(lambda (key val) 
         (when (search key clean-artwork)
           (cond 
             ((> val 0)
              (incf +total val))
             ((< val 0)
              (decf -total val)))))
         aesthetic)
    (values +total -total)))

(defun describe-aesthetic-value (positive negative)
  "Describe the value of the work allowing for pos & neg points."
  (cond 
    ((and (>= positive 3) (= negative 0)) "a masterpiece")
    ((and (>= negative 3) (= positive 0)) "a failure")
    ((> positive negative) "good")
    ((> negative positive) "bad")
    ((and (= negative positive) (>= negative 2)) "extremely mixed")
    ((= negative positive 1) "mixed")
    (t "uninteresting")))

(defun critique-artwork (description aesthetic identifier)
  "Verbally critique the artwork."
  (multiple-value-bind (+val -val) 
            (total-aesthetic-values description aesthetic)
    (format nil 
            "~a \"~a\" is ~a."
            (choose-one-of '("I think that" "In my opinion," "I would say that"
                             "Aesthetically speaking," ))
            identifier
            (describe-aesthetic-value +val -val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Similarity, strength of resemblance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun euclidean-aesthetic-similarity (aesthetic-list aesthetic-hash)
  "Get the normalized euclidean distance between two aesthetics"
  (let ((total 0.0)) 
    (dolist (key aesthetic-list) 
      (let ((aesthetic-weight (gethash key aesthetic-hash nil)))
    (when aesthetic-weight
      ;; Assume the presence of a property to be the origin (0.0),
      ;;  and so the distance is the aesthetic's value for that property
      ;;  (-1.0 or +1.0)
      (incf total (expt aesthetic-weight 2)))))
    (/ 1.0 (+ 1.0 (sqrt total)))))
