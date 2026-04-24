;; The Cybernetic Artworld - Simulate how the artworld doesn't work.
;; Copyright (C) 2009 Rhea Myers <rhea@myers.studio>
;; Copyright (C) 2023, 2026 Myers Studio Ltd. & Rhea Myers
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO:
;; 1. Improve term matching as per comments.
;; 2. Reduce duplication of values. Hash vs. list?
;; 3. Decide on complete aesthetic vs. specific terms.
;; 4. Fix artist and critic weights and weight updates.
;; 5. Weights for subcategories and categories, and their effects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp environment setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn ;;init forms
  ;;(ros:ensure-asdf)
  ;;#+quicklisp(ql:quickload '() :silent t)
  )

(defpackage :cybernetic-artworld
  (:use :cl)
  (:export #:main))
(in-package :cybernetic-artworld)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base configuration for aesthetic properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +min-properties+ 4)
(defparameter +max-properties+ 12)
(defparameter +max-properties-to-delete+ 2)
(defparameter +max-properties-to-mutate+ 2)
(defparameter +max-properties-to-add+ 2)

;; Use a flat three-layer structure.

(defparameter +properties+
  '((:amounts
     (:singular "A")
     (:plural "A pair of" "Some" "Many"))
    (:colours
     (:monochromes "black" "grey" "white")
     (:hues "red" "orange" "yellow" "green" "blue" "purple")
     (:colours "magenta" "cyan" "brown" "pink" "turquoise" "mauve")
     (:metals "gold" "silver" "bronze" "platinum" "copper"
      "rust-coloured")
     (:fabrics "khaki" "cotton-coloured" "denim blue"
      "suede-coloured")
     (:naturals "sky blue" "leaf green" "sea green" "sunset red")
     (:artificials "neon blue" "sunset yellow" "shocking pink"
      "non-repro blue" "blue-screen blue"))
    (:tones
     (:brightness "pale" "rich")
     (:saturation "bright" "dark"))
    (:textures
     (:rough "halftoned" "crosshatched" "scumbled" "sketchy")
     (:smooth "glazed" "smooth"))
    (:shapes
     (:geometric "circle" "triangle" "square" "pentagon" "hexagon"
      "octagon")
     (:abstract "organic shape" "spiky shape" "irregular shape")
     (:figurative "house" "skyscraper"
      "car" "aeroplane" "ship"
      "bird" "cat" "dog" "horse"))
    (:sizes
     (:small "tiny" "small")
     (:medium "medium" "middling")
     (:large "large" "massive"))
    (:treatments
     (:abstracting "silhouetted" "outlined" "abstracted")
     (:realistic "realistic" "photorealistic" "naturalistic"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random choice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun choose-randomly (choices)
  "Choose one of the parameters randomly."
  (nth (random (list-length choices))
       choices))

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

(defun plus-or-minus-one ()
  "Choose either one or minus one."
  (choose-randomly '(1.0 -1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-string (&rest strings)
  "Concatenate a list of strings with an optional given prefix,
   separator and suffix."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <property>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <property> ()
  ((subcategory :initarg :subcategory
                :accessor subcategory)
   (category :initarg :category
             :accessor category)
   (name :initarg :name
         :accessor name)
   (weight :initarg :weight
           :initform 0.0
           :accessor weight))
  (:documentation "An aesthetic property."))

(defmethod print-object ((object <property>) stream)
  "Print the property to the provided stream."
  (format stream "#<<property> ~a: ~a>" (name object) (weight object)))

(defun random-property-values (weight)
  "Choose random values for name/subcategory/category
   and return with weight"
  (let* ((category (choose-randomly +properties+))
         (subcategory (choose-randomly (cdr category)))
         (property (choose-randomly (cdr subcategory))))
    (values property weight (car category)(car subcategory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <properties>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <properties> ()
  ((properties :initform (make-hash-table)
               :accessor properties)
   (properties-list :initform nil
                    :accessor properties-list))
  (:documentation "A collection of aesthetic properties."))

(defmethod property-names ((p <properties>))
  "Get a flat list of the property names from the property list."
  (loop for key being the hash-keys of (properties p)
        collecting key))

(defmethod properties-list-pos ((pl <properties>))
  "Get only the positive properties from the list."
  (remove-if-not #'(lambda (p) (> (weight p) 0.0)) (properties-list pl)))

(defmethod properties-list-neg ((pl <properties>))
  "Get only the negative properties from the list."
  (remove-if-not #'(lambda (p) (< (weight p) 0.0)) (properties-list pl)))

(defmethod properties-total-value (properties)
  "Sum the values of the properties."
  (loop for prop in properties
        sum (weight prop)))

(defmethod choose-property-pos ((properties <properties>))
  "Chose a property from the property list with a positive weight."
  (let* ((candidates (properties-list-pos properties))
         (total (properties-total-value candidates))
         (i (random total)))
    (loop for prop in candidates
          sum (weight prop) into prob-so-far
          when (>= prob-so-far i)
            return prop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <subcategory>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <subcategory> (<properties>)
  ((category :initarg :category
             :accessor category)
   (name :initarg :name
         :accessor name))
  (:documentation "A groups of related aesthetic properties
                   within a broader category."))

(defun print-subcategory (sub stream)
  "Print the subcategory to the provided stream."
  (format stream "  ~a:" (name sub))
  (loop for key being the hash-keys of (properties sub)
          using (hash-value value)
        do (format stream " ~a: ~a" (name value) (weight value)))
  (format stream "~%"))

(defmethod print-object ((object <subcategory>) stream)
  "Print the subcategory to the provided stream."
  (print-subcategory object stream))

(defmethod weight ((sub <subcategory>))
  "The total weight of the properties in the subcategory."
  (loop for key being the hash-keys of (properties sub)
          using (hash-value value)
        sum (weight value)))

(defmethod nprops ((sub <subcategory>))
  "The number of properties in the subcategory."
  (hash-table-count (properties sub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <category>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <category> (<properties>)
  ((name :initarg :name
         :accessor name)
   (subcategories :initform (make-hash-table)
                  :accessor subcategories))
  (:documentation "A category of aesthetic properties."))

(defun print-category (cat stream)
  "Print the category to the provided stream."
  (format stream "~a:~%" (name cat))
  (loop for key being the hash-keys of (subcategories cat)
          using (hash-value value)
        do (print-subcategory value stream)))

(defmethod print-object ((object <category>) stream)
  "Print the category to the provided stream."
  (print-category object stream))

(defmethod weight ((cat <category>))
  "The total weight of the properties in the category's subcategories."
  (loop for key being the hash-keys of (subcategories cat)
          using (hash-value value)
        sum (weight value)))

(defmethod nprops ((cat <category>))
  "The total number of properties in the category's subcategories."
  (loop for key being the hash-keys of (subcategories cat)
          using (hash-value value)
        sum (nprops value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <aesthetic> (<properties>)
  ((categories :initform (make-hash-table)
               :accessor categories))
  (:documentation "An aesthetic of a member of the artworld."))

(defmethod print-object ((object <aesthetic>) stream)
  "Print the aesthetic to the provided stream."
  (format stream "#<<AESTHETIC> categories:~%")
  (loop for key being the hash-keys of (categories object)
          using (hash-value value)
        do (print-category value stream))
  (format stream ">~%"))

(defmethod nprops ((aesthetic <aesthetic>))
  "The total number of properties in the subcategories of the
   aesthetic's properties."
  (loop for key being the hash-keys of (categories aesthetic)
          using (hash-value value)
        sum (nprops value)))

(defmethod add-property ((ae <aesthetic>) name weight category subcategory)
  "Ensure that the category and subcategory exist, then add the property
   with the provided details to them and to the aesthetic."
  (if (not (gethash category (categories ae)))
      (setf (gethash category (categories ae))
            (make-instance '<category> :name category)))
  (let ((cat (gethash category (categories ae))))
    (if (not (gethash subcategory (subcategories cat)))
        (setf (gethash subcategory (subcategories cat))
              (make-instance '<subcategory>
                             :name subcategory
                             :category cat)))
    (let* ((sub (gethash subcategory (subcategories cat)))
           (prop (make-instance '<property>
                                :name name
                                :weight weight
                                :category cat
                                :subcategory sub)))
      ;;FIXME: define add-property on each and call here.
      ;;OR: bubble insertion up/down.
      (push prop (properties-list ae))
      (push prop (properties-list cat))
      (push prop (properties-list sub))
      (setf (gethash name (properties ae)) prop)
      (setf (gethash name (properties cat)) prop)
      (setf (gethash name (properties sub)) prop))))

(defmethod remove-property ((aesthetic <aesthetic>) (prop <property>))
  "Remove the property object from the aesthetic, subcategory,
   and category."
  (let ((name (name prop)))
    (setf (properties-list (subcategory prop))
          (remove-if (lambda (p) (string= (name p) name))
                     (properties-list (subcategory prop))))
    (setf (properties-list (category prop))
          (remove-if (lambda (p) (string= (name p) name))
                     (properties-list (category prop))))
    (setf (properties-list aesthetic)
          (remove-if (lambda (p) (string= (name p) name))
                     (properties-list aesthetic)))
    (remhash name (properties (subcategory prop)))
    (remhash name (properties (category prop)))
    (remhash name (properties aesthetic))))

(defmethod add-random-property ((aesthetic <aesthetic>) weight)
  "Add a randomly chosen property of the given weight to the aesthetic."
  (multiple-value-bind (name weight category subcategory)
      (random-property-values weight)
    (add-property aesthetic name weight category subcategory))
  aesthetic)

(defmethod add-random-properties ((aesthetic <aesthetic>))
  "Add a random amount of randomly chosen properties to the aesthetic."
  (dotimes (i (max +min-properties+
                   (random +max-properties-to-add+)))
    (add-random-property aesthetic (plus-or-minus-one)))
  aesthetic)

(defun aesthetic-opinions (aesthetic)
  "Sort the properties into likes (positive) and dislikes (negative)."
  (let ((likes (mapcar #'(lambda (p) (name p))
                       (properties-list-pos aesthetic)))
        (dislikes (mapcar #'(lambda (p) (name p))
                          (properties-list-neg aesthetic))))
    (values likes dislikes)))

(defun describe-aesthetic (aesthetic)
  "Describe the current likes and dislikes."
  ;;FIXME - Replace the final comma with an and or ampersand.
  (multiple-value-bind (likes dislikes) (aesthetic-opinions aesthetic)
    (let ((likes-string (when likes
                          (format nil
                                  "I like~{ ~A~^,~}. "
                                  likes)))
          (dislikes-string (when dislikes
                             (format nil
                                     "I dislike~{ ~A~^,~}."
                                     dislikes))))
      (values likes-string dislikes-string))))

(defun ensure-positive-categories-members (aesthetic)
  "Make sure that there is a path to a non-zero/negative item in each
   category."
  (dolist (c +properties+)
    (let ((cat-name (car c)))
      (when (or (not (gethash (car c) (categories aesthetic)))
                (= (nprops (gethash (car c) (categories aesthetic))) 0))
        (let* ((subcat (choose-one-of (cdr c)))
               (subcat-name (car subcat))
               (property (choose-one-of (cdr subcat))))
          (add-property aesthetic property 1.0 cat-name subcat-name)))))
  aesthetic)

(defun truncate-aesthetic-properties (aesthetic)
  "Delete 0+ randomly chosen properties to ensure the aesthetic has
   at most +max-properties+ ."
  (let ((count (- (nprops aesthetic) +max-properties+)))
    (if (> count 0)
        (let ((choices (choose-n-of count (properties-list aesthetic))))
          (dolist (choice choices)
            (remove-property aesthetic choice)))))
  aesthetic)

(defun delete-aesthetic-properties (aesthetic)
  "Delete 0+ randomly chosen properties, don't reduce the number of
   properties below +min-properties+."
  (let* ((count (random +max-properties-to-delete+))
         (choices (choose-n-of count (properties-list aesthetic))))
    (dolist (choice choices)
      (remove-property aesthetic choice)))
  aesthetic)

(defun add-aesthetic-properties (aesthetic)
  "Add zero or more randomly chosen properties that the aesthetic
   doesn't already contain."
  (loop with remaining = (min (max +min-properties+
                                   (random +max-properties-to-add+))
                              (- +max-properties+ (nprops aesthetic)))
        while (> remaining 0)
        do (multiple-value-bind (name weight category subcategory)
               (random-property-values (plus-or-minus-one))
             (when (not (gethash name (properties aesthetic)))
               (add-property aesthetic name weight category subcategory)
               (decf remaining))))
  aesthetic)

(defun mutate-aesthetic-properties (aesthetic)
  "Mutate (flip the weight of) zero or more randomly chosen properties."
  (dolist (prop (choose-n-of (random +max-properties-to-mutate+)
                             (properties-list aesthetic)))
    (setf (weight prop)
          (- (weight prop))))
  aesthetic)

(defmethod update-aesthetic ((aesthetic <aesthetic>))
  "Update the aesthetic to reflect the passage of time."
  (ensure-positive-categories-members
   (truncate-aesthetic-properties
    (add-aesthetic-properties
     (mutate-aesthetic-properties
      (delete-aesthetic-properties aesthetic))))))

(defmethod common-property-names ((a <aesthetic>) (b <aesthetic>))
  "Determine which properties are common to both aesthetics, and return
   their names."
  (intersection (property-names a) (property-names b)))

(defmethod update-aesthetic-based-on ((a <aesthetic>) (b <aesthetic>)
                                      influence)
  "Influence a based on b (add a small amount of b (* influence) to a)."
  (dolist (bp (properties-list b))
    (unless (gethash (properties a) (name bp))
      (add-property a (name bp) 0.0 (subcategory bp) (category bp)))
    (incf (gethash (properties a) (name bp))
          (* (weight bp) influence)))
  (truncate-aesthetic-properties a))

(defun eval-aesthetic (a b)
  "Score aesthetic a against aesthetic b."
  (let ((als (properties-list a))
        (bps (properties b))
        (total 0.0)
        (pos 0.0)
        (neg 0.0))
    (dolist (a als)
      (let ((b (gethash (name a) bps)))
        (when b
          (let ((val (* (weight a)
                        (weight b))))
            (incf total val)
            (if (> val 0)
                (incf pos val))
            (if (< val 0)
                (incf neg val))))))
    (values total pos neg)))

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

(defun make-aesthetic (default-value)
  "Make an aesthetic object and populate it with all the properties."
  (let ((aesthetic (make-instance '<aesthetic>)))
    (dolist (category +properties+)
      (let ((category-name (car category)))
        (dolist (subcategory (cdr category))
          (let ((subcategory-name (car subcategory)))
            (dolist (name (cdr subcategory))
              (add-property aesthetic name default-value category-name
                            subcategory-name))))))
    aesthetic))
;;(add-random-properties (make-instance '<aesthetic>)))

(defun genamount (aesthetic)
  "Generate a quantity description."
  (name (choose-property-pos (gethash :amounts (categories aesthetic)))))

(defun gencolour (aesthetic)
  "Choose a flat colour from a palette"
  (name (choose-property-pos (gethash :colours (categories aesthetic)))))

(defun describe-colour (aesthetic)
  "Generate a colour description."
  (let* ((tones (gethash :tones (categories aesthetic)))
         (tone (choose-property-pos tones)))
    (concatenate-string (name tone) (gencolour aesthetic))))

(defun gentexture (aesthetic)
  "Choose a texture."
  (name (choose-property-pos (gethash :textures (categories aesthetic)))))

(defun genappearance (aesthetic)
  "Generate the appearance of a figure."
  (concatenate-string (gentexture aesthetic) (describe-colour aesthetic)))

(defun gensize (aesthetic)
  (name (choose-property-pos (gethash :sizes (categories aesthetic)))))

(defun genshape (aesthetic)
  (name (choose-property-pos (gethash :shapes (categories aesthetic)))))

(defun describe-shape (aesthetic plural)
  "Generate a shape description."
  (concatenate-string (gensize aesthetic) (genappearance aesthetic)
                      (pluralise (genshape aesthetic) plural)))

(defun genground (aesthetic)
  "Generate a simple ground description."
  (genappearance aesthetic))

(defun describe-figures (aesthetic)
  "Describe a single (set of) figure(s) on a single ground."
  (let ((plural (genamount aesthetic)))
    (concatenate-string plural (describe-shape aesthetic plural)
                        "on a" (genground aesthetic) "ground")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <cyberartist>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <cyberartist> ()
  ((aesthetic :accessor aesthetic
              :initarg :aesthetic
              :documentation "The artist's aesthetic."))
  (:documentation "An artist collector in the artworld."))

(defun make-cyberartist ()
  (make-instance '<cyberartist> :aesthetic (make-aesthetic 1.0)))

(defmethod create-art ((o <cyberartist>))
  "Describe a single (set of) figure(s) on a single ground."
  (let* ((aesthetic (aesthetic o))
         (plural (genamount aesthetic)))
    (concatenate-string plural (describe-shape aesthetic plural)
                        "on a" (genground aesthetic) "ground")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <cybercritic>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <cybercritic> ()
  ((aesthetic :initarg :aesthetic
              :accessor aesthetic
              :documentation "The critic's aesthetic.")))

(defun make-cybercritic ()
  (make-instance '<cybercritic> :aesthetic (make-aesthetic 0.1)))

(defmethod print-aesthetic ((critic <cybercritic>))
  "Post a description of the critic's aesthetic"
  (multiple-value-bind (good bad)
      (describe-aesthetic (aesthetic critic))
    (format t "    ~a~%    ~a~%" good bad)))

(defmethod update ((critic <cybercritic>))
  "Update the aesthetic and print it."
  (update-aesthetic (aesthetic critic)))

;; How to handle ambiguities? yellow vs sunset yellow?
;; Sort keys by length
;; Find longest first
;; Remove from string
;; But what if that creates bogus sequences? Break into substrings?
;; Just always hyphenate multi-word sequences?

(defun score-artwork (artwork aesthetic)
  "Process the string description of the artwork to generate a score."
  (let ((clean-artwork (string-downcase artwork)))
    (loop for prop in (properties-list aesthetic)
          if (search (name prop) clean-artwork)
            sum (weight prop))))

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
             (properties aesthetic))
    (values +total -total)))

(defun evaluate-artwork (artwork aesthetic)
  "Set range to max of +/-1, probably less."
  (/ (score-artwork artwork aesthetic) (nprops aesthetic)))

(defun describe-artwork-evaluation (score)
  "Turn the -1.0..1.0 score into a verbal description."
  (cond
    ((< score -0.6) "terrible")
    ((< score -0.1) "bad")
    ((< score 0.1) "ok")
    ((< score 0.6) "good")
    (t "excellent")))

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
                     ((> (weight val) 0)
                      (incf +total (weight val)))
                     ((< (weight val) 0)
                      (decf -total (weight val))))))
             (properties aesthetic))
    (values +total -total)))

(defmethod critique-art ((critic <cybercritic>) (art string))
  "Verbally critique the artwork."
  (multiple-value-bind (+val -val)
      (total-aesthetic-values art (aesthetic critic))
    (format nil
            "~a \"~a\" is ~a."
            (choose-one-of '("I think that"
                             "In my opinion,"
                             "I would say that"
                             "Aesthetically speaking," ))
            art
            (describe-aesthetic-value +val -val))))

(defmethod critique-art ((critic <cybercritic>) (art array))
  "Critique a vector of artworks."
  (map 'vector
       #'(lambda (artwork) (critique-art critic artwork))
       art))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <cybercollector>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <cybercollector> ()
  ()
  (:documentation "An art collector in the artworld."))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <cyberartworld>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter seasons #("Spring" "Summer" "Autumn" "Winter"))

(defun format-date (months)
  (format nil "~a ~d"
          (aref seasons (mod (floor months 3) 4))
          (floor months 12)))

(defclass <cybernetic> ()
  ((date :initform (* 1950 12)
         :accessor date
         :documentation "The current month since 0.")
   (artist :initarg :artist
           :accessor artist
           :documentation "The cybernetic artist.")
   (critic :initarg :critic
           :accessor critic
           :documentation "The cybernetic critic.")
   (collector :initarg :collector
              :accessor collector
              :documentation "The cybernetic collector.")
   (artworks :initform #()
             :accessor artworks
             :documentation "Artworks created by the artist recently.")
   (critiques :initform #()
              :accessor critiques
              :documentation "Critiques recently generated by the critic.")
   (collected :initform #()
              :accessor collected
              :documentation "Purchases the collector made recently")))

(defun make-cybernetic ()
  (make-instance '<cybernetic>
                 :artist (make-cyberartist)
                 :critic (make-cybercritic)
                 :collector (make-cybercollector)))

(defun run-artist (artworld)
  "Update the artist's state for one time period."
  (let* ((count (+ (random 11) 1))
         (artworks (make-array count)))
    (dotimes (i count)
      (setf (aref artworks i)
            (create-art (artist artworld))))
    (setf (artworks artworld) artworks)
    (format t "The artist created new artworks depicting:~%")
    (loop for artwork across artworks
          do (format t "    ~a.~%" artwork))))

(defun run-critic (artworld)
  "Update the critic's state for one time period."
  (setf (critiques artworld)
        (critique-art (critic artworld)
                      (artworks artworld)))
  (format t "The critic had this to say about them:~%")
  (loop for critique across (critiques artworld)
        do (format t "    ~a~%" critique))
  (format t "Based on their current aesthetic:~%")
  (print-aesthetic (critic artworld))
  (update (critic artworld)))

(defun run-collector (artworld)
  "Update the collector's state for one time period."
  (setf (collected artworld)
        (collect-art (collector artworld)
                     (artworks artworld)
                     (critiques artworld)))
  (if (= (length (collected artworld)) 0)
      (format t "The collector did not buy any of the artworks.~%")
      (progn
        (format t "The collector bought the following artwork~p:~%"
                (length (collected artworld)))
        (loop for collecting across (collected artworld)
              do (format t "    ~a~%" collecting)))))

(defun run-cybernetic (artworld)
  "Update the artworld's state for one time period."
  (format t "~%")
  (format t "~a~%" (format-date (date artworld)))
  (format t "~%")
  (run-artist artworld)
  (format t "~%")
  (run-critic artworld)
  (format t "~%")
  (run-collector artworld)
  (format t "~%")
  (incf (date artworld) 3))

(defun run-cybernetic-until-collector-collects (artworld)
  "Repeatedly update the artworld's state until the collector buys one
   of the artist's artworks based on the critic's opinion."
  (loop while (= (length (collected artworld)) 0)
        do (run-cybernetic artworld)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main flow of execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main (&rest argv)
  "Main entry point for running The Cybernetic Artworld."
  (declare (ignorable argv))
  (setf *random-state* (make-random-state t))
  (let ((artworld (make-cybernetic)))
    (run-cybernetic-until-collector-collects artworld)))

#|-*- mode:lisp -*-|#
