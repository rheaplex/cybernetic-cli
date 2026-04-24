
;; Copyright (C) 2009, 2023, 2026  Myers Studio Ltd. & Rhea Myers rhea@myers.studio

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
     (:geometric "circle" "triangle" "square" "pentagon" "hexagon" "octagon")
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
           :accessor weight)))

(defmethod print-object ((object <property>) stream)
  (format stream "#<<property> ~a: ~a>" (name object) (weight object)))

(defun random-property-values (weight)
  (let* ((category (choose-randomly +properties+))
         (subcategory (choose-randomly (cdr category)))
         (property (choose-randomly (cdr subcategory))))
    (values property weight (car category)(car subcategory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <property-list>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <property-list> ()
  ((properties :initform (make-hash-table)
               :accessor properties)
   (properties-list :initform nil
                    :accessor properties-list)))

(defmethod property-names ((p <property-list>))
  (loop for key being the hash-keys of (properties p)
        collecting key))

(defmethod properties-list-pos ((pl <property-list>))
  (remove-if-not #'(lambda (p) (> (weight p) 0.0)) (properties-list pl)))

(defmethod properties-list-neg ((pl <property-list>))
  (remove-if-not #'(lambda (p) (< (weight p) 0.0)) (properties-list pl)))

(defmethod properties-total-value (items)
  "Sum the values of the properties."
  (loop for item in items
        sum (weight item)))

(defmethod choose-positive-property ((p <property-list>))
  (let* ((candidates (properties-list-pos p))
         (total (properties-total-value candidates))
         (i (random total)))
    (loop for prop in candidates
          sum (weight prop) into prob-so-far
          when (>= prob-so-far i)
            return prop)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <subcategory>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <subcategory> (<property-list>)
  ((category :initarg :category
           :accessor category)
   (name :initarg :name
         :accessor name)))

(defun print-subcategory (sub stream)
  (format stream "  ~a:" (name sub))
  (loop for key being the hash-keys of (properties sub)
          using (hash-value value)
        do (format stream " ~a: ~a" (name value) (weight value)))
  (format stream "~%"))

(defmethod print-object ((object <subcategory>) stream)
  (print-subcategory object stream))

(defmethod weight ((sub <subcategory>))
  (loop for key being the hash-keys of (properties sub)
          using (hash-value value)
        sum (weight value)))

(defmethod nprops ((sub <subcategory>))
  (hash-table-count (properties sub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <category>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <category> (<property-list>)
  ((name :initarg :name
         :accessor name)
   (subcategories :initform (make-hash-table)
                  :accessor subcategories)))

(defun print-category (cat stream)
  (format stream "~a:~%" (name cat))
  (loop for key being the hash-keys of (subcategories cat)
          using (hash-value value)
        do (print-subcategory value stream)))

(defmethod print-object ((object <category>) stream)
  (print-category object stream))

(defmethod weight ((cat <category>))
  (loop for key being the hash-keys of (subcategories cat)
          using (hash-value value)
        sum (weight value)))

(defmethod nprops ((cat <category>))
  (loop for key being the hash-keys of (subcategories cat)
          using (hash-value value)
        sum (nprops value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <aesthetic> (<property-list>)
  ((categories :initform (make-hash-table)
               :accessor categories)))

(defmethod print-object ((object <aesthetic>) stream)
  (format stream "#<<AESTHETIC> categories:~%")  
  (loop for key being the hash-keys of (categories object)
          using (hash-value value)
        do (print-category value stream))
  (format stream ">~%"))

(defmethod nprops ((ae <aesthetic>))
  (loop for key being the hash-keys of (categories ae)
          using (hash-value value)
        sum (nprops value)))

(defmethod add-property ((ae <aesthetic>) name weight category subcategory)
  (if (not (gethash category (categories ae)))
      (setf (gethash category (categories ae))
            (make-instance '<category> :name category)))
  (let ((cat (gethash category (categories ae))))
    (if (not (gethash subcategory (subcategories cat)))
             (setf (gethash subcategory (subcategories cat))
                   (make-instance '<subcategory> :name subcategory :category cat)))
    (let* ((sub (gethash subcategory (subcategories cat)))
           (prop (make-instance '<property>
                                :name name
                                :weight weight
                                :category cat
                                :subcategory sub)))
      ;;FIXME: define add-property on each and call here. OR: bubble insertion up/down.
      (push prop (properties-list ae))
      (push prop (properties-list cat))
      (push prop (properties-list sub))
      (setf (gethash name (properties ae)) prop)
      (setf (gethash name (properties cat)) prop)
      (setf (gethash name (properties sub)) prop))))

(defmethod remove-property ((ae <aesthetic>) (prop <property>))
  (let ((name (name prop)))
    (setf (properties-list (subcategory prop))
          (remove-if (lambda (p) (string= (name p) name))
                     (properties-list (subcategory prop))))
    (setf (properties-list (category prop))
          (remove-if (lambda (p) (string= (name p) name))
                     (properties-list (category prop))))
    (setf (properties-list ae)
          (remove-if (lambda (p) (string= (name p) name))
                     (properties-list ae)))
    (remhash name (properties (subcategory prop)))
    (remhash name (properties (category prop)))
    (remhash name (properties ae))))

(defmethod add-random-property ((ae <aesthetic>) weight)
  (multiple-value-bind (name weight category subcategory) (random-property-values weight)
    (add-property ae name weight category subcategory))
  ae)

(defmethod add-random-properties ((ae <aesthetic>))
  (dotimes (i (max +min-properties+ 
                   (random +max-properties-to-add+)))
    (add-random-property ae (plus-or-minus-one)))
  ae)

(defun aesthetic-opinions (aesthetic)
  "Sort the properties into likes and dislikes."
  (let ((likes (mapcar #'(lambda (p) (name p)) (properties-list-pos aesthetic)))
        (dislikes (mapcar #'(lambda (p) (name p)) (properties-list-neg aesthetic))))
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

(defun ensure-positive-categories-members (aesthetic)
  "Make sure that there is a path to a non-zero/negative item in each category."
  (dolist (c +properties+)
    (let ((category-name (car c)))
      (when (or (not (gethash (car c) (categories aesthetic)))
                (= (nprops (gethash (car c) (categories aesthetic))) 0))
          (let* ((subcategory (choose-one-of (cdr c)))
                 (property (choose-one-of (cdr subcategory))))
            (add-property aesthetic property 1.0 category-name (car subcategory))))))
  aesthetic)

(defun truncate-aesthetic-properties (aesthetic)
  "Delete 0+ properties, don't reduce properties below +min-properties+."
  (let ((count (- (nprops aesthetic) +max-properties+)))
    (if (> count 0)
      (let ((choices (choose-n-of count (properties-list aesthetic))))
        (dolist (choice choices)
          (remove-property aesthetic choice)))))
  aesthetic)

(defun delete-aesthetic-properties (aesthetic)
  "Delete 0+ properties, don't reduce properties below +min-properties+."  
  (let* ((count (random +max-properties-to-delete+))
         (choices (choose-n-of count (properties-list aesthetic))))
    (dolist (choice choices)
      (remove-property aesthetic choice)))
  aesthetic)

(defun add-aesthetic-properties (aesthetic)
    "Add zero or more properties."
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
  "Mutate (flip the weight of) zero or more properties."
  (dolist (prop (choose-n-of (random +max-properties-to-mutate+)
                             (properties-list aesthetic)))
    (setf (weight prop)
          (- (weight prop))))
  aesthetic)

(defmethod update-aesthetic ((aesthetic <aesthetic>))
  "Update the aesthetic."
  (ensure-positive-categories-members
   (truncate-aesthetic-properties
    (add-aesthetic-properties 
     (mutate-aesthetic-properties
      (delete-aesthetic-properties aesthetic))))))

(defmethod common-property-names ((a <aesthetic>) (b <aesthetic>))
  (intersection (property-names a) (property-names b)))

(defmethod update-aesthetic-based-on ((a <aesthetic>) (b <aesthetic>) influence)
  "Influence a based on b (add a small amount of b (* influence) to a)."
  (dolist (bp (properties-list b))
    (unless (gethash (properties a) (name bp))
      (add-property a (name bp) 0.0 (subcategory bp) (category bp)))
    (incf (gethash (properties a) (name bp))
          (* (weight bp) influence)))
  (truncate-aesthetic-properties a))

(defun eval-aesthetic (a b)
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
  (let ((aesthetic (make-instance '<aesthetic>)))
    (dolist (category +properties+)
      (let ((category-name (car category)))
        (dolist (subcategory (cdr category))
          (let ((subcategory-name (car subcategory)))
            (dolist (name (cdr subcategory))
              (add-property aesthetic
                            name
                            default-value
                            category-name
                            subcategory-name))))))
    aesthetic))
  ;;(add-random-properties (make-instance '<aesthetic>)))

(defun amount (aesthetic)
  "Generate a quantity description."
  (name (choose-positive-property (gethash :amounts (categories aesthetic)))))

(defun colour (aesthetic)
  "Choose a flat colour from a palette"
  (name (choose-positive-property (gethash :colours (categories aesthetic)))))

(defun colour-description (aesthetic)
  "Generate a colour description."
  (concatenate-string (name (choose-positive-property (gethash :tones
                                                         (categories aesthetic))))
                      (colour aesthetic)))

(defun texture (aesthetic)
  "Choose a texture."
  (name (choose-positive-property (gethash :textures (categories aesthetic)))))

(defun appearance (aesthetic)
  "Generate the appearance of a figure."
  (concatenate-string (texture aesthetic)
                      (colour-description aesthetic)))

(defun size (aesthetic)
  (name (choose-positive-property (gethash :sizes (categories aesthetic)))))

(defun shape (aesthetic)
  (name (choose-positive-property (gethash :shapes (categories aesthetic)))))

(defun shape-description (aesthetic plural)
  "Generate a shape description."
  (concatenate-string (size aesthetic)
                      (appearance aesthetic)
                      (pluralise (shape aesthetic) plural)))

(defun ground (aesthetic)
  "Generate a simple ground description."
  (appearance aesthetic))

(defun generate-description (aesthetic)
  "Describe a single (set of) figure(s) on a single ground."
  (let ((plural (amount aesthetic)))
    (concatenate-string plural (shape-description aesthetic plural)
                        "on a" (ground aesthetic) "ground")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <cyberartist>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <cyberartist> ()
  ((aesthetic :accessor aesthetic
              :initarg :aesthetic
              :documentation "The artist's aesthetic.")))

(defun make-cyberartist ()
  (make-instance '<cyberartist> :aesthetic (make-aesthetic 1.0)))

(defmethod create-art ((o <cyberartist>))
  "Describe a single (set of) figure(s) on a single ground."
  (let* ((aesthetic (aesthetic o))
         (plural (amount aesthetic)))
    (concatenate-string plural (shape-description aesthetic plural)
                        "on a" (ground aesthetic) "ground")))

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
;; Just always hypenate multi-word sequences?

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
  (/ (score-artwork artwork aesthetic)
     (nprops aesthetic)))

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
            (choose-one-of '("I think that" "In my opinion," "I would say that"
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
              :documentation "Critiques generated by the critic recently.")
   (collected :initform #()
              :accessor collected
              :documentation "Purchases by the collector recently")))

(defun make-cybernetic ()
  (make-instance '<cybernetic>
                 :artist (make-cyberartist)
                 :critic (make-cybercritic)
                 :collector (make-cybercollector)))

(defun run-artist (artworld)
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
  (loop while (= (length (collected artworld)) 0)
        do (run-cybernetic artworld)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main flow of execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (setf *random-state* (make-random-state t))
  (let ((artworld (make-cybernetic)))
    (run-cybernetic-until-collector-collects artworld)))
