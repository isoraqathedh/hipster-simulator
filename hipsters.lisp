;;;; HIPSTER SIMULATOR 2014
;;;; A Common Lisp implmentation of the hipster model as seen in http://arxiv.org/abs/1410.8001.

(defpackage info.isoraqathedh.hipster-simulator
  (:use :cl)
  (:nicknames :hipster-simulator))

(in-package :hipster-simulator)

(defvar *style-character-list* "#.=?O|+*_:~X;'$%^\"/vq`"
  "List of characters eligible for styles. This can be rebound if you desire more characters.")

;;; Classes

(defclass inhabitant ()
  ((type :initform :hipster
         :initarg :inhabitant-type
         :accessor inhabitant-type
         :documentation "A type of inhabitant. Currently only :hipster is supported.")
   (style :initform #\.
          :initarg :style
          :accessor styles
          :documentation "What clothes the inhabitant is wearing. Must be a character.")
   (hipsterish-tendency :initform 2/3
                        :initarg :hipsterish-tendency
                        :accessor hipsterish-tendency
                        :documentation "How likely would this individual be nonconformist at any given time."))
  (:documentation "An inhabitant in a hipster town."))
          

(defclass town-snapshot ()
  ((ticks :initform 0
          :initarg :time
          :accessor ticks
          :documentation "Ticks that have passed since the founding of this town.")
   (population :initform nil
               :initarg :population
               :accessor population
               :documentation "List of everyone who lives in the town, marked by their clothing choice.")
   (styles :initarg :styles
           :accessor styles
           :documentation "Available styles. Styles are always represented by characters.")
   (hipsterish-tendency :initarg :hipsterish-tendency
                        :accessor hipsterish-tendency
                        :documentation "How likely would any inidividual be nonconformist at any given time."))
  (:documentation "A hipster town at a particular time."))

(defclass delayed-town (town-snapshot)
  ((period :initform 2
           :initarg :period
           :accessor period
           :documentation "How long before the individuals know the clothing choice of their peers, measured in ticks.")
   (history :initarg :history
            :accessor town-history
            :documentation "The last iterations of the simulation, for use in computing certain future iterations."))
  (:documentation "A town with a temporal delay. Inhabitants are only able to see their popularities from some time ago."))

(defclass foggy-town (town-snapshot)
  ((visibility :initform 4
               :initarg :visibility
               :accessor visibility
               :documentation "How many other other inhabitants that each inhabitant can see."))
  (:documentation "A town with a spatial delay. Inhabitants are only able to see their popularities with reference to a subset of the population."))

;;; Class constructors

(defun generate-random-town (population styles volatile)
  "Generates a random town of n people with the given vector of styles."
  (let ((out (make-array (list population) :adjustable volatile)))
    (dotimes (i population)
      (setf (aref out i) (make-instance 'inhabitant :style (aref styles (random (length styles))))))
    out))
               
(defun make-town (population &key (hipsterish-tendency 2/3) (styles #(#\# #\.)) volatile)
  "Builds a new town with a population of population, seeding with the two types of clothes."
  (make-instance 'town-snapshot
                 :population (generate-random-town population styles volatile)
                 :styles styles
                 :hipsterish-tendency hipsterish-tendency))

(defun make-delayed-town (population period &key (hipsterish-tendency 2/3) (styles #(#\# #\.)) volatile)
  (make-instance 'delayed-town
                 :population (generate-random-town population styles volatile)
                 :period period
                 :history (make-array (list period population) :adjustable volatile :initial-element nil)
                 :hipsterish-tendency hipsterish-tendency
                 :styles styles))

(defmethod initialize-instance :after ((instance delayed-town) &key)
  (when (zerop (ticks instance))
    (loop for i across (population instance)
          for j from 0 below (length (population instance))
          do (setf (aref (town-history instance) 0 j) i))))

(defun make-foggy-town (population visibility &key (hipsterish-tendency 2/3) (styles #(#\# #\.)) volatile)
  (make-instance 'foggy-town
                 :population (generate-random-town population styles volatile)
                 :visibility visibility
                 :hipsterish-tendency hipsterish-tendency
                 :styles styles))

;;; Common class methods

(defmethod print-object ((object town-snapshot) stream)
  (with-accessors ((ticks ticks) (population population) (tendency hipsterish-tendency) (styles styles)) object
    (print-unreadable-object (object stream :type t)
      (format stream ":~:[~;VOLATILE-~]POPULATION ~a :TENDENCY ~4,2f% :STYLES ~a @ t = ~a"
              (adjustable-array-p population) (length population) (* tendency 100) (length styles) ticks))))

(defmethod print-object ((object delayed-town) stream)
  (with-accessors ((ticks ticks) (population population) (tendency hipsterish-tendency) (styles styles) (period period)) object
    (print-unreadable-object (object stream :type t)
      (format stream ":~:[~;VOLATILE-~]POPULATION ~a :TENDENCY ~4,2f% :STYLES ~a :DELAY ~a @ t = ~a"
              (adjustable-array-p population) (length population) (* tendency 100) (length styles) period ticks))))

(defmethod print-object ((object foggy-town) stream)
  (with-accessors ((ticks ticks) (population population) (tendency hipsterish-tendency) (styles styles) (visibility visibility)) object
    (print-unreadable-object (object stream :type t)
      (format stream ":~:[~;VOLATILE-~]POPULATION ~a :TENDENCY ~4,2f% :STYLES ~a :VISIBILITY ~a @ t = ~a"
              (adjustable-array-p population) (length population) (* tendency 100) (length styles) visibility ticks))))

(defmethod print-object ((object inhabitant) stream)
  (with-accessors ((type inhabitant-type) (tendency hipsterish-tendency) (styles styles)) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~@c ~:[(~a) ~;~*~]:TENDENCY ~4,2f%" styles (eql type :hipster) type (* tendency 100)))))

(defgeneric copy (town)
  (:documentation "Returns a deep copy of a given town. Useful for when you need to keep the initiator town around.")
  (:method ((town town-snapshot))
    (make-instance 'town-snapshot
                   :hipsterish-tendency (hipsterish-tendency town)
                   :styles              (copy-seq (styles town))
                   :population          (map 'vector #'copy (population town))
                                        ; This line might have to change as I may replace the characters with actual human objects.
                   :time                (ticks town)))
  (:method ((town delayed-town))
    (make-instance 'delayed-town
                   :hipsterish-tendency (hipsterish-tendency town)
                   :styles              (copy-seq (styles town))
                   :population          (map 'vector #'copy (population town)) ; Same here.
                   :time                (ticks town)
                   :period              (period town)
                   :history             (loop with history = (make-array (list (period town) (length (population town)))
                                                                         :adjustable (adjustable-array-p (town-history town))
                                                                         :initial-element nil)
                                              with to-copy = (town-history town)
                                              ;; Copying multidimensional arrays = ugh.
                                              for i from 0 below (period town)
                                              do (loop for j from 0 below (length (population town))
                                                       when (aref to-copy i j)
                                                         do (setf (aref history i j) (copy (aref to-copy i j))))
                                              finally (return history))))
  (:method ((town foggy-town))
    (Make-instance 'foggy-town
                   :visibility          (visibility town)
                   :hipsterish-tendency (hipsterish-tendency town)
                   :styles              (copy-seq (styles town))
                   :population          (map 'vector #'copy (population town))
                   :time                (ticks town)))
  (:method ((inhabitant inhabitant))
    (make-instance 'inhabitant
                   :hipsterish-tendency (hipsterish-tendency inhabitant)
                   :style               (styles inhabitant)
                   :inhabitant-type     (inhabitant-type inhabitant))))

;;; Other methods

;;; Methods pertaining to direct simulation

(defun true-with-probability (probability)
  (assert (<= 0 probability 1) (probability))
  (< (random 1.0) probability))

(defgeneric style-popularity (town)
  (:documentation "Determines the popularity of each style in the town.")
  (:method ((town town-snapshot))
    (loop with stats = (loop for style across (styles town)
                             collect (cons style 0))
          for i across (population town)
          do (incf (cdr (assoc (styles i) stats)))
          finally (return stats)))
  (:method ((town delayed-town))
    (loop with stats = (loop for style across (styles town) collect (cons style 0))
          for i from 0 below (length (population town))
          do (incf (cdr (assoc (styles (aref (town-history town) (mod (1+ (ticks town)) (period town)) i)) stats)))
          finally (return stats)))
  (:method ((town foggy-town))
    (loop with stats = (loop for style across (styles town) collect (cons style 0))
          with population = (population town)
          for i from 1 to (visibility town)
          for sample = (styles (elt population (random (length population))))
          do (incf (cdr (assoc sample stats)))
          finally (return stats))))

(defgeneric survey (town)
  (:documentation "Counts what styles the inhabitant sees.")
  (:method ((town town-snapshot))
    (loop with style-list = (style-popularity town)
          for (style) in style-list
          for i = (copy-tree style-list)
          unless (zerop (cdr (assoc style i))) ; This doesn't perfectly implement "don't count yourself" but that would have to wait.
            do (decf (cdr (assoc style i)))
          collect (cons style (nbutlast (sort i #'< :key #'cdr)
                                        (ceiling (/ (length style-list) 2)))))))

(defun select-style (style style-alist)
  (let ((style-alist (cdr (assoc style style-alist))))
    (car (nth (random (length style-alist)) style-alist))))

(defgeneric deconform (town stickiness)
  (:documentation "Find-next-tick algorithm. Destructively modifies the snapshot so that inhabitants attempt to wear the less popular style. 
Stickiness refers to how likely any inhabitant would like to keep the clothes that are currently on if the inhabitant decides not to be nonconformist.")
  (:method ((town town-snapshot) (stickiness number))
    (let ((candidate-styles (survey town)))
      (with-accessors ((population population) (tendency hipsterish-tendency) (town-styles styles)) town
        (loop for i across population
            do (setf (styles i)
                     (cond
                       ((true-with-probability tendency)
                        (select-style (styles i) candidate-styles))
                       ((true-with-probability stickiness) (styles i))
                       (t (aref town-styles (random (length town-styles)))))))))))

(defgeneric wander (town stickiness)
  (:documentation "Find-next-tick algorithm. Destructively modifies the snapshot so that the inhabitants randomly change clothes sometimes.
Stickiness refers to how likely any inhabitant would like to keep the clothes that are currently on.")
  (:method ((town town-snapshot) (stickiness number))
    (with-accessors ((town-styles styles) (population population)) town
      (loop for i across population
            do (setf (styles i)
                     (if (true-with-probability stickiness)
                         (styles i)
                         (aref town-styles (random (length town-styles)))))))))
(defgeneric tick! (town)
  (:documentation "Destructively modifies a snapshot to become the next iteration of the simulation.")
  (:method ((town town-snapshot))
    (deconform town 10/11))
  (:method ((town delayed-town))
    (if (null (aref (town-history town) (mod (1+ (ticks town)) (period town)) 0))
        (wander town 5/6)        ; While the history fills up, randomly wander, changing fashions 1/6 of the time
        (deconform town 10/11))) ; After that, proceed as normal
  (:method :after ((town town-snapshot))
    (incf (ticks town)))
  (:method :after ((town delayed-town))
    (loop for i across (population town)
          for j from 0
          do (setf (aref (town-history town) (mod (ticks town) (period town)) j) (copy i))))
  (:method :around ((town town-snapshot))
    (call-next-method)
    town))

;;; Reporting methods

(defgeneric print-population (town &key stream limit specific-style)
  (:documentation "Prints out the members in the population along with a generation number wearing the given style if non-nil, truncated to [limit] members if provided.")
  (:method ((town town-snapshot) &key (stream t) limit specific-style)
    (with-accessors ((age ticks) (members population)) town
      (format stream "~&~3d: ~{~a~}~:[~;…~]"
              age
              (if specific-style
                  (substitute-if #\Space #'(lambda (a) (char-not-equal specific-style a))
                                 (map 'list #'styles (subseq members 0 limit)))
                  (map 'list #'styles (subseq members 0 limit)))
              limit))))

(defgeneric print-popularity (town &key stream limit)
  (:documentation "Prints out the styles and its percentage popularity, along with a genreation number, truncated to [limit] styles if provided.")
  (:method ((town town-snapshot) &key (stream t) limit)
    (with-accessors ((age ticks) (members population) (styles styles)) town
      (format stream "~&~3d:  " age)
      (loop for i across styles
            for j from 0 below (or limit (length styles))
            do (format stream "~a ~7,2f%~,20t" i (/ (count-if #'(lambda (a) (char-equal i (styles a))) members)
                                                 (length members)
                                                 1/100))))))

;;; When seeing the development of the town, we need to repeat things a lot.
(defmacro with-town-iterator ((var town &key (times 20) (tick-time :before) copyp) &body body)
  "Creates a loop that evolves a town, allowing the user to do something for each generation of the town. Can be made nondestructive by passing non-nil to copyp."
  `(let ((,var ,(if copyp `(copy ,town) town)))
     (assert (typep ,var 'town-snapshot))
     (assert (typep ,tick-time '(member :before :after)))
     (loop repeat ,times
           do (progn
                ,(if (eql tick-time :before) `(tick! ,var))
                ,@body
                ,(if (eql tick-time :after) `(tick! ,var))))
     ,var))
