;;;; HIPSTER SIMULATOR 2014
;;;; A Common Lisp implmentation of the hipster model as seen in http://arxiv.org/abs/1410.8001.

(defpackage info.isoraqathedh.hipster-simulator
  (:use :cl)
  (:nicknames :hipster-simulator))

(in-package :hipster-simulator)

(defvar *style-character-list* "#.=?O|+*_:~X;'$%^\"/vq`"
  "List of characters eligible for styles. This can be rebound if you desire more characters.")

;;; Classes

(defstruct inhabitant
  (type :hipster)
  (style #\.)
  hipsterish-tendency)

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

;; Structs are easy but they still need a bit of tweaking.
(defmethod hipsterish-tendency ((object inhabitant)) (inhabitant-hipsterish-tendency object))
(defmethod (setf hipsterish-tendency) (value (object inhabitant) ) (setf (inhabitant-hipsterish-tendency object) value))
(defmethod styles ((object inhabitant)) (inhabitant-style object))
(defmethod (setf styles) (value (object inhabitant)) (setf (inhabitant-style object) value))

;;; Class constructors

(defun generate-random-town (population styles)
  "Generates a random town of n people with the given vector of styles."
  (mapcar #'(lambda (a)
              (declare (ignore a))
              (make-inhabitant :style (aref styles (random (length styles)))))
          (make-list population)))
               
(defun make-town (population &key (hipsterish-tendency 2/3) (styles #(#\# #\.)))
  "Builds a new town with a population of population, seeding with the two types of clothes."
  (make-instance 'town-snapshot
                 :population (generate-random-town population styles)
                 :styles styles
                 :hipsterish-tendency hipsterish-tendency))

(defun make-delayed-town (population period &key (hipsterish-tendency 2/3) (styles #(#\# #\.)))
  (make-instance 'delayed-town
                 :population (generate-random-town population styles)
                 :period period
                 :history (make-array (list period))
                 :hipsterish-tendency hipsterish-tendency
                 :styles styles))

(defmethod initialize-instance :after ((instance delayed-town) &key)
  (when (zerop (ticks instance))
    (setf (aref (town-history instance) (mod (ticks instance) (period instance))) (population instance))))

(defun make-foggy-town (population visibility &key (hipsterish-tendency 2/3) (styles #(#\# #\.)))
  (make-instance 'foggy-town
                 :population (generate-random-town population styles)
                 :visibility visibility
                 :hipsterish-tendency hipsterish-tendency
                 :styles styles))

;;; Common class methods

(defmethod print-object ((object town-snapshot) stream)
  (with-accessors ((ticks ticks) (population population) (tendency hipsterish-tendency) (styles styles)) object
    (print-unreadable-object (object stream :type t)
      (format stream ":POPULATION ~a :TENDENCY ~4,2f% :STYLES ~a @ t = ~a" (length population) (* tendency 100) (length styles) ticks))))

(defmethod print-object ((object delayed-town) stream)
  (with-accessors ((ticks ticks) (population population) (tendency hipsterish-tendency) (styles styles) (period period)) object
    (print-unreadable-object (object stream :type t)
      (format stream ":POPULATION ~a :TENDENCY ~4,2f% :STYLES ~a :DELAY ~a @ t = ~a" (length population) (* tendency 100) (length styles) period ticks))))

(defmethod print-object ((object foggy-town) stream)
  (with-accessors ((ticks ticks) (population population) (tendency hipsterish-tendency) (styles styles) (visibility visibility)) object
    (print-unreadable-object (object stream :type t)
      (format stream ":POPULATION ~a :TENDENCY ~4,2f% :STYLES ~a :VISIBILITY ~a @ t = ~a"
              (length population) (* tendency 100) (length styles) visibility ticks))))

(defgeneric copy-town (town)
  (:documentation "Returns a deep copy of a given town. Useful for when you need to keep the initiator town around.")
  (:method ((town town-snapshot))
    (make-instance 'town-snapshot
                   :hipsterish-tendency (hipsterish-tendency town)
                   :styles              (styles town)
                   :population          (mapcar #'copy-inhabitant (population town))
                                        ; This line might have to change as I may replace the characters with actual human objects.
                   :time                (ticks town)))
  (:method ((town delayed-town))
    (make-instance 'delayed-town
                   :hipsterish-tendency (hipsterish-tendency town)
                   :styles              (styles town)
                   :population          (mapcar #'copy-inhabitant (population town)) ; Same here.
                   :time                (ticks town)
                   :period              (period town)
                   :history             (loop with history = (make-array (list (period town)))
                                              ;; Copying arrays with lists embedded in them = ugh.
                                              for i across (town-history town)
                                              for j from 0
                                              do (setf (aref history j) i)
                                              finally (return history))))
  (:method ((town foggy-town))
    (Make-instance 'foggy-town
                   :visibility          (visibility town)
                   :hipsterish-tendency (hipsterish-tendency town)
                   :styles              (styles town)
                   :population          (mapcar #'copy-inhabitant (population town))
                   :time                (ticks town))))

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
          for i in (population town)
          do (incf (cdr (assoc (styles i) stats)))
          finally (return stats)))
  (:method ((town delayed-town))
    (call-next-method
     (make-instance 'town-snapshot
                    :styles (styles town)
                    :population (aref (town-history town) (mod (1+ (ticks town)) (period town))))))
  (:method ((town foggy-town))
    (loop with stats = (loop for style across (styles town) collect (cons style 0))
          with population = (population town)
          for i from 1 to (visibility town)
          for sample = (styles (nth (random (length population)) population))
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

(defgeneric deconform (town)
  (:documentation "Simulates the selection of styles of all hipsters.
In this case, they will attempt at random any style on the less popular half of the style.")
  (:method ((town town-snapshot))
    (let ((candidate-styles (survey town)))
      (mapc #'(lambda (style-of-self)
                (setf (styles style-of-self)
                      (cond
                        ((true-with-probability (hipsterish-tendency town))
                         (select-style (styles style-of-self) candidate-styles))
                        ((true-with-probability 10/11) (styles style-of-self))
                        (t (aref (styles town) (random (length (styles town))))))))
            (population town))))
  (:method ((town delayed-town))
    (if (numberp (aref (town-history town) (mod (1+ (ticks town)) (period town))))
        ;; While the history fills up, randomly wander, changing fashions 1/6 of the time
        (mapc #'(lambda (style-of-self)
                  (setf (styles style-of-self)
                        (if (true-with-probability 1/6)
                            (aref (styles town) (random (length (styles town))))
                            (styles style-of-self))))
         (population town))
        (let ((candidate-styles (survey town)))
          ;; After that, proceed as normal
          (mapc #'(lambda (style-of-self)
                    (setf (styles style-of-self)
                          (cond
                            ((true-with-probability (hipsterish-tendency town))
                             (select-style (styles style-of-self) candidate-styles))
                            ((true-with-probability 10/11) (styles style-of-self))
                            (t (aref (styles town) (random (length (styles town))))))))
                (population town))))))

(defgeneric tick! (town)
  (:documentation "Destructively modifies a snapshot to become the next iteration of the simulation.")
  (:method ((town town-snapshot))
    (deconform town)
    (incf (ticks town)))
  (:method :after ((town delayed-town))
    (setf (aref (town-history town) (mod (ticks town) (period town))) (population town))))

;;; Reporting methods

(defgeneric print-population (town &key stream limit specific-style)
  (:documentation "Prints out the members in the population along with a generation number wearing the given style if non-nil, truncated to [limit] members if provided.")
  (:method ((town town-snapshot) &key (stream t) limit specific-style)
    (with-accessors ((age ticks) (members population)) town
      (format stream "~&~3d: ~{~a~}~:[~;…~]"
              age
              (if specific-style
                  (substitute-if #\Space #'(lambda (a) (char-not-equal specific-style a))
                                 (mapcar #'styles (subseq members 0 limit)))
                  (mapcar #'styles (subseq members 0 limit)))
              limit))))

(defgeneric print-popularity (town &key stream limit)
  (:documentation "Prints out the styles and its percentage popularity, along with a genreation number, truncated to [limit] styles if provided.")
  (:method ((town town-snapshot) &key (stream t) limit)
    (with-accessors ((age ticks) (members population) (styles styles)) town
      (format stream "~&~3d:  " age)
      (loop for i across styles
            for j from 0 below (or limit (length styles))
            do (format stream "~20@<~a ~7,2f%~>" i (/ (count-if #'(lambda (a) (char-equal i (styles a))) members)
                                                 (length members)
                                                 1/100))))))

;;; When seeing the development of the town, we need to repeat things a lot.
(defmacro with-town-iterator ((var town &key (times 20) (tick-time :before) copyp) &body body)
  "Creates a loop that evolves a town, allowing the user to do something for each generation of the town. Can be made nondestructive by passing non-nil to copyp."
  `(let ((,var ,(if copyp `(copy-town ,town) town)))
     (assert (typep ,var 'town-snapshot))
     (assert (typep ,tick-time '(member :before :after)))
     (loop repeat ,times
           do (progn
                ,(if (eql tick-time :before) `(tick! ,var))
                ,@body
                ,(if (eql tick-time :after) `(tick! ,var))))
     ,var))
