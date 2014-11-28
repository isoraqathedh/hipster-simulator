;;;; HIPSTER SIMULATOR 2014
;;;; A Common Lisp implmentation of the hipster model as seen in http://arxiv.org/abs/1410.8001.

(defpackage info.isoraqathedh.hipster-simulator
  (:use :cl)
  (:nicknames :hipster-simulator))

(in-package :hipster-simulator)

(defvar *style-character-list* "#.=?O|+*_:~X;'$%^\"/vq`"
  "List of characters eligible for styles. This can be rebound if you desire more characters.")

;;; Classes

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
            :documentation "The last iterations of the simulation, for use in computing certain future iterations.")))

;;; Class constructors

(defun generate-random-town (population styles)
  "Generates a random town of n people with the given vector of styles."
  (mapcar #'(lambda (a)
              (declare (ignore a))
              (aref styles (random (length styles))))
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
  (setf (aref (town-history instance) (mod (ticks instance) (period instance))) (population instance)))

;;; Common class methods

(defmethod print-object ((object town-snapshot) stream)
  (with-accessors ((ticks ticks) (population population) (tendency hipsterish-tendency) (styles styles)) object
    (print-unreadable-object (object stream :type t)
      (format stream ":POPULATION ~a :TENDENCY ~4,2f% :STYLES ~a @ t = ~a" (length population) (* tendency 100) styles ticks))))

(defmethod print-object ((object delayed-town) stream)
  (with-accessors ((ticks ticks) (population population) (tendency hipsterish-tendency) (styles styles) (period period)) object
    (print-unreadable-object (object stream :type t)
      (format stream ":POPULATION ~a :TENDENCY ~4,2f% :STYLES ~a :DELAY ~a @ t = ~a" (length population) (* tendency 100) styles period ticks))))

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
          do (incf (cdr (assoc i stats)))
          finally (return stats)))
  (:method ((town delayed-town))
    (call-next-method
     (make-instance 'town-snapshot
                    :styles (styles town)
                    :population (aref (town-history town) (mod (1+ (ticks town)) (period town)))))))

(defgeneric survey (town)
  (:documentation "Counts what styles the inhabitant sees.")
  (:method ((town town-snapshot))
    (loop with style-list = (style-popularity town)
          for (style) in style-list
          for i = (copy-tree style-list)
          do (decf (cdr (assoc style i)))
          collect (cons style (nbutlast (sort i #'< :key #'cdr)
                                        (ceiling (/ (length style-list) 2)))))))

(defgeneric deconform (town)
  (:documentation "Simulates the selection of styles of all hipsters.
In this case, they will attempt at random any style on the less popular half of the style.")
  (:method ((town town-snapshot))
    (let ((candidate-styles (survey town)))
      (mapcar #'(lambda (style-of-self)
                  (cond
                    ((true-with-probability (hipsterish-tendency town))
                     (car (nth
                           (random
                            (length
                             (cdr (assoc style-of-self candidate-styles))))
                           (cdr (assoc style-of-self candidate-styles)))))
                    ((true-with-probability 10/11) style-of-self)
                    (t (aref (styles town) (random (length (styles town)))))))
              (population town))))
  (:method ((town delayed-town))
    (if (numberp (aref (town-history town) (mod (1+ (ticks town)) (period town))))
        (mapcar 
         ;; While the history fills up, randomly wander, changing fashions 1/6 of the time
         #'(lambda (style-of-self)
             (if (true-with-probability 1/6)
                 (aref (styles town) (random (length (styles town))))
                 style-of-self))
         (population town))
        (let ((candidate-styles (survey town)))
          (mapcar 
           ;; After that, proceed as normal
           #'(lambda (style-of-self)
               (cond
                 ((true-with-probability (hipsterish-tendency town))
                  (car (nth
                        (random
                         (length
                          (cdr (assoc style-of-self candidate-styles))))
                        (cdr (assoc style-of-self candidate-styles)))))
                 ((true-with-probability 10/11) style-of-self)
                 (t (aref (styles town) (random (length (styles town)))))))
           (population town))))))

(defgeneric tick (town)
  (:documentation "Nondestructively returns the next iteration of the simulation.")
  (:method ((town town-snapshot))
    (make-instance 'town-snapshot
                   :time                (1+ (ticks town))
                   :population          (deconform town)
                   :styles              (styles town)
                   :hipsterish-tendency (hipsterish-tendency town))))
          
(defgeneric tick! (town)
  (:documentation "Destructively modifies a snapshot to become the next iteration of the simulation.")
  (:method ((town town-snapshot))
    (setf (population town) (deconform town))
    (incf (ticks town)))
  (:method :after ((town delayed-town))
    (setf (aref (town-history town) (mod (ticks town) (period town))) (population town))))

;;; Reporting methods

(defgeneric print-population (town &key stream limit specific-style)
  (:documentation "Prints out the first [limit] members in the population along with a generation number wearing the given style if non-nil.")
  (:method ((town town-snapshot) &key (stream t) limit specific-style)
    (with-accessors ((age ticks) (members population)) town
      (if specific-style
          (format stream "~&~3d: ~{~a~}~:[~;…~]" age (mapcar #'(lambda (a) (if (char-equal specific-style a)
                                                                                         specific-style
                                                                                         #\Space))
                                                                       (subseq members 0 limit)) limit)
          (format stream "~&~3d: ~{~a~}~:[~;…~]" age (subseq members 0 limit) limit)))))

(defgeneric print-popularity (town &key stream limit)
  (:documentation "Prints out the first [limit] styles and its percentage popularity, along with a genreation number.")
  (:method ((town town-snapshot) &key (stream t) limit)
    (with-accessors ((age ticks) (members population) (styles styles)) town
      (format stream "~&~3d:  " age)
      (loop for i across styles
            for j from 0 below (or limit (length styles))
            do (format stream "~20@<~a ~7,2f%~>" i (/ (count-if #'(lambda (a) (char-equal i a)) members)
                                                 (length members)
                                                 1/100))))))

;;; When seeing the development of the town, we need to repeat things a lot.
(defmacro with-town-iterator ((var town &key (times 20) (tick-time :before)) &body body)
  `(let ((,var ,town))
     (assert (typep ,var 'town-snapshot))
     (assert (typep ,tick-time '(member :before :after)))
     (assert (typep ,times '(integer 1)))
     (loop repeat ,times
           do (progn
                ,(if (eql tick-time :before) `(tick! ,var))
                ,@body
                ,(if (eql tick-time :after) `(tick! ,var))))
     ,var))
