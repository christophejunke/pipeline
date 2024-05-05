(defpackage :env (:use :cl))
(in-package :env)

(defgeneric environment-type (environment)
  (:method ((o cons))       (if (consp (first o)) :alist :plist))
  (:method ((o null))       :empty)
  (:method ((o hash-table)) :hash))

(defvar *attribute-fold* nil)

(defgeneric fold-attribute (attribute old-value new-value environment)
  (:method (_ old-value new-value environment)
    "Shadow previous value with new one"
    (declare (ignore old-value environment))
    new-value))

(declaim (inline %aconsp
                 %eref-alist 
                 %eref-plist
                 %eref-alist-set
                 %eref-plist-set
                 %eref-alist-pure
                 %eref-plist-pure
                 %fold-alist
                 %fold-plist))

(defun %aconsp (cons)
  (declare (type cons cons))
  (consp (first cons)))

;;;

(defun %eref-alist (e k d)
  (declare (type cons e))
  (block nil (cdr (or (assoc k e) (return d)))))

(defun %eref-alist-set (e k v)
  (declare (type cons e))  
  (let ((entry (member k e :key #'car)))
    (if entry
        (setf (cdr entry) v)
        (setf (cdr e) (acons k v (cdr e))))))

(defun %eref-alist-pure (e k v)
  (declare (type cons e))  
  (acons k v (remove k e :key #'car :count 1)))

(defun %map-alist (f alist)
  (loop for (k . v) in alist
        collect (cons k (funcall f k v))))

;;;

(defun %eref-plist (e k d)
  (declare (type cons e))  
  (getf e k d))

(defun %eref-plist-set (e k v)
  (declare (type cons e))
  (prog1 v
    (let ((entry (member k e)))
      (if entry
          (setf (second entry) v)
          (setf (cddr e) (list* k v (cddr e)))))))

(defun %eref-plist-pure (e k v)
  (declare (type cons e))
  (let ((entry (member k e)))
    (if entry
        (append (list k v)
                (ldiff e entry)
                (rest entry))
        (list* k v e))))

(defun %map-plist (f plist)
  (loop for (k v) on plist by #'cddr
        collect k
        collect (funcall f k v)))
;;;

(defgeneric eref (environment key &optional default)
  (:argument-precedence-order environment key)
  (:method ((e null) k &optional default)
    default)
  (:method ((e hash-table) k &optional d)
    (gethash k e d))
  (:method ((e cons) k &optional d)
    (if (%aconsp e)
        (%eref-alist e k d)
        (%eref-plist e k d))))

(defgeneric (setf eref) (value environment key)
  (:argument-precedence-order environment key value)
  (:method (v (e hash-table) k)
    (setf (gethash k e) v))
  (:method (v (e cons) k)
    (if (%aconsp e)
        (%eref-alist-set e k v)
        (%eref-plist-set e k v))))

(defgeneric augment (environment key value)
  (:argument-precedence-order environment key value)
  (:method ((e null) k v)
    (acons k v nil))
  (:method ((e hash-table) k v)
    (setf (gethash k e) v))
  (:method ((e cons) k v)
    (if (%aconsp e)
        (%eref-alist-pure e k v)
        (%eref-plist-pure e k v))))

(defun fold-env/aggregate (environment key value)
  (if environment
      (augment environment key value)
      (ecase *aggregate*
        (:plist (list key value))
        (:alist (acons key value nil))
        (:hash (let ((hash (make-hash-table)))
                 (prog1 hash
                   (setf (gethash key hash) value)))))))

(defgeneric map-env (environment function)
  (:argument-precedence-order environment function)
  (:method ((e null) f) nil)
  (:method ((e cons) f)
    (if (%aconsp e)
        (%map-alist f e)
        (%map-plist f e)))
  (:method ((h hash-table) f)
    (maphash (lambda (k v) (setf (gethash k h) (funcall f acc k v))) h)))

(fold-env (lambda (a k v) (list a k v)) '((:a . 3) (:b . 2)) :initial-value 3)
(fold-env (lambda (a k v) (list a k v)) '(:a 3 :b 2) :initial-value 3)
(fold-env (lambda (a k v) (fold-env/aggregate a k v)) '(:a 3 :b 2))
(fold-env (lambda (a k v) (fold-env/aggregate a k v)) '((:a 3) (:b 2)))

(defun combine-environments (old-env new-env &optional reducers)
  (flet ((trans (k o n e) (funcall (eref reducers k #'fold-attribute) o n e)))
    (declare (inline trans))
    (fold-env (lambda ()))
    
    )
  )

(defun combine-environments (old-env new-env &optional reducers)
  "Combine property lists w.r.t. reducers.

REDUCERS is either a plist of property names to reducer functions, or
a reducer function that is used for all properties.

A reducer function takes an old value O, a new value N and returns the
combined value of O and N. The special *ENVIRONMENT* variable is bound
to the current environment in a reducer function.

When an attribute is missing from REDUCERS, the default *REDUCER* is
tried. If all fails, the generic function ATTRIBUTE-REDUCER is
called. By default, *REDUCER* is NIL and the default method for
ATTRIBUTE-REDUCER is used, which simply let the new value shadows the
previous one. For example:

   (combine-environments '(:a 1 :b 2 :c 3)
                         '(:a 0 :b 20 :c 4)
                         (list :a #'list :b #'+))
   => (:A (1 0) :B 22 :C 4)

An attribute can appear multiple times in the environment: if so, its
consecutive values will be combined in the order of appearance.

   (combine-environments `()
                         `(:transform ,(scale 3 3)
                           :transform ,(move 1 1)))

   => (:TRANSFORM #S(TRANSFORM :SX 3 :SY 3 :TX 3 :TY 3))
"
  (flet ((combine (attribute old new *environment*)
           (if-let (reducer (typecase reducers
                              (function reducers)
                              (list (getf reducers attribute *reducer*))))
             (funcall reducer old new)
             (attribute-reducer attribute old new))))
    (declare (inline combine))
    (loop with env = (copy-seq old-env)
          for (attribute raw-value) on new-env by #'cddr
          for value = (maybe-resolve% env raw-value attribute)
          for old-value = (resolve-value env attribute)
          do (setf (getf env attribute)
                   (if old-value
                       (combine attribute old-value value env)
                       value))
          finally (return env))))
