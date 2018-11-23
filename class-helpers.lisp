(load "./initialize.lisp")

(let ((class-table (make-hash-table :test #'eq)))
  (defun find-class (symbol &optional (errorp t))
    (let ((class (gethash symbol class-table nil)))
      (if (and (null class) errorp)
          (error "No class name ~S." symbol)
          class)))

  (defun (setf find-class) (new-value symbol)
    (setf (gethash symbol class-table) new-value))

  )

;;; Takes name & desc of class as keyword arguments defines class. Call's make-instance
;;; to create & initialize metaobject (standard-class). 
(defun ensure-class (name &rest all-keys)
  (if (find-class name nil)
      (error "Can't redefine the class named. ~S." name)
      (let ((class (apply #'make-instance 'standard-class :name name all-keys)))
        (setf (find-class name) class)
        class)))
