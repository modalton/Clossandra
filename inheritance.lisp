(load "./helper.lisp")

(defun collect-superclasses* (class)
  (remove-duplicates
   (cons class (mapappend #'collect-superclasses* (class-direct-superclasses class)))))

(defun compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                       (mapappend #'local-precedence-ordering classes-to-order))
                      #'std-tie-breaker-rule)))

(defun compute-slots (class)
  (mapcar #'(lambda (slot)
              (make-effective-slot-definition
               :name (slot-definition-name slot)
               :initform (slot-definition-initform slot)
               :initfunction (slot-definition-initfunction slot)
               :initargs (slot-definition-initargs slot)))
          (remove-duplicates
           (mapappend #'class-direct-slots (class-precedence-list class))
           :key #'slot-definition-name :from-end t))) 

(defun finalize-inheritance (class)
  (setf (class-precedence-list class) (compute-class-precedence-list class))
  (setf (class-slots class) (compute-slots class))
  (values))
