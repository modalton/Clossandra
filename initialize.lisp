;;; After method on my-class to initialize overhead (default value for direct superclass, inheritance, etc) 
(defmethod initialize-instance :after
    ((class standard-class) &key direct-superclasses direct-slots)
  (let ((supers (or direct-superclasses (list (find-class 'standard-object)))))
    (setf (class-direct-superclasses class) supers)
    (dolist (superclass supers)
      (push class (class-direct-subclasses superclass))))
  (let ((slots (mapcar #'(lambda (slot-properties) 
                           (apply #'make-direct-slot-definition slot-properties)) direct-slots)))
    (setf (class-direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (slot-definition-readers direct-slot))
        (add-reader-method
         class reader (slot-definition-name direct-slot)))
      (dolist (writer (slot-definition-writers direct-slot))
        (add-writer-method
         class writer (slot-definition-name direct-slot)))))
  (finalize-inheritance class))
