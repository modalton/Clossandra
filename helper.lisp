(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;;; Like mapcar but appending results
(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

;;; like mapcar for property lists (:key value?). Hence funcall vs apply
(defun mapplist (fun x)
  (if (null x)
      ()
      (cons (funcall fun (car x ) (cadr x))
            (mapplist fun (cddr x)))))

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (loop
       (let ((minimal-elements
              (remove-if
               #'(lambda (class)
                   (member class remaining-constraints :key #'cadr))
               remaining-elements)))
         (when (null minimal-elements)
           (if (null remaining-elements)
               (return-from topological-sort result)
               (error "Inconsistent precedence graph")))
         (let ((choice (if (null (cdr minimal-elements))
                           (car minimal-elements)
                           (funcall tie-breaker
                                    minimal-elements
                                    result))))
           (setq result (append result (list choice)))
           (setq remaining-elements
                 (remove choice remaining-elements))
           (setq remaining-constraints
                 (remove choice remaining-constraints :test #'member)))))))
