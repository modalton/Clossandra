(load "./helper.lisp")

;;; Convert slot's name into :name property
;;; Convert :initform option -> :initargs property
;;; Seperate :accessor options into :reader & :writer options, collect multiple 
(defun canonicalize-direct-slot (spec)
  (if (symbolp spec)                    ; if symbol
      `(list :name ',spec)              ; return list -> :name _
      (let ((name (car spec))           ; else
            (initfunction nil)
            (initform nil)
            (initargs ())
            (readers ())
            (writers ())
            (other-options ()))
        (do ((olist (cdr spec) (cddr olist)))  ; Go through spec unitl null, if keyform matchespush list in obj-list 
            ((null olist))     
          (case (car olist)   
            (:initform (setq initfunction `(function (lambda () ,(cadr olist))))
                       (setq initform `',(cadr olist)))
            (:initarg (push-on-end (cadr olist) initargs))
            (:reader (push-on-end (cadr olist) readers))
            (:writer (push-on-end (cadr olist) writers))
            (:accesor (push-on-end (cadr olist) writers)
                      (push-on-end `(setf ,(cadr olist)) writers))
            (otherwise (push-on-end `',(car olist) other-options)
                       (push-on-end `',(cadr olist) other-options))))
        `(list
          :name ',name
          ,@(when initfunction
              `(:initform ,initform
                :initfunction ,initfunction))
          ,@(when initargs `(:initargs ,initargs))
          ,@(when readers `(:readers ,readers))
          ,@(when writers `(:writers ,writers))
          ,@other-options
             )
        )))

(defun canonicalize-direct-slots (direct-slots)
  `(list ,@(mapcar #'canonicalize-direct-slot direct-slots)))

(defun canonicalize-direct-superclass (class-name)
  `(find-class ',class-name))

(defun canonicalize-direct-superclasses (direct-superclasses)
  `(list ,@(mapcar #'canonicalize-direct-superclass direct-superclasses)))

(defun canonicalize-defclass-option (option)
  (case (car option)
    (:metaclass
     (list ':metaclass `(find-class ',(cadr option))))
    (:default-initargs
     (list
      ':direct-default-initargs
      `(list ,@(mapappend #'(lambda (x) x)
                        (mapplist
                         #'(lambda (key value)
                             `(',key ,value))
                         (cdr option))))))
    (t (list `',(car option) `',(cadr option)))))


(defun canonicalize-defclass-options (options)
  (mapappend #'canonicalize-defclass-option options))
