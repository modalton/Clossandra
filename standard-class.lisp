(load "./canonicalize.lisp")
(load "./class-helpers.lisp")

(defmacro my-defclass (name direct-superclasses direct-slots &rest options)
  `(ensure-class ',name
    :direct-superclasses ,(canonicalize-direct-superclasses direct-superclasses)
    :direct-slots ,(canonicalize-direct-slots direct-slots)
    ,@(canonicalize-defclass-options options)
    ))

;;; The standard metaobject. All inputs will be canonicalized into this form.
;;; Class metaobjects are instances of standard-class
(my-defclass my-standard-class ()
  ((name
    :initarg :name
    :accessor class-name)
   (direct-slots
    :accessor class-direct-slots)
   (direct-superclasses
    :initarg :direct-superclasses
    :accessor class-direct-superclasses)
   (effective-slots
    :accessor class-slots)
   (direct-subclasses
    :initform ()
    :accesor class-direct-subclasses)
   (direct-methods
    :initform ()
    :accessor class-direct-methods)))
  

