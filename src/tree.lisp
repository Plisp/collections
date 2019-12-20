(in-package #:cl-user)

(defpackage #:cl.lisp.collections.tree
  (:local-nicknames (#:a #:alexandria))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:max
   #:min)
  (:export
   #:delete
   #:find
   #:insert
   #:make-tree
   #:max
   #:min
   #:next
   #:previous
   #:valid-p
   #:walk))

(in-package #:cl.lisp.collections.tree)

(defclass tree ()
  ((%root :accessor root
          :initform nil)
   (%key :reader key
         :initarg :key)
   (%test :reader test
          :initarg :test)))

(defclass node ()
  ((%tree :reader tree
          :initarg :tree)
   (%data :accessor data
          :initarg :data)
   (%parent :accessor parent
            :initform nil)))

(defgeneric make-tree (type &key)
  (:method (type &key (key #'identity) (test #'<))
    (if (and (not (eq type 'tree))
             (subtypep type 'tree))
        (make-instance type :key key :test test)
        (error "Unknown tree type: ~s." type))))

(defgeneric make-node (tree item &rest args)
  (:method (tree item &rest args)
    (let ((type (a:symbolicate (class-name (class-of tree)) '#:-node)))
      (apply #'make-instance type :tree tree :data item args))))

(defgeneric valid-p (tree)
  (:method (tree)
    (error "Not implemented.")))

(defgeneric walk (tree func)
  (:method (tree func)
    (error "Not implemented."))
  (:method :before ((tree tree) func)
    (check-type func function)))

(defgeneric find (tree item)
  (:method (tree item)
    (error "Not implemented.")))

(defgeneric insert (tree item)
  (:method (tree item)
    (error "Not implemented.")))

(defgeneric delete (tree item)
  (:method (tree item)
    (error "Not implemented.")))

(defgeneric min (tree)
  (:method (tree)
    (error "Not implemented.")))

(defgeneric max (tree)
  (:method (tree)
    (error "Not implemented.")))

(defgeneric previous (node)
  (:method (node)
    (error "Not implemented.")))

(defgeneric next (node)
  (:method (node)
    (error "Not implemented.")))
