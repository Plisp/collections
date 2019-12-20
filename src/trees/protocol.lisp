(in-package #:cl-user)

(defpackage #:cl.lisp.collections.tree
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:max
   #:min)
  ;; tree types
  (:export
   #:binary-search-tree
   #:red-black-tree)
  ;; API
  (:export
   #:delete
   #:find
   #:insert
   #:make-node
   #:make-tree
   #:max
   #:min
   #:next
   #:node
   #:previous
   #:valid-p
   #:walk))

(in-package #:cl.lisp.collections.tree)

(defclass tree ()
  ((%sentinel :accessor sentinel
              :initform nil)
   (%root :accessor root
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
    (let* ((class-name (class-name (class-of tree)))
           (type (a:format-symbol (symbol-package class-name) "~a-NODE"
                                  class-name)))
      (apply #'make-instance type :tree tree :data item args))))

(defgeneric node-p (node)
  (:method ((node null))
    nil)
  (:method ((node node))
    node))

(defgeneric transplant (node1 node2)
  (:method ((node1 node) node2)
    (error "Not implemented.")))

(defgeneric rotate (direction node)
  (:method (direction (node node))
    (error "Not implemented.")))

(defgeneric valid-p (tree)
  (:method ((tree tree))
    (error "Not implemented.")))

(defgeneric walk (tree func)
  (:method ((tree tree) func)
    (error "Not implemented."))
  (:method :before ((tree tree) func)
    (check-type func function)))

(defgeneric find (tree item)
  (:method ((tree tree) item)
    (error "Not implemented.")))

(defgeneric insert (tree item)
  (:method ((tree tree) item)
    (error "Not implemented.")))

(defgeneric delete (tree item)
  (:method ((tree tree) item)
    (error "Not implemented.")))

(defgeneric delete/fix (tree node)
  (:method ((tree tree) (node node))
    (error "Not implemented.")))

(defgeneric min (tree)
  (:method ((tree tree))
    (error "Not implemented.")))

(defgeneric max (tree)
  (:method ((tree tree))
    (error "Not implemented.")))

(defgeneric previous (node)
  (:method ((node node))
    (error "Not implemented.")))

(defgeneric next (node)
  (:method ((node node))
    (error "Not implemented.")))
