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
   #:make-tree
   #:max
   #:min
   #:next
   #:node
   #:previous
   #:valid-p
   #:walk))

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

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
  ((%tree :accessor tree
          :initarg :tree)
   (%data :accessor data
          :initarg :data)
   (%parent :accessor parent
            :initform nil)))

(defun make-tree (type &key (key #'identity) (test #'<))
  (if (and (not (eq type 'tree))
           (subtypep type 'tree))
      (make-instance type :key key :test test)
      (error "Unknown tree type: ~s." type)))

(defun make-node (tree item &rest args)
  (let* ((class-name (class-name (class-of tree)))
         (type (a:format-symbol (symbol-package class-name) "~a-NODE"
                                class-name)))
    (apply #'make-instance type :tree tree :data item args)))

;;; Internal utility functions

(defun node-p (node)
  (unless (or (not (typep node 'node))
              (eq node (sentinel (tree node))))
    node))

;;; Internal protocol

(defgeneric transplant (node1 node2))

(defgeneric rotate (direction node))

;;; User protocol

(defgeneric valid-p (tree))

(defgeneric walk (tree func &key order)
  (:method ((tree tree) func &key (order :in))
    (check-type func function)
    (a:when-let ((node (root tree)))
      (walk node func :order order))))

(defgeneric find (tree item))

(defgeneric insert (tree item)
  (:method ((tree tree) item)
    (let ((node (make-node tree item)))
      (insert tree node))))

(defgeneric delete (tree item)
  (:method ((tree tree) item)
    (a:when-let ((node (node-p (nth-value 1 (find tree item)))))
      (delete tree node))))

(defgeneric min (tree)
  (:method ((tree tree))
    (a:when-let ((node (root tree)))
      (min node))))

(defgeneric max (tree)
  (:method ((tree tree))
    (a:when-let ((node (root tree)))
      (max node))))

(defgeneric previous (node))

(defgeneric next (node))
