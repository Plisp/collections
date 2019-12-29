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
   #:avl-tree
   #:binary-search-tree
   #:red-black-tree
   #:splay-tree)
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
   (%item-type :reader item-type
               :initarg :item-type)
   (%key :reader key
         :initarg :key)
   (%sorter :reader sorter
            :initarg :sorter)
   (%hash-test :reader hash-test
               :initarg :hash-test)))

(defclass node ()
  ((%tree :accessor tree
          :initarg :tree)
   (%key :reader key
         :initarg :key)
   (%data :accessor data
          :initarg :data)
   (%parent :accessor parent
            :initform nil)))

(defun make-tree (type &key item-type (key #'identity) (sort #'<)
                         (hash-test #'eql))
  (unless item-type
    (error "Must specify :ITEM-TYPE denoting the type of items stored in the ~
            tree."))
  (if (and (not (eq type 'tree))
           (subtypep type 'tree))
      (make-instance type
                     :item-type item-type
                     :key key
                     :sorter sort
                     :hash-test hash-test)
      (error "Unknown tree type: ~s." type)))

(defun make-node (tree item &rest args)
  (let* ((class-name (class-name (class-of tree)))
         (type (a:format-symbol (symbol-package class-name) "~a-NODE"
                                class-name))
         (key (funcall (key tree) item)))
    (apply #'make-instance type
           :tree tree
           :key key
           :data (u:dict (hash-test tree) item item)
           args)))

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
    (a:when-let ((node (node-p (root tree))))
      (walk node func :order order))))

(defgeneric find (tree item))

(defgeneric insert (tree item)
  (:method ((tree tree) item)
    (a:if-let ((node (node-p (nth-value 1 (find tree item)))))
      (progn
        (setf (u:href (data node) item) item)
        node)
      (let ((node (make-node tree item)))
        (insert tree node)
        node))))

(defgeneric delete (tree item)
  (:method ((tree tree) item)
    (a:when-let ((node (node-p (nth-value 1 (find tree item)))))
      (delete tree node))))

(defgeneric delete (tree item)
  (:method ((tree tree) item)
    (a:when-let* ((node (node-p (nth-value 1 (find tree item))))
                  (data (data node)))
      (if (<= (hash-table-count data) 1)
          (delete tree node)
          (remhash item data)))))

(defgeneric min (tree)
  (:method ((tree tree))
    (a:when-let ((node (node-p (root tree))))
      (min node))))

(defgeneric max (tree)
  (:method ((tree tree))
    (a:when-let ((node (node-p (root tree))))
      (max node))))

(defgeneric previous (node))

(defgeneric next (node))
