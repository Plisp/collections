;;;; An AVL tree implementation.
;;;; A specialization of red-black trees, for lookup-intensive applications. AVL
;;;; trees can be faster than red-black trees because they are more strictly
;;;; height-balanced.

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

(defclass avl-tree (red-black-tree) ())

(defclass avl-tree-node (red-black-tree-node)
  ((%balance-factor :accessor balance-factor
                    :initform 0)))

;;; Internal utility functions

(defun %avl-tree/double-rotation-balance (node)
  (let ((left (left node))
        (right (right node)))
    (cond
      ((plusp (balance-factor node))
       (setf (balance-factor left) -1
             (balance-factor right) 0))
      ((zerop (balance-factor node))
       (setf (balance-factor left) 0
             (balance-factor right) 0))
      (t (setf (balance-factor left) 0
               (balance-factor right) 1)))
    (setf (balance-factor node) 0)
    node))

;;; Internal protocol

(defmethod rotate :after ((direction (eql :left)) (node avl-tree-node))
  (let ((left (left node)))
    (cond
      ((zerop (balance-factor node))
       (setf (balance-factor node) -1)
       (when (node-p left)
         (setf (balance-factor left) 1)))
      (t (setf (balance-factor node) 0
               (balance-factor left) 0)))
    node))

(defmethod rotate :after ((direction (eql :right)) (node avl-tree-node))
  (let ((left (left node)))
    (cond
      ((zerop (balance-factor node))
       (setf (balance-factor node) 1)
       (when (node-p left)
         (setf (balance-factor left) -1)))
      (t (setf (balance-factor node) 0
               (balance-factor left) 0)))
    node))

(defmethod rotate :after ((direction (eql :left/right)) (node avl-tree-node))
  (%avl-tree/double-rotation-balance node))

(defmethod rotate :after ((direction (eql :right/left)) (node avl-tree-node))
  (%avl-tree/double-rotation-balance node))

;;; User protocol
