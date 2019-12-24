;;;; An AVL tree implementation.
;;;; A specialization of red-black trees, for lookup-intensive applications. AVL
;;;; trees can be faster than red-black trees because they are more strictly
;;;; height-balanced.

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

(defclass avl-tree (binary-search-tree) ())

(defclass avl-tree-node (binary-search-tree-node)
  ((%height :accessor height
            :initform 0)
   (%balance-factor :accessor balance-factor
                    :initform 0)))

;;; Internal utility functions

(defun %avl-tree/update (node)
  (let ((left-height -1)
        (right-height -1))
    (when (node-p (left node))
      (setf left-height (height (left node))))
    (when (node-p (right node))
      (setf right-height (height (right node))))
    (setf (height node) (1+ (cl:max left-height right-height))
          (balance-factor node) (- right-height left-height))))

(defun %avl-tree/balance (node)
  (case (balance-factor node)
    (-2 (if (<= (balance-factor (left node)) 0)
            (rotate :right node)
            (progn
              (setf (left node) (rotate :left (left node)))
              (rotate :right node))))
    (2 (if (>= (balance-factor (right node)) 0)
           (rotate :left node)
           (progn
             (setf (right node) (rotate :right (right node)))
             (rotate :left node))))
    (t node)))

;;; Internal protocol

(defmethod rotate ((direction (eql :left)) (node avl-tree-node))
  (let ((p (parent node))
        (b (right node)))
    (setf (right node) (left b))
    (when (node-p (left b))
      (setf (parent (left b)) node))
    (setf (left b) node
          (parent node) b
          (parent b) p)
    (when (node-p p)
      (if (eq (right p) node)
          (setf (right p) b)
          (setf (left p) b)))
    (%avl-tree/update node)
    (%avl-tree/update b)
    b))

(defmethod rotate ((direction (eql :right)) (node avl-tree-node))
  (let ((p (parent node))
        (b (left node)))
    (setf (left node) (right b))
    (when (node-p (right b))
      (setf (parent (right b)) node))
    (setf (right b) node
          (parent node) b
          (parent b) p)
    (when (node-p p)
      (if (eq (left p) node)
          (setf (left p) b)
          (setf (right p) b)))
    (%avl-tree/update node)
    (%avl-tree/update b)
    b))

;;; User protocol

(defmethod insert :after ((tree avl-tree) (node avl-tree-node))
  (%avl-tree/update (parent node))
  (%avl-tree/balance (parent node))
  node)
