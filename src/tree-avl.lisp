;;;; An AVL tree implementation.
;;;; A specialization of a binary search tree, for lookup-intensive
;;;; applications. AVL trees can be faster than red-black trees because they are
;;;; more strictly height-balanced.

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

(defclass avl-tree (binary-search-tree) ())

(defclass avl-tree-node (binary-search-tree-node)
  ((%balance-factor :accessor balance-factor
                    :initform 0)))

;;; Internal utility functions

(defun %avl-tree/balance (new)
  (loop :with z = new
        :with parx
        :with new-root
        :for x = (parent z) :then (parent z)
        :while (node-p x)
        :do (tagbody
               (if (eq z (right x))
                   (if (plusp (balance-factor x))
                       (progn
                         (setf parx (parent x))
                         (if (minusp (balance-factor z))
                             ;; XXX really should use double rotation here v
                             (let ((bal-lz (balance-factor (left z))))
                               (rotate :right z)
                               (setf new-root (rotate :left x)) ; new root was (left z)
                               (case bal-lz
                                 (-1 (setf (balance-factor new-root) 0
                                           (balance-factor x) 0
                                           (balance-factor z) +1))
                                 (0 (setf (balance-factor new-root) 0
                                          (balance-factor x) 0
                                          (balance-factor z) 0))
                                 (+1 (setf (balance-factor new-root) 0
                                           (balance-factor x) -1
                                           (balance-factor z) 0))))
                             (setf new-root (rotate :left x))))
                       (progn
                         (when (minusp (balance-factor x))
                           (setf (balance-factor x) 0)
                           (return))
                         (incf (balance-factor x))
                         (setf z x)
                         (go upwards)))
                   (if (minusp (balance-factor x))
                       (progn
                         (setf parx (parent x))
                         (if (plusp (balance-factor z))
                             ;; save factor of (right z) before rotation and fix afterwards
                             (let ((bal-rz (balance-factor (right z))))
                               (rotate :left z)
                               (setf new-root (rotate :right x))
                               (case bal-rz
                                 (-1 (setf (balance-factor new-root) 0
                                           (balance-factor x) +1
                                           (balance-factor z) 0))
                                 (0 (setf (balance-factor new-root) 0
                                          (balance-factor x) 0
                                          (balance-factor z) 0))
                                 (1 (setf (balance-factor new-root) 0
                                          (balance-factor x) 0
                                          (balance-factor z) -1))))
                             (setf new-root (rotate :right x))))
                       (progn
                         (when (plusp (balance-factor x))
                           (setf (balance-factor x) 0)
                           (return))
                         (decf (balance-factor x))
                         (setf z x)
                         (go upwards))))
               (setf (parent new-root) parx)
               (if (node-p parx)
                   (return)
                   (return new-root))
             upwards)))

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
    (if (zerop (balance-factor b))
        (setf (balance-factor b) -1
              (balance-factor node) +1)
        (setf (balance-factor b) 0
              (balance-factor node) 0))
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
    (if (zerop (balance-factor b))
        (setf (balance-factor b) +1
              (balance-factor node) -1)
        (setf (balance-factor b) 0
              (balance-factor node) 0))
    b))

;;; User protocol

(defmethod insert :after ((tree avl-tree) (node avl-tree-node))
  (a:when-let ((new-root (%avl-tree/balance node)))
    (setf (root tree) new-root))
  node)
