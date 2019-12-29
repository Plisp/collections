;;;; An AVL tree implementation.
;;;; A specialization of a binary search tree, for lookup-intensive
;;;; applications. AVL trees can be faster than red-black trees because they are
;;;; more strictly height-balanced.

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

(defclass avl-tree (binary-search-tree) ())

(defclass avl-tree-node (binary-search-tree-node)
  ((%height :accessor height
            :initform 0) ; TODO factor these into insertion/deletion and rotations
   (%balance-factor :accessor balance-factor
                    :initform 0
                    :type (integer -2 2))))

(defmethod initialize-instance :after ((instance avl-tree) &key)
  (let ((sentinel (make-node instance nil)))
    (setf (sentinel instance) sentinel
          (root instance) sentinel)))

(defmethod initialize-instance :after ((instance avl-tree-node) &key)
  (setf (left instance) (sentinel (tree instance))
        (right instance) (sentinel (tree instance))))

;;; Internal utility functions

(defun %avl-tree/check-invariants (tree)
  (labels ((recur (node)
             (cond ((not (node-p node)) 0)
                   (t (let ((left-height (recur (left node)))
                            (right-height (recur (right node))))
                        (if (or (null left-height) (null right-height)
                                (/= (+ left-height (balance-factor node)) right-height))
                            nil
                            (1+ (cl:max left-height right-height))))))))
    (recur (root tree))))

(defun %avl-tree/insertion-rebalance (new)
  "NEW is the inserted node. NEW-ROOT is set to the new roots of rotated subtrees
(and possibly the new actual tree root, which is returned (otherwise, NIL
is returned, indicating rebalancing stopped before reaching the root))."
  (loop :with child = new
        :for node = (parent child)
        :while (node-p node)
        :do (if (eq child (left node))
                (ecase (decf (balance-factor node))
                  (0 (return))
                  (-1 (setf child node))
                  (-2 ;   V  V parent of new-root's subtree after rotation
                   (let ((node-parent (parent node))
                         (new-root (if (= (balance-factor child) +1)
                                       (rotate :left/right node)
                                       (rotate :right node))))
                     (setf (parent new-root) node-parent)
                     (if (node-p node-parent) ; rotation immediately restores invariants
                         (return)
                         (return new-root)))))
                ;; symmetric case for child = (right node)
                (ecase (incf (balance-factor node))
                  (0 (return))
                  (+1 (setf child node))
                  (+2
                   (let ((node-parent (parent node))
                         (new-root (if (= (balance-factor child) -1)
                                       (rotate :right/left node)
                                       (rotate :left node))))
                     (setf (parent new-root) node-parent)
                     (if (node-p node-parent)
                         (return)
                         (return new-root))))))))

(defun %avl-tree/delete (node)
  (if (and (node-p (left node)) (node-p (right node)))
      (let ((replacement (min (right node))))
        (setf (data node) (data replacement))
        (%avl-tree/delete replacement))
      (let ((direction (if (eq node (left (parent node)))
                           :left
                           :right)))
        (cond ((node-p (left node))
               (transplant node (left node))
               (%avl-tree/deletion-rebalance (left node) direction))
              ((node-p (right node))
               (transplant node (right node))
               (%avl-tree/deletion-rebalance (right node) direction))
              (t ; node has both sentinel children - arbitrarily transplant left
               (transplant node (left node))
               (%avl-tree/deletion-rebalance (left node) direction))))))

(defun %avl-tree/deletion-rebalance (new-root direction)
  "NEW-ROOT is the new root of the subtree that was rooted by the deleted node.
DIRECTION is necessary to treat the case in which NODE is the sentinel,
specifying on which side of NODE's parent the deleted node was located so we can
adjust the balance factor appropriately."
  (loop :for first-time = t then nil
        :with child = new-root
        :for node = (parent child)
        :while (node-p node)
        :do (if (and (or (not first-time) (and first-time (eq direction :left)))
                     (eq child (left node)))
                (ecase (incf (balance-factor node))
                  (0 (setf child node))
                  (+1 (return))
                  (+2
                   (let ((node-parent (parent node))
                         (right-child (right node)))
                     (if (= (balance-factor right-child) -1)
                         (setf child (rotate :right/left node))
                         (setf child (rotate :left node)))
                     (setf (parent child) node-parent)
                     (cond ((not (node-p node-parent))
                            (return child))
                           ((= (balance-factor right-child) -1)
                            (return))))))
                ;; symmetric case for child = (right node)
                (ecase (decf (balance-factor node))
                  (0 (setf child node))
                  (-1 (return))
                  (-2
                   (let ((node-parent (parent node))
                         (left-child (left node)))
                     (if (= (balance-factor left-child) +1)
                         (setf child (rotate :left/right node))
                         (setf child (rotate :right node)))
                     (setf (parent child) node-parent)
                     (cond ((not (node-p node-parent))
                            (return child))
                           ((= (balance-factor left-child) +1)
                            (return)))))))))

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

(defmethod rotate ((direction (eql :left/right)) (node avl-tree-node))
  (let* ((z (left node))
         (new-root (right z)) ; will be rotated to the root of this subtree
         (new-root-balance (balance-factor new-root)))
    (rotate :left z)
    (rotate :right node)
    (case new-root-balance
      (-1
       (setf (balance-factor node) +1
             (balance-factor z) 0))
      (0
       (setf (balance-factor node) 0
             (balance-factor z) 0))
      (+1
       (setf (balance-factor node) 0
             (balance-factor z) -1)))
    (setf (balance-factor new-root) 0)
    new-root))

(defmethod rotate ((direction (eql :right/left)) (node avl-tree-node))
  (let* ((z (right node))
         (new-root (left z)) ; will be rotated to the root of this subtree
         (new-root-balance (balance-factor new-root)))
    (rotate :right z)
    (rotate :left node)
    (case new-root-balance
      (-1
       (setf (balance-factor node) 0
             (balance-factor z) +1))
      (0
       (setf (balance-factor node) 0
             (balance-factor z) 0))
      (+1
       (setf (balance-factor node) -1
             (balance-factor z) 0)))
    (setf (balance-factor new-root) 0)
    new-root))

;;; User protocol

(defmethod insert :after ((tree avl-tree) (node avl-tree-node))
  (a:when-let ((new-root (%avl-tree/insertion-rebalance node)))
    (setf (root tree) new-root)))

(defmethod delete ((tree avl-tree) (node avl-tree-node))
  (a:when-let ((new-root (%avl-tree/delete node)))
    (setf (root tree) new-root))
  (setf (parent (sentinel tree)) nil)
  node)
