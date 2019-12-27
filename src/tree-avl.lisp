;;;; An AVL tree implementation.
;;;; A specialization of red-black trees, for lookup-intensive applications. AVL
;;;; trees can be faster than red-black trees because they are more strictly
;;;; height-balanced.

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
                   (let ((parx (parent node))
                         (new-root (if (plusp (balance-factor child))
                                       (rotate :left/right node)
                                       (rotate :right node))))
                     (setf (parent new-root) parx)
                     (if (node-p parx) ; rotation immediately restores invariants
                         (return)
                         (return new-root)))))
                ;; symmetric case for child = (right node)
                (ecase (incf (balance-factor node))
                  (0 (return))
                  (+1 (setf child node))
                  (+2
                   (let ((parx (parent node))
                         (new-root (if (minusp (balance-factor child))
                                       (rotate :right/left node)
                                       (rotate :left node))))
                     (setf (parent new-root) parx)
                     (if (node-p parx)
                         (return)
                         (return new-root))))))))

(defun %avl-tree/delete (tree node) ;TODO clean up tomorrow
  (if (and (node-p (left node)) (node-p (right node)))
      (let ((replacement (nth-value 1 (min (right node)))))
        (setf (data node) (data replacement))
        (%avl-tree/delete tree replacement))
      (let (sroot direction)
        (cond ((node-p (left node))
               (setf sroot (left node))
               (transplant node sroot)
               (setf direction (if (eq sroot (left (parent sroot)))
                                   :left
                                   :right)))
              ((node-p (right node))
               (setf sroot (right node))
               (transplant node sroot)
               (setf direction (if (eq sroot (left (parent sroot)))
                                   :left
                                   :right)))
              (t ; sentinel children - (left (parent node)) points to sentinel
               (assert (eq (left node) (right node)))
               (assert (eq (left node) (sentinel tree)))
               (setf sroot (left node))
               (setf direction (if (eq node (left (parent node)))
                                   :left
                                   :right))
               (transplant node (if (eq node (left (parent node)))
                                    (left node)
                                    (right node)))))
        (%avl-tree/deletion-rebalance tree sroot direction))))

(defun %avl-tree/deletion-rebalance (tree n direction)
  "TODO oh god. This needs to be cleaned up hard. At least it works."
  (loop :with parx
        :with z and z-balance-factor
        :for first-time = t then nil
        :for x = (parent n) then parx
        :while (node-p x)
        :do (setf parx (parent x))
            (if (and (or (not first-time) (and first-time (eq direction :left)))
                     (eq n (left x)))
                (ecase (incf (balance-factor x))
                  (0 (setf n x))
                  (+1 (return))
                  (+2
                   (let ((x-was-left-child (eq x (left parx))))
                     (setf z (right x)
                           z-balance-factor (balance-factor z))
                     (if (< z-balance-factor 0)
                         (setf n (rotate :right/left x))
                         (setf n (rotate :left x)))
                     (setf (parent n) parx)
                     (if (node-p parx)
                         (progn
                           (if x-was-left-child
                               (setf (left parx) n)
                               (setf (right parx) n))
                           (when (zerop z-balance-factor)
                             (return)))
                         (setf (root tree) n)))))
                (ecase (decf (balance-factor x))
                  (0 (setf n x))
                  (-1 (return))
                  (-2
                   (let ((x-was-left-child (eq x (left parx))))
                     (setf z (left x)
                           z-balance-factor (balance-factor z))
                     (if (> z-balance-factor 0)
                         (setf n (rotate :left/right x))
                         (setf n (rotate :right x)))
                     (setf (parent n) parx)
                     (if (node-p parx)
                         (progn
                           (if x-was-left-child
                               (setf (left parx) n)
                               (setf (right parx) n))
                           (when (zerop z-balance-factor)
                             (return)))
                         (setf (root tree) n))))))))

(defparameter *test* (make-tree 'avl-tree))
(insert *test* (make-node *test* 91))
(insert *test* (make-node *test* 73))
(insert *test* (make-node *test* 32))
(insert *test* (make-node *test* 54))
(insert *test* (make-node *test* 16))
(insert *test* (make-node *test* 81))
(insert *test* (make-node *test* 24))

;; (delete *test* 3) (print (tree-to-list (root *test*)))
;; (delete *test* 28)

(defun tree-to-list (root)
  "Returns the binary tree as a list."
  (cond ((not (node-p root)) :sentinel)
        (t
         (list (format nil "value:~d b-factor:~d" (data root) (balance-factor root))
               (tree-to-list (left root))
               (tree-to-list (right root))))))

;; (loop :with test = (make-tree 'avl-tree)
;;       :with nums
;;       :for random = (random 100)
;;       :repeat 1000
;;       :do (push random nums)
;;           (insert test (make-node test random))
;;           (unless (%avl-tree/check-invariants test)
;;             (return (values test t)))
;;       finally (loop :named inner ; deletion
;;                     :with random-nums = (a:shuffle nums)
;;                     :for n = (pop random-nums)
;;                     :while n
;;                     :do (delete test n)
;;                         (unless (%avl-tree/check-invariants test)
;;                           (return (values test t))))
;;               (return (values test nil)))

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
    (if (= (balance-factor b) 0)
        (progn
          (setf (balance-factor b) -1
                (balance-factor node) +1))
        (progn
          (setf (balance-factor b) 0
                (balance-factor node) 0)))
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
    (if (= (balance-factor b) 0)
        (progn
          (setf (balance-factor b) +1
                (balance-factor node) -1))
        (progn
          (setf (balance-factor b) 0
                (balance-factor node) 0)))
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
  (let ((new-root (%avl-tree/insertion-rebalance node)))
    (when new-root
      (setf (root tree) new-root))))

(defmethod delete ((tree avl-tree) (node avl-tree-node))
  (%avl-tree/delete tree node)
  node)
