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
(and possibly the new actual tree root, which is returned).
TODO BALANCE-FACTORs are never updated to (+/-)2 - instead the code takes these
into account silently and performs rotation(s) without updating."
  (loop :with z = new
        :with parx
        :with new-root
        :for x = (parent z)
        :while (node-p x)
        :do (if (eq z (left x))
                (ecase (balance-factor x)
                  (+1
                   (decf (balance-factor x))
                   (return))
                  (0
                   (decf (balance-factor x))
                   (setf z x))
                  (-1 ;XXX
                   (setf parx (parent x))
                   (if (plusp (balance-factor z)) ;XXX
                       (setf new-root (rotate :left/right x))
                       (setf new-root (rotate :right x)))
                   (setf (parent new-root) parx)
                   (if (node-p parx)
                       (return)
                       (return new-root))))
                ;; symmetric case for z = (right x)
                (ecase (balance-factor x)
                  (-1
                   (incf (balance-factor x))
                   (return))
                  (0
                   (incf (balance-factor x))
                   (setf z x))
                  (+1
                   (setf parx (parent x))
                   (if (minusp (balance-factor z))
                       (setf new-root (rotate :right/left x))
                       (setf new-root (rotate :left x)))
                   (setf (parent new-root) parx)
                   (if (node-p parx)
                       (return)
                       (return new-root)))))))

(defun %avl-tree/delete (tree node)
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
                                   :right))
               (printv:printv (data sroot) (data (parent sroot))))
              (t ; sentinel children - (left (parent node)) points to sentinel
               (assert (eq (left node) (right node)))
               (assert (eq (left node) (sentinel tree)))
               (setf sroot (left node))
               (setf direction (if (eq node (left (parent node)))
                                   :left
                                   (printv:printv :right)))
               (transplant node (if (eq node (left (parent node)))
                                    (left node)
                                    (right node)))))
        (when (node-p (root tree))
          (%avl-tree/deletion-rebalance tree sroot direction)))))

(defun %avl-tree/deletion-rebalance (tree n direction)
  "TODO see insertion - same clarity issue here."
  (loop :with parx
        :with z and z-balance-factor
        :for first-time = t then nil
        :for x = (printv:printv (data (parent n)) (parent n)) then parx
        :while (node-p x)
        :do (setf parx (printv:printv (data (parent x)) (parent x)))
            (if (and (or (not first-time) (and first-time (eq direction :left)))
                     (printv:printv (eq n (printv:printv (data (left x)) (left x)))))
                (ecase (printv:printv (balance-factor x))
                  (+1
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
                         (setf (root tree) n))))
                  (0
                   (incf (balance-factor x))
                   (return))
                  (-1
                   (setf n x)
                   (printv:printv (incf (balance-factor x)))))
                (ecase (balance-factor x)
                  (-1
                   (let ((x-was-left-child (eq x (left parx))))
                    (setf z (left x)
                          z-balance-factor (printv:printv (data z) (balance-factor z)))
                    (printv:printv (tree-to-list (left x)))
                    (if (> z-balance-factor 0)
                        (setf n (rotate :left/right x))
                        (setf n (rotate :right x)))
                    (printv:printv (tree-to-list (root tree)))
                    (setf (parent n) (printv:printv (data parx) (balance-factor z)
                                                    parx))
                    (if (node-p parx)
                        (progn
                          (if x-was-left-child
                              (setf (left parx) n)
                              (setf (right parx) n))
                          (when (zerop z-balance-factor)
                            (return)))
                        (setf (root tree) n))
                    (printv:printv (tree-to-list (root tree)))))
                  (0
                   (decf (balance-factor x))
                   (return))
                  (+1
                   (setf n x)
                   (decf (balance-factor x)))))))

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
;;       :repeat 500
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
