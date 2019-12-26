;;;; An AVL tree implementation.
;;;; A specialization of red-black trees, for lookup-intensive applications. AVL
;;;; trees can be faster than red-black trees because they are more strictly
;;;; height-balanced.

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

(defclass avl-tree (binary-search-tree) ())

(defclass avl-tree-node (binary-search-tree-node)
  ((%balance-factor :accessor balance-factor
                    :initform 0
                    :type (integer -2 2))))

;;; Internal utility functions

;; these two are merely for testing

(defun tree-to-list (root)
  "Returns the binary tree as a list."
  (cond ((not (node-p root)) :sentinel)
        (t
         (list (format nil "value:~d b-factor:~d" (data root) (balance-factor root))
               (tree-to-list (left root))
               (tree-to-list (right root))))))

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

;; (loop with test = (make-tree 'avl-tree)
;;             repeat 2000
;;             do (insert test (make-node test (print (random 10))))
;;                (unless (%avl-tree/check-invariants test)
;;                  (return (values test t)))
;;             finally (return (values test nil)))

(defun %avl-tree/balance (new)
  (loop :with z = new
        :with parx
        :with new-root
        :for x = (parent z) then (parent z)
        :while (node-p x)
        :do (tagbody
               (if (eq z (right x))
                   (if (plusp (balance-factor x))
                       (progn
                         (setf parx (parent x))
                         (if (minusp (balance-factor z))
                             ;; XXX really should use double rotation here v
                             (let ((bal-lz (balance-factor (left z))))
                               ;;(printv:printv :rl)
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
                                           (balance-factor z) 0)))
                               ;; (printv:printv (tree-to-list (parent new-root))
                               ;;                (data new-root)
                               ;;                (balance-factor new-root))
                               )
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
                               ;;(printv:printv :lr (tree-to-list x))
                               (rotate :left z)
                               (setf new-root (rotate :right x))
                               (case bal-rz
                                 (-1
                                  (setf (balance-factor new-root) 0
                                        (balance-factor x) +1
                                        (balance-factor z) 0))
                                 (0
                                  (setf (balance-factor new-root) 0
                                        (balance-factor x) 0
                                        (balance-factor z) 0))
                                 (+1
                                  (setf (balance-factor new-root) 0
                                        (balance-factor x) 0
                                        (balance-factor z) -1)))
                               ;;(printv:printv (tree-to-list new-root))
                               )
                             (setf new-root (rotate :right x))))
                       (progn
                         (when (plusp (balance-factor x))
                           (setf (balance-factor x) 0)
                           (return))
                         (decf (balance-factor x))
                         (setf z x)
                         (go upwards))))
               ;;(when (node-p parx) (printv:printv (tree-to-list parx)))
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

;;; User protocol

(defmethod insert :after ((tree avl-tree) (node avl-tree-node))
  (let ((new-root (%avl-tree/balance node)))
    (when new-root
      (setf (root tree) new-root)))
  node)
