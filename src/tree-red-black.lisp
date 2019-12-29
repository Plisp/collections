;;;; A red-black tree implementation.
;;;; A specialization of binary search trees in which each node is assigned a
;;;; red or black color, used to keep the tree approximately height-balanced
;;;; when a mutation occurs.

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

(defclass red-black-tree (binary-search-tree) ())

(defclass red-black-tree-node (binary-search-tree-node)
  ((%color :accessor color
           :initform :black)))

(defmethod initialize-instance :after ((instance red-black-tree) &key)
  (let ((sentinel (make-node instance nil)))
    (setf (sentinel instance) sentinel
          (root instance) sentinel)))

(defmethod initialize-instance :after ((instance red-black-tree-node) &key)
  (setf (parent instance) instance
        (left instance) instance
        (right instance) instance))

;;; Internal utility functions

(defun %red-black-tree/check-invariants (tree)
  "Checks the red-black-tree conditions for the tree/subtree with root ROOT.
Returns the black depth of the tree on success.
TODO: Put in red-black tree tests."
  (labels ((recur (node)
             (if (node-p node)
                 (let ((a (recur (left node)))
                       (b (recur (right node)))
                       (parcheck t))
                   (when (node-p (left node))
                     (setf parcheck (eq (parent (left node)) node)))
                   (when (node-p (right node))
                     (setf parcheck (and parcheck
                                         (eq (parent (right node)) node))))
                   (when (and parcheck a b (= a b))
                     (if (eq :black (color node))
                         (1+ a)
                         (when (and (eq :black (color (left node)))
                                    (eq :black (color (right node))))
                           a)))))))
    (and (eq (color (root tree)) :black)
         (recur (root tree)))))

(defun %red-black-tree/delete (node)
  (let* ((x nil)
         (y node)
         (color (color y)))
    (cond
      ((not (node-p (left node)))
       (setf x (right node))
       (transplant node (right node)))
      ((not (node-p (right node)))
       (setf x (left node))
       (transplant node (left node)))
      (t (setf y (min (right node))
               color (color y)
               x (right y))
         (cond
           ((eq (parent y) node)
            (setf (parent x) y))
           (t (transplant y (right y))
              (setf (right y) (right node)
                    (parent (right y)) y)))
         (transplant node y)
         (setf (left y) (left node)
               (parent (left y)) y
               (color y) (color node))))
    ;; y was root, blacken sentinel and return
    (unless (node-p (parent y))
      (setf (color x) :black
            (parent x) (sentinel (tree node)))
      (return-from %red-black-tree/delete x))
    (when (eq color :black)
      (if (eq (color x) :red)
          (setf (color x) :black) ;(%red-black-tree/delete-fix x)
          (return-from %red-black-tree/delete)))
    nil))

(defun %red-black-tree/delete-fix (node)
  (macrolet ((fix (rotate1 rotate2)
               (let ((child1 (a:symbolicate rotate1))
                     (child2 (a:symbolicate rotate2)))
                 `(progn
                    (setf w (,child2 (parent x)))
                    (when (eq (color w) :red)
                      (setf (color w) :black
                            (color (parent x)) :red)
                      (rotate ,rotate1 (parent x))
                      (setf w (,child2 (parent x))))
                    (cond
                      ((and (eq (color (,child1 w)) :black)
                            (eq (color (,child2 w)) :black))
                       (setf (color w) :red
                             x (parent x)))
                      ((eq (color (,child2 w)) :black)
                       (setf (color (,child1 w)) :black
                             (color w) :red)
                       (rotate ,rotate2 w)
                       (setf w (,child2 (parent x))))
                      (t (setf (color w) (color (parent x))
                               (color (parent x)) :black
                               (color (,child2 w)) :black)
                         (rotate ,rotate1 (parent x))
                         (setf x (root (tree node)))))))))
    (let ((x node)
          (w nil))
      (u:while (and (not (node-p x))
                    (eq (color x) :black))
        (if (eq x (left (parent x)))
            (fix :left :right)
            (fix :right :left))))))

;;; Internal protocol

(defmethod transplant :after ((node1 red-black-tree-node)
                              (node2 red-black-tree-node))
  (setf (parent node2) (parent node1)))

;;; User protocol

(defmethod insert :after ((tree red-black-tree) (node red-black-tree-node))
  (setf (left node) (sentinel tree)
        (right node) (sentinel tree)
        (color node) :red)
  (loop :with current = node
        :for parent = (parent current)
        :for grandparent = (parent parent)
        :with new-root = (root tree)
        :while (eq (color parent) :red)
        :do (flet ((process (child rotate1 rotate2)
                     (let ((y (funcall child grandparent)))
                       (ecase (color y)
                         (:red
                          (setf (color parent) :black
                                (color y) :black
                                (color grandparent) :red
                                current grandparent))
                         (:black
                          (when (eq current (funcall child parent))
                            (setf current parent)
                            (rotate rotate1 current)
                            (setf parent (parent current)
                                  grandparent (parent parent)))
                          (setf (color parent) :black
                                (color grandparent) :red)
                          (let ((subroot (rotate rotate2 grandparent)))
                            (unless (node-p (parent subroot))
                              (setf new-root subroot))))))))
              (if (eq parent (left grandparent))
                  (process #'right :left :right)
                  (process #'left :right :left)))
        :finally (setf (root tree) new-root
                       (color (root tree)) :black)))

(defmethod delete ((tree red-black-tree) (node red-black-tree-node))
  (a:when-let ((new-root (%red-black-tree/delete node)))
    (setf (root tree) new-root))
  node)
