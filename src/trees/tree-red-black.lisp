;;;; A red-black tree implementation.

(in-package #:cl.lisp.collections.tree)

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
        (child/left instance) instance
        (child/right instance) instance))

(defmethod node-p ((node red-black-tree-node))
  (unless (eq node (sentinel (tree node)))
    node))

(defmethod transplant :after ((node1 red-black-tree-node)
                              (node2 red-black-tree-node))
  (setf (parent node2) (parent node1)))

(defmethod insert :after ((tree red-black-tree) (node red-black-tree-node))
  (setf (child/left node) (sentinel tree)
        (child/right node) (sentinel tree)
        (color node) :red)
  (loop :for current = node
        :for parent = (parent current)
        :for grandparent = (parent parent)
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
                            (rotate rotate1 current))
                          (setf (color parent) :black
                                (color grandparent) :red)
                          (rotate rotate2 grandparent)))
                       nil)))
              (if (eq parent (child/left grandparent))
                  (process #'child/right :left :right)
                  (process #'child/left :right :left)))
        :finally (setf (color (root tree)) :black))
  node)

(defmethod delete ((tree red-black-tree) (node red-black-tree-node))
  (let* ((x nil)
         (y node)
         (color (color y)))
    (cond
      ((not (node-p (child/left node)))
       (setf x (child/right node))
       (transplant node (child/right node)))
      ((not (node-p (child/right node)))
       (setf x (child/left node))
       (transplant node (child/left node)))
      (t (setf y (nth-value 1 (min (child/right node)))
               color (color y)
               x (child/right y))
         (cond
           ((eq (parent y) node)
            (setf (parent x) y))
           (t (transplant y (child/right y))
              (setf (child/right y) (child/right node)
                    (parent (child/right y)) y)))
         (transplant node y)
         (setf (child/left y) (child/left node)
               (parent (child/left y)) y
               (color y) (color node))))
    (when (eq color :black)
      (delete/fix tree x))
    node))

(defmethod delete/fix ((tree red-black-tree) (node red-black-tree-node))
  (macrolet ((fix (rotate1 rotate2)
               (let ((child1 (a:symbolicate '#:child/ rotate1))
                     (child2 (a:symbolicate '#:child/ rotate2)))
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
                         (setf x (root tree))))))))
    (let ((x node)
          (w nil))
      (u:while (and (not (node-p x))
                    (eq (color x) :black))
        (if (eq x (child/left (parent x)))
            (fix :left :right)
            (fix :right :left))))))
