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

(declaim (inline black-p red-p blacken redden))
(defun black-p (node)
  (eq (color node) :black))
(defun red-p (node)
  (eq (color node) :red))
(defun blacken (node)
  (setf (color node) :black))
(defun redden (node)
  (setf (color node) :red))

(defun %red-black-tree/check-invariants (tree)
  "Checks the red-black-tree conditions for the tree/subtree with root ROOT.
Returns the black depth of the tree on success.
TODO put in red-black tree tests"
  (labels ((recur (node)
             (cond ((not (node-p node)) 0)
                   (t (let ((a (recur (left node)))
                            (b (recur (right node)))
                            (parcheck t))
                        (when (node-p (left node))
                          (setf parcheck (eq (parent (left node)) node)))
                        (when (node-p (right node))
                          ;; if parcheck is already wrong it's wrong
                          (setf parcheck (and parcheck (eq (parent (right node)) node))))
                        (if (or (not parcheck) (null a) (null b) (/= a b))
                            nil
                            (cond ((black-p node) (1+ a))
                                  (t
                                   (if (and (black-p (left node))
                                            (black-p (right node)))
                                       a
                                       nil)))))))))
    (and (black-p (root tree)) (recur (root tree)))))

(let ((prev/next 0))
  (defun %red-black-tree/delete (node root)
    (let (x z) ; x is 'actually' deleted, z is x's child (may be +sentinel+)
      (cond ((not (node-p (left node)))
             (setf x node)
             (setf z (right node)))
            ((not (node-p (right node)))
             (setf x node)
             (setf z (left node)))
            (t ; alternate successor/predecessor to lessen unbalancing
             (if (zerop (setf prev/next (logxor prev/next 1)))
                 (progn
                   (setf x (min (right node)))
                   (setf (data node) (data x))
                   (setf z (right x)))
                 (progn ; symmetric case
                   (setf x (max (left node)))
                   (setf (data node) (data x))
                   (setf z (left x))))))
      (when (eq x root) ; deleting root node, z is sentinel
        (blacken z)
        (return-from %red-black-tree/delete z))
      (transplant x z)
      (if (black-p x)
          (if (red-p z)
              (progn
                ;; When the deleted node x is black and its only child z is red,
                ;; blackening z respects the black-depth of the tree.
                (blacken z)
                root)
              (%red-black-tree/delete-fix z (parent x) root))
          ;; if x is red then its children must have been leaf nodes. Removing x
          ;; has no effect on the black depth meaning no correction is needed
          root))))

(defun %red-black-tree/delete-fix (node parent root)
  (let (new-root)
    (loop
      :while (not (eq node root))
      :finally (return root)
      :do (setf new-root nil)
          (if (eq (left parent) node)
              (let ((alpha parent)
                    (beta)
                    (gamma)
                    (delta))
                (cond ((red-p alpha)
                       (setf beta (right alpha))
                       (setf gamma (left beta))
                       (cond ((black-p gamma) ;1a
                              (if (node-p (parent alpha))
                                  (rotate :left alpha)
                                  (setf root (rotate :left alpha))))
                             (t
                              (if (node-p (parent alpha))
                                  (rotate :right/left alpha)
                                  (setf root (rotate :right/left alpha)))
                              (blacken alpha)))
                       (return-from %red-black-tree/delete-fix root))
                      ((black-p alpha)
                       (setf beta (right alpha))
                       (cond ((black-p beta)
                              (setf gamma (left beta))
                              (setf delta (right beta))
                              (cond ((red-p gamma)
                                     (cond ((red-p delta)
                                            (redden beta)
                                            (blacken gamma)
                                            (blacken delta)) ; 2c -> 3
                                           (t
                                            (if (node-p (parent alpha))
                                                (rotate :right/left alpha)
                                                (setf root (rotate :right/left alpha)))
                                            (blacken gamma)
                                            (return-from %red-black-tree/delete-fix root)))) ; 2b1
                                    ;; gamma is black, now decide if delta is black
                                    ;; too (2a) or red (2b2)
                                    (t
                                     (cond ((red-p delta)
                                            (if (node-p (parent alpha))
                                                (rotate :left alpha)
                                                (setf root (rotate :left alpha)))
                                            (blacken delta)
                                            (return-from %red-black-tree/delete-fix root)) ; 2b2
                                           (t ; now comes 2a
                                            (redden beta)
                                            (setf new-root alpha))))))
                             (t ; this means beta is red, this gives cases 3a and 3b
                              (setf gamma (left beta))
                              (setf delta (left gamma))
                              (cond ((red-p delta) ; this is 3b
                                     (if (node-p (parent alpha))
                                         (rotate :left alpha)
                                         (setf root (rotate :left alpha)))
                                     (if (node-p (parent gamma))
                                         (rotate :right gamma)
                                         (setf root (rotate :right gamma)))
                                     (if (node-p (parent alpha))
                                         (rotate :left alpha)
                                         (setf root (rotate :left alpha)))
                                     (blacken beta)
                                     (return-from %red-black-tree/delete-fix root))
                                    (t ; this is 3a
                                     (if (node-p (parent alpha))
                                         (rotate :left alpha)
                                         (setf root (rotate :left alpha)))
                                     (if (node-p (parent alpha))
                                         (rotate :left alpha)
                                         (setf root (rotate :left alpha)))
                                     (redden alpha)
                                     (blacken beta)
                                     (return-from %red-black-tree/delete-fix root))))))))

              ;; the following code is dual under left-right
              (let ((alpha parent)
                    (beta)
                    (gamma)
                    (delta))
                (cond ((red-p alpha)
                       (setf beta (left alpha))
                       (setf gamma (right beta))
                       (cond ((black-p gamma) ;1a
                              (if (node-p (parent alpha))
                                  (rotate :right alpha)
                                  (setf root (rotate :right alpha))))
                             (t
                              (if (node-p (parent alpha))
                                  (rotate :left/right alpha)
                                  (setf root (rotate :left/right alpha)))
                              (blacken alpha)))
                       (return-from %red-black-tree/delete-fix root))
                      ((black-p alpha)
                       (setf beta (left alpha))
                       (cond ((black-p beta)
                              (setf gamma (right beta))
                              (setf delta (left beta))
                              (cond ((red-p gamma)
                                     (cond ((red-p delta)
                                            (redden beta)
                                            (blacken gamma)
                                            (blacken delta)) ; 2c -> 3
                                           (t
                                            (if (node-p (parent alpha))
                                                (rotate :left/right alpha)
                                                (setf root (rotate :left/right alpha)))
                                            (blacken gamma)
                                            (return-from %red-black-tree/delete-fix root)))) ; 2b1
                                    ;; gamma is black, is delta is black too (2a)?
                                    ;; or red (2b2)
                                    (t
                                     (cond ((red-p delta)
                                            (if (node-p (parent alpha))
                                                (rotate :right alpha)
                                                (setf root (rotate :right alpha)))
                                            (blacken delta)
                                            (return-from %red-black-tree/delete-fix root)) ; 2b2
                                           (t ; now comes 2a
                                            (redden beta)
                                            (setf new-root alpha))))))
                             (t ; this means beta is red, this gives cases 3a and 3b
                              (setf gamma (right beta))
                              (setf delta (right gamma))
                              (cond ((red-p delta) ; this is 3b
                                     (if (node-p (parent alpha))
                                         (rotate :right alpha)
                                         (setf root (rotate :right alpha)))
                                     (if (node-p (parent gamma))
                                         (rotate :left gamma)
                                         (setf root (rotate :left gamma)))
                                     (if (node-p (parent alpha))
                                         (rotate :right alpha)
                                         (setf root (rotate :right alpha)))
                                     (blacken beta)
                                     (return-from %red-black-tree/delete-fix root))
                                    (t ; this is 3a
                                     (if (node-p (parent alpha))
                                         (rotate :right alpha)
                                         (setf root (rotate :right alpha)))
                                     (if (node-p (parent alpha))
                                         (rotate :right alpha)
                                         (setf root (rotate :right alpha)))
                                     (redden alpha)
                                     (blacken beta)
                                     (return-from %red-black-tree/delete-fix root)))))))))
          (when new-root
            (setf node new-root)
            (setf parent (parent node))))))

;;; Internal protocol

(defmethod transplant :after ((node1 red-black-tree-node)
                              (node2 red-black-tree-node))
  (if (node-p node2)
      (setf (parent node2) (parent node1))
      (setf (parent node2) (sentinel (tree node2)))))

;;; User protocol

(defmethod insert :after ((tree red-black-tree) (node red-black-tree-node))
  (setf (left node) (sentinel tree)
        (right node) (sentinel tree)
        (color node) :red)
  (loop :with current = node
        :for parent = (parent current)
        :for grandparent = (parent parent)
        :with new-root = (root tree)
        :while (red-p parent)
        :do (flet ((process (child rotate1 rotate2)
                     (let ((y (funcall child grandparent)))
                       (ecase (color y)
                         (:red
                          (blacken parent)
                          (blacken y)
                          (redden grandparent)
                          (setf current grandparent))
                         (:black
                          (when (eq current (funcall child parent))
                            (setf current parent)
                            (rotate rotate1 current)
                            (setf parent (parent current)
                                  grandparent (parent parent)))
                          (blacken parent)
                          (redden grandparent)
                          (let ((subroot (rotate rotate2 grandparent)))
                            (when (not (node-p (parent subroot)))
                              (setf new-root subroot))))))))
              (if (eq parent (left grandparent))
                  (process #'right :left :right)
                  (process #'left :right :left)))
        :finally (setf (root tree) new-root
                       (color (root tree)) :black)))

(defmethod delete ((tree red-black-tree) (node red-black-tree-node))
  (setf (root tree) (%red-black-tree/delete node (root tree)))
  node)
