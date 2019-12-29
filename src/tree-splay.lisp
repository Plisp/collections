;;; A splay tree implementation.

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

(defclass splay-tree (binary-search-tree) ())

(defclass splay-tree-node (binary-search-tree-node) ())

;;; Internal utility functions

(defun %splay-tree/join (node1 node2)
  (unless (node-p node1)
    (return-from %splay-tree/join node2))
  (unless (node-p node2)
    (return-from %splay-tree/join node1))
  (let ((node (max node1)))
    (%splay-tree/splay node)
    (setf (right node) node2
          (parent node2) node)
    node))

(defun %splay-tree/split (node)
  (let ((node1 node)
        (node2 nil))
    (%splay-tree/splay node)
    (when (node-p (right node))
      (setf node2 (right node)
            (parent node2) nil))
    (setf (right node1) nil)
    (values node1 node2)))

(defun %splay-tree/splay (node)
  (loop :while (node-p (parent node))
        :do (cond
              ((not (node-p (parent (parent node))))
               (if (eq node (left (parent node)))
                   (rotate :right (parent node))
                   (rotate :left (parent node))))
              ((and (eq node (left (parent node)))
                    (eq (parent node) (left (parent (parent node)))))
               (rotate :right (parent (parent node)))
               (rotate :right (parent node)))
              ((and (eq node (right (parent node)))
                    (eq (parent node) (right (parent (parent node)))))
               (rotate :left (parent (parent node)))
               (rotate :left (parent node)))
              ((and (eq node (left (parent node)))
                    (eq (parent node) (right (parent (parent node)))))
               (rotate :right (parent node))
               (rotate :left (parent node)))
              (t (rotate :left (parent node))
                 (rotate :right (parent node))))
        :finally (return node)))

;;; User protocol

(defmethod valid-p ((tree splay-tree))
  (error "Not implemented."))

(defmethod find ((tree splay-tree) item)
  (u:mvlet ((item node (%binary-search-tree/find tree item)))
    (when node
      (%splay-tree/splay node))
    (values item node)))

(defmethod insert :after ((tree splay-tree) (node splay-tree-node))
  (%splay-tree/splay node))

(defmethod delete ((tree splay-tree) (node splay-tree-node))
  (let ((left (make-tree 'splay-tree))
        (right (make-tree 'splay-tree)))
    (when (node-p (root left))
      (setf (parent (root left)) nil))
    (when (node-p (root right))
      (setf (parent (root right)) nil))
    (setf (root left) (left (root tree))
          (root right) (right (root tree)))
    (if (node-p (root left))
        (let ((max (max (root left))))
          (%splay-tree/splay max)
          (setf (right (root left)) (root right)
                (root tree) (root left)))
        (setf (root tree) (root right)))
    node)
  )
