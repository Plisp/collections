;;;; A binary search tree (BST) implementation.
;;;; Stores items in a binary tree and allows fast searching, insertion, and
;;;; deletion of items.

(in-package #:cl.lisp.collections.tree)

;;; Type definitions and constructors

(defclass binary-search-tree (tree) ())

(defclass binary-search-tree-node (node)
  ((%left :accessor left
          :initform nil)
   (%right :accessor right
           :initform nil)))

;;; Internal utility functions

(defun %binary-search-tree/walk-pre-order (node func)
  (when (node-p node)
    (u:do-hash-keys (k (data node))
      (funcall func k))
    (%binary-search-tree/walk-pre-order (left node) func)
    (%binary-search-tree/walk-pre-order (right node) func)))

(defun %binary-search-tree/walk-in-order (node func)
  (loop :with current = (root (tree node))
        :with stack
        :do (cond
              ((node-p current)
               (push current stack)
               (setf current (left current)))
              (stack
               (setf current (pop stack))
               (u:do-hash-keys (k (data current))
                 (funcall func k))
               (setf current (right current)))
              (t (loop-finish)))))

(defun %binary-search-tree/walk-post-order (node func)
  (when (node-p node)
    (%binary-search-tree/walk-post-order (left node) func)
    (%binary-search-tree/walk-post-order (right node) func)
    (u:do-hash-keys (k (data node))
      (funcall func k))))

(defun %binary-search-tree/walk (node func order)
  (ecase order
    (:pre (%binary-search-tree/walk-pre-order node func))
    (:in (%binary-search-tree/walk-in-order node func))
    (:post (%binary-search-tree/walk-post-order node func))))

(defun %binary-search-tree/find (tree item)
  (labels ((%find (node key sorter)
             (a:when-let ((result (and (node-p node) (key node))))
               (cond
                 ((funcall sorter key result)
                  (%find (left node) key sorter))
                 ((funcall sorter result key)
                  (%find (right node) key sorter))
                 (t node)))))
    (when (typep item (item-type tree))
      (a:if-let ((node (%find (root tree)
                              (funcall (key tree) item)
                              (sorter tree))))
        (values (u:href (data node) item)
                node)
        (values nil nil)))))

(defun %binary-search-tree/rotate-left (node)
  (let ((parent (parent node))
        (right (right node)))
    (setf (right node) (left right))
    (when (node-p (left right))
      (setf (parent (left right)) node))
    (setf (parent right) parent)
    (cond
      ((not (node-p parent))
       (setf (root (tree node)) right))
      ((eq node (left parent))
       (setf (left parent) right))
      (t (setf (right parent) right)))
    (setf (left right) node
          (parent node) right)))

(defun %binary-search-tree/rotate-right (node)
  (let ((parent (parent node))
        (left (left node)))
    (setf (left node) (right left))
    (when (node-p (right left))
      (setf (parent (right left)) node))
    (setf (parent left) parent)
    (cond
      ((not (node-p parent))
       (setf (root (tree node)) left))
      ((eq node (right parent))
       (setf (right parent) left))
      (t (setf (left parent) left)))
    (setf (right left) node
          (parent node) left)))

(defun %binary-search-tree/delete (node)
  (cond
    ((not (node-p (left node)))
     (transplant node (right node)))
    ((not (node-p (right node)))
     (transplant node (left node)))
    (t (let* ((successor (min (right node)))
              (left (left successor))
              (right (right successor)))
         (unless (eq node (parent successor))
           (transplant successor right)
           (setf (right successor) (right node)
                 (parent right) successor))
         (transplant node successor)
         (setf (left successor) (left node))
         (when (node-p left)
           (setf (parent left) successor)))))
  node)

;;; Internal protocol

(defmethod rotate ((direction (eql :left)) (node binary-search-tree-node))
  (%binary-search-tree/rotate-left node))

(defmethod rotate ((direction (eql :right)) (node binary-search-tree-node))
  (%binary-search-tree/rotate-right node))

(defmethod rotate ((direction (eql :left/right)) (node binary-search-tree-node))
  (%binary-search-tree/rotate-left node)
  (%binary-search-tree/rotate-right node))

(defmethod rotate ((direction (eql :right/left)) (node binary-search-tree-node))
  (%binary-search-tree/rotate-right node)
  (%binary-search-tree/rotate-left node))

(defmethod transplant ((node1 binary-search-tree-node) node2)
  (let ((parent (parent node1)))
    (cond
      ((not (node-p parent))
       (setf (root (tree node1)) node2))
      ((eq node1 (left parent))
       (setf (left parent) node2))
      (t (setf (right parent) node2)))))

(defmethod transplant :after ((node1 binary-search-tree-node)
                              (node2 binary-search-tree-node))
  (setf (parent node2) (parent node1)))

;;; User protocol

(defmethod valid-p ((tree binary-search-tree))
  (let ((previous nil))
    (labels ((%check (node sorter)
               (when (node-p node)
                 (when (or (null (%check (left node) sorter))
                           (and previous
                                (funcall sorter (key node) (key previous))))
                   (return-from %check))
                 (setf previous node)
                 (return-from %check
                   (%check (right node) sorter)))
               t))
      (%check (root tree) (sorter tree)))))

(defmethod walk ((node binary-search-tree-node) func &key (order :in))
  (%binary-search-tree/walk node func order))

(defmethod find ((tree binary-search-tree) item)
  (%binary-search-tree/find tree item))

(defmethod insert ((tree binary-search-tree) (node binary-search-tree-node))
  (loop :with sorter = (sorter tree)
        :with key = (key node)
        :with current = (root tree)
        :with parent = (sentinel tree)
        :while (node-p current)
        :do (setf parent current)
            (if (funcall sorter key (key current))
                (setf current (left current))
                (setf current (right current)))
        :finally (setf (parent node) parent)
                 (cond
                   ((not (node-p parent))
                    (setf (root tree) node))
                   ((funcall sorter key (key parent))
                    (setf (left parent) node))
                   (t (setf (right parent) node))))
  node)

(defmethod delete ((tree binary-search-tree) (node binary-search-tree-node))
  (%binary-search-tree/delete node))

(defmethod min ((node binary-search-tree-node))
  (when (node-p node)
    (loop :for current = (left node)
          :while (node-p current)
          :do (setf node current)
          :finally (return node))))

(defmethod max ((node binary-search-tree-node))
  (when (node-p node)
    (loop :for current = (right node)
          :while (node-p current)
          :do (setf node current)
          :finally (return node))))

(defmethod previous ((node binary-search-tree-node))
  (when (node-p node)
    (a:if-let ((left (node-p (left node))))
      (max left)
      (loop :for current = node :then parent
            :for parent = (parent current)
            :while (and (node-p parent)
                        (eq current (left parent)))
            :finally (return parent)))))

(defmethod next ((node binary-search-tree-node))
  (when (node-p node)
    (a:if-let ((right (node-p (right node))))
      (min right)
      (loop :for current = node :then parent
            :for parent = (parent current)
            :while (and (node-p parent)
                        (eq current (right parent)))
            :finally (return parent)))))
