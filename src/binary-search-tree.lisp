;;;; A binary search tree (BST) implementation.
;;;; Stores items in a binary tree and allows fast searching, insertion, and
;;;; deletion of items.

(in-package #:cl.lisp.collections.tree)

(defclass binary-search-tree (tree) ())

(defclass binary-search-tree-node (node)
  ((%child/left :accessor child/left
                :initform nil)
   (%child/right :accessor child/right
                 :initform nil)))

(defmethod valid-p ((tree binary-search-tree))
  (let ((previous nil))
    (labels ((%check (node test key)
               (declare (function test key))
               (when node
                 (when (or (null (%check (child/left node) test key))
                           (and previous
                                (funcall test
                                         (funcall key (data node))
                                         (funcall key (data previous)))))
                   (return-from %check))
                 (setf previous node)
                 (return-from %check
                   (%check (child/right node) test key)))
               t))
      (%check (root tree) (test tree) (key tree)))))

(defmethod walk ((tree binary-search-tree) func)
  (a:when-let ((root (root tree)))
    (walk root func)))

(defmethod walk ((node binary-search-tree-node) func)
  (labels ((%walk (node)
             (when node
               (%walk (child/left node))
               (funcall func (data node))
               (%walk (child/right node)))))
    (%walk node)))

(defmethod find ((tree binary-search-tree) item)
  (labels ((%find (node key test)
             (a:when-let ((result (and node (funcall key (data node)))))
               (cond
                 ((funcall test item result)
                  (%find (child/left node) key test))
                 ((funcall test result item)
                  (%find (child/right node) key test))
                 (t node)))))
    (with-slots (%root %key %test) tree
      (a:when-let ((node (%find %root %key %test)))
        (values (data node)
                node)))))

(defmethod insert ((tree binary-search-tree) item)
  (with-slots (%root %key %test) tree
    (flet ((test (data)
             (funcall %test (funcall %key item) (funcall %key data))))
      (loop :with node = (make-node tree item)
            :with current = %root
            :with parent = nil
            :while current
            :do (setf parent current)
                (if (test (data current))
                    (setf current (child/left current))
                    (setf current (child/right current)))
            :finally (setf (parent node) parent)
                     (cond
                       ((null parent)
                        (setf %root node))
                       ((test (data parent))
                        (setf (child/left parent) node))
                       (t (setf (child/right parent) node)))
                     (return node)))))

(defmethod delete ((tree binary-search-tree) (node binary-search-tree-node))
  (labels ((%transplant (node1 node2)
             (let ((parent (parent node1)))
               (cond
                 ((null parent)
                  (setf (root tree) node2))
                 ((eq node1 (child/left parent))
                  (setf (child/left parent) node2))
                 (t (setf (child/right parent) node2)))
               (when node2
                 (setf (parent node2) parent))))
           (%delete (node)
             (cond
               ((null (child/left node))
                (%transplant node (child/right node)))
               ((null (child/right node))
                (%transplant node (child/left node)))
               (t (let* ((successor (min (child/right node)))
                         (left (child/left successor))
                         (right (child/right successor)))
                    (unless (eq node (parent successor))
                      (%transplant successor right)
                      (setf (child/right successor) (child/right node)
                            (parent right) successor))
                    (%transplant node successor)
                    (setf (child/left successor) (child/left node)
                          (parent left) successor)
                    t)))))
    (values tree (%delete node))))

(defmethod delete ((tree binary-search-tree) item)
  (let ((node (nth-value 1 (find tree item))))
    (values tree
            (when node
              (delete node)))))

(defmethod min ((tree binary-search-tree))
  (a:when-let ((root (root tree)))
    (min root)))

(defmethod min ((node binary-search-tree-node))
  (loop :for current = (child/left node)
        :while current
        :do (setf node current)
        :finally (return (values (data node) node))))

(defmethod max ((tree binary-search-tree))
  (a:when-let ((root (root tree)))
    (max root)))

(defmethod max ((node binary-search-tree-node))
  (loop :for current = (child/right node)
        :while current
        :do (setf node current)
        :finally (return (values (data node) node))))

(defmethod previous ((node binary-search-tree-node))
  (when node
    (a:if-let ((left (child/left node)))
      (max left)
      (loop :for current = node :then parent
            :for parent = (parent current)
            :while (and parent (eq current (child/left parent)))
            :finally (when parent
                       (return
                         (values (data parent)
                                 parent)))))))

(defmethod next ((node binary-search-tree-node))
  (when node
    (a:if-let ((right (child/right node)))
      (min right)
      (loop :for current = node :then parent
            :for parent = (parent current)
            :while (and parent (eq current (child/right parent)))
            :finally (when parent
                       (return
                         (values (data parent)
                                 parent)))))))
