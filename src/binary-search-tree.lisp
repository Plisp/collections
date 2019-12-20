;;;; A binary search tree (BST) implementation.
;;;; Stores items in a binary tree and allows fast searching, insertion, and
;;;; deletion of items.

(in-package #:cl-user)

(defpackage #:cl.lisp.collections.bst
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:max
   #:min)
  (:export
   #:delete
   #:find
   #:insert
   #:make-tree
   #:max
   #:min
   #:next
   #:node
   #:previous
   #:tree
   #:valid-p
   #:walk))

(in-package #:cl.lisp.collections.bst)

(defstruct (tree (:predicate nil)
                 (:copier nil)
                 (:conc-name nil)
                 (:constructor %make-tree))
  root
  key
  test)

(defstruct (node (:predicate nil)
                 (:copier nil)
                 (:conc-name nil))
  tree
  data
  parent
  child/left
  child/right)

(u:define-printer (tree stream :type nil :identity t)
  (format stream "BINARY-SEARCH-TREE"))

(u:define-printer (node stream :identity t)
  (format stream "~s" (data node)))

(declaim (ftype (function (&key (:key function) (:test function))) make-tree))
(defun make-tree (&key (key #'identity) (test #'<))
  "Construct a new binary search tree object. `KEY` is a function taking 1
  argument used to test the equality of the data stored in a node. `TEST` is a
  function taking 2 arguments, which is used for sorting the tree. The `TEST`
  function should return non-NIL if the first argument is less than the second
  object, and it should fail if the first argument is greater than or equal to
  the second argument."
  (declare (optimize speed))
  (%make-tree :key key :test test))

(declaim (ftype (function (tree) boolean) valid-p))
(defun valid-p (tree)
  "Check the validity of a binary search tree. Returns T if walking in-order
  would yield a sorted sequence, else NIL. Running time is O(n), proportional to
  the number of nodes in the tree."
  (declare (optimize speed))
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

(declaim (ftype (function ((or tree node) &optional function) null) walk))
(defun walk (tree/node &optional (func (constantly nil)))
  "Walk a binary search tree from the root and call a function for each node
  visited. `TREE/NODE` can be either an instance of a TREE object as constructed
  with MAKE-TREE, or any NODE object in the tree, to operate on the sub-tree
  rooted at that node. `FUNCTION` is an arbitrary function taking 1 argument,
  which represents the data stored for that node. Running time is O(n),
  proportional to the number of nodes in the tree."
  (declare (optimize speed)
           (function func))
  (labels ((%walk (node)
             (when node
               (%walk (child/left node))
               (funcall func (data node))
               (%walk (child/right node)))))
    (etypecase tree/node
      (tree (%walk (root tree/node)))
      (node (%walk tree/node)))))

(declaim (ftype (function (tree t) (values t (or node null))) find))
(defun find (tree data)
  "Search for data stored in a binary search tree. `TREE` is a tree object as
  constructed with MAKE-TREE. `DATA` is the data to search for in the tree.
  Returns as multiple values, the data if found, and the node object containing
  this data. If the second return value is non-existent, it can be assumed that
  the search did not find the data requested. Running time is O(h) where 'h' is
  the height of the tree."
  (declare (optimize speed))
  (labels ((%find (node key test)
             (declare (function key test))
             (a:when-let ((result (and node (funcall key (data node)))))
               (cond
                 ((funcall test data result)
                  (%find (child/left node) key test))
                 ((funcall test result data)
                  (%find (child/right node) key test))
                 (t node)))))
    (a:when-let ((node (%find (root tree) (key tree) (test tree))))
      (values (data node)
              node))))

(declaim (ftype (function (tree t) node) insert))
(defun insert (tree data)
  "Insert a new node into a binary search tree. `TREE` is a tree object as
  constructed with MAKE-TREE. `DATA` is the data to search for in the tree.
  Returns the NODE object that was inserted. Running time is O(h) where 'h' is
  the height of the tree."
  (declare (optimize speed))
  (let ((node (make-node :tree tree :data data))
        (key (key tree))
        (test (test tree))
        (current (root tree))
        (parent nil))
    (declare (function key test))
    (flet ((test (data2)
             (funcall test (funcall key data) (funcall key data2))))
      (loop :while current
            :do (setf parent current)
                (if (test (data current))
                    (setf current (child/left current))
                    (setf current (child/right current))))
      (setf (parent node) parent)
      (cond
        ((null parent)
         (setf (root tree) node))
        ((test (data parent))
         (setf (child/left parent) node))
        (t (setf (child/right parent) node)))
      node)))

(declaim (ftype (function (tree t) (values tree boolean)) delete))
(defun delete (tree node/data)
  "Delete a node from a binary search tree. `TREE` is a tree object as
  constructed with MAKE-TREE. `NODE/DATA` can be either a NODE object, or data
  to search for. In the case of data, the running time is increased, as a search
  with FIND must first complete to get the NODE object. Returns as multiple
  values, the modified TREE object, and T is deletion was successful.  If the
  second return value is non-existent, it can be assumed that the data supplied
  for `NODE/DATA` did not find a node to delete. Running time is O(h) where 'h'
  is the height of the tree."
  (declare (optimize speed))
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
                          (parent left) successor))))))
    (typecase node/data
      (node
       (%delete node/data)
       (values tree t))
      (t (let ((node (nth-value 1 (find tree node/data))))
           (values tree
                   (when node
                     (%delete node)
                     t)))))))

(declaim (ftype (function ((or tree node)) (values t (or node null))) min))
(defun min (tree/node)
  "Retrieve the minimum element of a binary search tree. `TREE/NODE` can be
  either a TREE object as constructed with MAKE-TREE, or any NODE object in the
  tree, to operate on the sub-tree rooted at that node. Running time is O(h)
  where 'h' is the height of the tree."
  (declare (optimize speed))
  (flet ((%min (node)
           (when node
             (loop :for current = (child/left node)
                   :while current
                   :do (setf node current)
                   :finally (return (values (data node) node))))))
    (etypecase tree/node
      (tree (%min (root tree/node)))
      (node (%min tree/node)))))

(declaim (ftype (function ((or tree node)) (values t (or node null))) max))
(defun max (tree/node)
  "Retrieve the maximum element in a binary search tree. `TREE/NODE` can be
  either a TREE object as constructed with MAKE-TREE, or any NODE object in the
  tree, to operate on the sub-tree rooted at that node. Running time is O(h)
  where 'h' is the height of the tree."
  (declare (optimize speed))
  (flet ((%max (node)
           (when node
             (loop :for current = (child/right node)
                   :while current
                   :do (setf node current)
                   :finally (return (values (data node) node))))))
    (etypecase tree/node
      (tree (%max (root tree/node)))
      (node (%max tree/node)))))

(declaim (ftype (function (node) (values t (or node null))) previous))
(defun previous (node)
  "Retrieve the predecessor of a given node in a binary search tree. `NODE` is a
  NODE object within the tree. Returns as multiple values, the data of the
  previous node, and the previous NODE object. Running time is O(h) where 'h' is
  the height of the tree."
  (declare (optimize speed))
  (when node
    (a:if-let ((left (child/left node)))
      (max left)
      (loop :for current = node :then parent
            :for parent = (parent current)
            :while (and parent (eq current (child/left parent)))
            :finally (return (values (when parent (data parent))
                                     parent))))))

(declaim (ftype (function (node) (values t (or node null))) next))
(defun next (node)
  "Retrieve the successor of a given node in a binary search tree. `NODE` is a
  NODE object within the tree. Returns as multiple values, the data of the
  previous node, and the previous NODE object. Running time is O(h) where 'h' is
  the height of the tree."
  (declare (optimize speed))
  (when node
    (a:if-let ((right (child/right node)))
      (min right)
      (loop :for current = node :then parent
            :for parent = (parent current)
            :while (and parent (eq current (child/right parent)))
            :finally (return (values (when parent (data parent))
                                     parent))))))
