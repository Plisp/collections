;;;; A red-black tree implementation.

(in-package #:cl-user)

(defpackage #:cl.lisp.collections.rbt
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

(in-package #:cl.lisp.collections.rbt)

(defstruct (tree (:predicate nil)
                 (:copier nil)
                 (:conc-name nil)
                 (:constructor %make-tree))
  root
  nil-node
  key
  test)

(defstruct (node (:predicate nil)
                 (:copier nil)
                 (:conc-name nil)
                 (:constructor %make-node))
  tree
  data
  color
  parent
  child/left
  child/right)

(u:define-printer (tree stream :type nil :identity t)
  (format stream "RED-BLACK-TREE"))

(u:define-printer (node stream :identity t)
  (format stream "~s" (data node)))

(declaim (ftype (function (&key (:tree tree) (:data t)) node) make-node))
(defun make-node (&key tree data)
  (declare (optimize speed))
  (let ((node (%make-node :tree tree :color :black :data data)))
    (setf (parent node) node
          (child/left node) node
          (child/right node) node)
    node))

(declaim (ftype (function (node) (or node null)) node-p))
(defun node-p (node)
  (declare (optimize speed))
  (unless (eq node (nil-node (tree node)))
    node))

(declaim (ftype (function (&key (:key function) (:test function))) make-tree))
(defun make-tree (&key (key #'identity) (test #'<))
  (declare (optimize speed))
  (let* ((tree (%make-tree :key key :test test))
         (nil-node (make-node :tree tree)))
    (setf (nil-node tree) nil-node
          (root tree) nil-node)
    tree))

(declaim (ftype (function (tree) boolean) valid-p))
(defun valid-p (tree)
  (declare (optimize speed))
  (let ((previous nil))
    (labels ((%check (node test key)
               (declare (function test key))
               (when (node-p node)
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
  (declare (optimize speed)
           (function func))
  (labels ((%walk (node)
             (when (node-p node)
               (%walk (child/left node))
               (funcall func (data node))
               (%walk (child/right node)))))
    (etypecase tree/node
      (tree (%walk (root tree/node)))
      (node (%walk tree/node)))))

(declaim (ftype (function (tree t) (values t (or node null))) find))
(defun find (tree data)
  (declare (optimize speed))
  (labels ((%find (node key test)
             (declare (function key test))
             (a:when-let ((result (and (node-p node)
                                       (funcall key (data node)))))
               (cond
                 ((funcall test data result)
                  (%find (child/left node) key test))
                 ((funcall test result data)
                  (%find (child/right node) key test))
                 (t node)))))
    (a:when-let ((node (%find (root tree) (key tree) (test tree))))
      (values (data node)
              node))))

(declaim (ftype (function (tree node) tree) rotate/left))
(defun rotate/left (tree node)
  (declare (optimize speed))
  (let ((right (child/right node)))
    (setf (child/right node) (child/left right)
          (parent (child/left right)) node
          (parent right) (parent node))
    (cond
      ((not (node-p (parent node)))
       (setf (root tree) right))
      ((eq node (child/left (parent node)))
       (setf (child/left (parent node)) right))
      (t (setf (child/right (parent node)) right)))
    (setf (child/left right) node
          (parent node) right)
    tree))

(declaim (ftype (function (tree node) tree) rotate/right))
(defun rotate/right (tree node)
  (declare (optimize speed))
  (let ((left (child/left node)))
    (setf (child/left node) (child/right left)
          (parent (child/right left)) node
          (parent left) (parent node))
    (cond
      ((not (node-p (parent node)))
       (setf (root tree) left))
      ((eq node (child/right (parent node)))
       (setf (child/right (parent node)) left))
      (t (setf (child/left (parent node)) left)))
    (setf (child/right left) node
          (parent node) left)
    tree))

(declaim (ftype (function (tree node) tree) insert/fix))
(defun insert/fix (tree node)
  (declare (optimize speed))
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
                            (funcall rotate1 tree current))
                          (setf (color parent) :black
                                (color grandparent) :red)
                          (funcall rotate2 tree grandparent)))
                       nil)))
              (if (eq parent (child/left grandparent))
                  (process #'child/right #'rotate/left #'rotate/right)
                  (process #'child/left #'rotate/right #'rotate/left)))
        :finally (setf (color (root tree)) :black))
  tree)

(declaim (ftype (function (tree t) node) insert))
(defun insert (tree data)
  (declare (optimize speed))
  (let* ((node (make-node :tree tree :data data))
         (key (key tree))
         (test (test tree))
         (nil-node (nil-node tree))
         (parent nil-node)
         (current (root tree)))
    (declare (function key test))
    (flet ((test (data2)
             (funcall test (funcall key data) (funcall key data2))))
      (loop :while (node-p current)
            :do (setf parent current)
                (if (test (data current))
                    (setf current (child/left current))
                    (setf current (child/right current))))
      (setf (parent node) parent)
      (cond
        ((not (node-p parent))
         (setf (root tree) node))
        ((test (data parent))
         (setf (child/left parent) node))
        (t (setf (child/right parent) node)))
      (setf (child/left node) nil-node
            (child/right node) nil-node
            (color node) :red)
      (insert/fix tree node)
      node)))

(declaim (ftype (function (tree node) null) delete/fix))
(defun delete/fix (tree node)
  (let ((x node)
        (w nil))
    (u:while (and (not (node-p x))
                  (eq (color x) :black))
      (cond
        ((eq x (child/left (parent x)))
         (setf w (child/right (parent x)))
         (when (eq (color w) :red)
           (setf (color w) :black
                 (color (parent x)) :red)
           (rotate/left tree (parent x))
           (setf w (child/right (parent x))))
         (cond
           ((and (eq (color (child/left w)) :black)
                 (eq (color (child/right w)) :black))
            (setf (color w) :red
                  x (parent x)))
           ((eq (color (child/right w)) :black)
            (setf (color (child/left w)) :black
                  (color w) :red)
            (rotate/right tree w)
            (setf w (child/right (parent x))))
           (t (setf (color w) (color (parent x))
                    (color (parent x)) :black
                    (color (child/right w)) :black)
              (rotate/left tree (parent x))
              (setf x (root tree)))))
        (t (setf w (child/left (parent x)))
           (when (eq (color w) :red)
             (setf (color w) :black
                   (color (parent x)) :red)
             (rotate/right tree (parent x))
             (setf w (child/left (parent x))))
           (cond
             ((and (eq (color (child/right w)) :black)
                   (eq (color (child/left w)) :black))
              (setf (color w) :red
                    x(parent x)))
             ((eq (color (child/left w)) :black)
              (setf (color (child/right w)) :black
                    (color w) :red)
              (rotate/left tree w)
              (setf w (child/left (parent x))))
             (t (setf (color w) (color (parent x))
                      (color (parent x)) :black
                      (color (child/left w)) :black)
                (rotate/right tree (parent x))
                (setf x (root tree)))))))
    nil))

(declaim (ftype (function (tree t) (values tree boolean)) delete))
(defun delete (tree node/data)
  (declare (optimize speed))
  (labels ((%transplant (node1 node2)
             (let ((parent (parent node1)))
               (cond
                 ((not (node-p parent))
                  (setf (root tree) node2))
                 ((eq node1 (child/left parent))
                  (setf (child/left parent) node2))
                 (t (setf (child/right parent) node2)))
               (setf (parent node2) parent)))
           (%delete (node)
             (let* ((x (nil-node tree))
                    (y node)
                    (original-color (color y)))
               (cond
                 ((not (node-p (child/left node)))
                  (setf x (child/right node))
                  (%transplant node (child/right node)))
                 ((not (node-p (child/right node)))
                  (setf x (child/left node))
                  (%transplant node (child/left node)))
                 (t (setf y (nth-value 1 (min (child/right node)))
                          original-color (color y)
                          x (child/right y))
                    (cond
                      ((eq (parent y) node)
                       (setf (parent x) y))
                      (t (%transplant y (child/right y))
                         (setf (child/right y) (child/right node)
                               (parent (child/right y)) y)))
                    (%transplant node y)
                    (setf (child/left y) (child/left node)
                          (parent (child/left y)) y
                          (color y) (color node))))
               (when (eq original-color :black)
                 (delete/fix tree x))
               nil)))
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
  (declare (optimize speed))
  (flet ((%min (node)
           (when (node-p node)
             (loop :for current = (child/left node)
                   :while (node-p current)
                   :do (setf node current)
                   :finally (return (values (data node) node))))))
    (etypecase tree/node
      (tree (%min (root tree/node)))
      (node (%min tree/node)))))

(declaim (ftype (function ((or tree node)) (values t (or node null))) max))
(defun max (tree/node)
  (declare (optimize speed))
  (flet ((%max (node)
           (when (node-p node)
             (loop :for current = (child/right node)
                   :while (node-p current)
                   :do (setf node current)
                   :finally (return (values (data node) node))))))
    (etypecase tree/node
      (tree (%max (root tree/node)))
      (node (%max tree/node)))))

(declaim (ftype (function (node) (values t (or node null))) previous))
(defun previous (node)
  (declare (optimize speed))
  (a:if-let ((left (node-p (child/left node))))
    (max left)
    (loop :for current = node :then parent
          :for parent = (parent current)
          :while (and (node-p parent)
                      (eq current (child/left parent)))
          :finally (return (values (data parent) parent)))))

(declaim (ftype (function (node) (values t (or node null))) next))
(defun next (node)
  (declare (optimize speed))
  (a:if-let ((right (node-p (child/right node))))
    (min right)
    (loop :for current = node :then parent
          :for parent = (parent current)
          :while (and (node-p parent)
                      (eq current (child/right parent)))
          :finally (return (values (data parent) parent)))))
