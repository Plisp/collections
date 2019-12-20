(in-package #:cl.lisp.collections.test)

(define-test bst/make-tree
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (of-type tree:binary-search-tree tree)
    (false (tree::root tree))
    (is eq #'identity (tree::key tree))
    (is eq #'< (tree::test tree))))

(define-test bst/valid-p
  (dotimes (i 10)
    (let ((tree (tree:make-tree 'tree:binary-search-tree)))
      (dotimes (i 1000)
        (tree:insert tree (random 1d0)))
      (true (tree:valid-p tree))))
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (dotimes (i 10)
      (tree:insert tree (1+ i)))
    (setf (tree::data (tree::root tree)) 42)
    (false (tree:valid-p tree))))

(define-test bst/walk
  (let ((tree (tree:make-tree 'tree:binary-search-tree))
        (input '(8 2 5 1 4 9 3 10 7 6))
        (result nil))
    (false (tree:walk tree #'identity))
    (dotimes (i 10)
      (tree:insert tree (pop input)))
    (false (tree:walk tree #'identity))
    (tree:walk tree (lambda (x) (push (* x 10) result)))
    (is equal '(10 20 30 40 50 60 70 80 90 100) (reverse result))))

(define-test bst/find
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (false (tree:find tree 42))
    (tree:insert tree 420)
    (tree:insert tree 621)
    (dotimes (i 1000)
      (tree:insert tree (random 1000.0)))
    (is = 420 (tree:find tree 420))
    (of-type tree:node (nth-value 1 (tree:find tree 621)))
    (setf tree (tree:make-tree 'tree:binary-search-tree :key #'cdr))
    (tree:insert tree '(:foo . 420))
    (dotimes (i 1000)
      (tree:insert tree `(:bar . ,(random 1000.0))))
    (is = 420 (cdr (tree:find tree 420)))
    (fail (tree:find tree :foo))
    (fail (tree:find tree '(:foo . 420)))))

(define-test bst/insert
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (of-type tree:node (tree:insert tree 42))
    (is = 42 (tree:find tree 42))))

(define-test bst/delete
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (false (tree:delete tree 1))
    (dotimes (i 1000)
      (tree:insert tree (1+ i)))
    (true (tree:delete tree 1))
    (false (tree:find tree 1))
    (setf tree (tree:make-tree 'tree:binary-search-tree :key #'cdr))
    (tree:insert tree '(:foo . 1))
    (dotimes (i 1000)
      (tree:insert tree `(:bar . ,(+ 2 (random 1000.0)))))
    (true (tree:delete tree 1))
    (false (tree:find tree 1))))

(define-test bst/min
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (false (tree:min tree))
    (dotimes (i 1000)
      (tree:insert tree (1+ i)))
    (is = 1 (tree:min tree))
    (setf tree (tree:make-tree 'tree:binary-search-tree :key #'cdr))
    (dotimes (i 200)
      (tree:insert tree `(:foo . ,(+ 200 (random 200.0)))))
    (tree:insert tree '(:bar . 199))
    (tree:insert tree '(:baz . 50))
    (dotimes (i 500)
      (tree:insert tree `(:qux . ,(+ 400 (random 500.0)))))
    (is = 50 (cdr (tree:min tree)))
    (tree:delete tree 50)
    (is = 199 (cdr (tree:min tree)))))

(define-test bst/max
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (false (tree:max tree))
    (dotimes (i 1000)
      (tree:insert tree (1+ i)))
    (is = 1000 (tree:max tree))
    (setf tree (tree:make-tree 'tree:binary-search-tree :key #'cdr))
    (dotimes (i 200)
      (tree:insert tree `(:foo . ,(random 200.0))))
    (tree:insert tree '(:bar . 700))
    (tree:insert tree '(:baz . 999))
    (dotimes (i 500)
      (tree:insert tree `(:qux . ,(+ 200 (random 500.0)))))
    (is = 999 (cdr (tree:max tree)))
    (tree:delete tree 999)
    (is = 700 (cdr (tree:max tree)))))

(define-test bst/previous
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (dotimes (i 100)
      (tree:insert tree (1+ i)))
    (is = 99 (tree:previous (nth-value 1 (tree:find tree 100))))
    (tree:delete tree 99)
    (is = 98 (tree:previous (nth-value 1 (tree:find tree 100))))
    (false (tree:previous (nth-value 1 (tree:find tree 1))))))

(define-test bst/next
  (let ((tree (tree:make-tree 'tree:binary-search-tree)))
    (dotimes (i 100)
      (tree:insert tree (1+ i)))
    (is = 2 (tree:next (nth-value 1 (tree:find tree 1))))
    (tree:delete tree 2)
    (is = 3 (tree:next (nth-value 1 (tree:find tree 1))))
    (false (tree:next (nth-value 1 (tree:find tree 100))))))
