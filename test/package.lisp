(in-package #:cl-user)

(defpackage #:cl.lisp.collections.test
  (:local-nicknames (#:tree #:cl.lisp.collections.tree))
  (:use #:cl
        #:prove))

(setf prove:*enable-colors* t)
