(in-package #:cl-user)

(defpackage #:cl.lisp.collections.test
  (:local-nicknames (#:bst #:cl.lisp.collections.bst)
                    (#:rbt #:cl.lisp.collections.rbt))
  (:use #:cl
        #:parachute))
