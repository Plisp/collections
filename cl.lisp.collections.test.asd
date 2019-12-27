(asdf:defsystem #:cl.lisp.collections.test
  :description ""
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/collections"
  :source-control (:git "https://github.com/mfiano/collections.git")
  :bug-tracker "https://github.com/mfiano/collections/issues"
  :encoding :utf-8
  :depends-on (#:cl.lisp.collections
               #:parachute)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (o c) (uiop:symbol-call :parachute :test :cl.lisp.collections.test))
  :components
  ((:file "package")
   (:file "tree-binary-search")
   (:file "tree-red-black")
   (:file "tree-avl")
   #++(:file "tree-splay")))
