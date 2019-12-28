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
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :perform (asdf:test-op (op c) (funcall (intern #.(string :run) :prove) c))
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:test-file "tree")))
