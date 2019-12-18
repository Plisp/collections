(asdf:defsystem #:cl.lisp.collections
  :description ""
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/collections"
  :source-control (:git "https://github.com/mfiano/collections.git")
  :bug-tracker "https://github.com/mfiano/collections/issues"
  :encoding :utf-8
  :depends-on (#:alexandria
               #:golden-utils)
  :pathname "src"
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op #:cl.lisp.collections.test)))
  :components
  ((:file "package")
   (:file "binary-search-tree")))
