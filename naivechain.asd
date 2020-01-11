(defsystem :cl-naivechain
  :class :package-inferred-system
  :version "0.0.1"
  :author "ihuku"
  :license "Public Domain"
  :depends-on ("ironclad" "swank-client" "hunchentoot")
  :components ((:file "naivechain")))
