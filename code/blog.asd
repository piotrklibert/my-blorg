(in-package :asdf-user)

(defsystem "blog"
  :depends-on (#:cl-env)
  :serial t
  :pathname "common-lisp/"
  :components
  ((:file "legacy")))
