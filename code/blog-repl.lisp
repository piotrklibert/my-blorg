(in-package :cl-user)
(proclaim
 '(optimize (debug 3) (safety 3) (speed 1) (space 1) (compilation-speed 1)))
(require :asdf)
;; (asdf:load-system "cl-env")
(ql:quickload :blog)
(ql:quickload :swank)
(ql:quickload :printv)

;; Limit the maximum default output.
(setf *print-lines* 1000)
(setf *print-level* 20)
(setf *print-length* 1000)
(setf *print-right-margin* 80)

;; Pretty-print hash-tables by default.
;; Enable/disable with toggle-pretty-print-hash-table
(serapeum:toggle-pretty-print-hash-table t)



(uiop:define-package #:blog-user
  (:use :cl :env.full))

(in-package :blog-user)
(in-readtable :env.readtable)
;; Should happen after this file is fully loaded, but don't count on it.
(when (or swank-loader:*started-from-emacs* swank::*emacs-connection*)
  (swank:eval-in-emacs '(slime-repl-set-package "BLOG-USER") t))


(defun all-user-help ()
  "Print a short welcome and help message."
  (format t "AweListLint version ~a~&" (asdf:system-version (asdf:find-system "awelistlint")))
  (format t "Documentation: "))

(defconst +swank-user-init+ "/home/cji/.swank.lisp")


;; If a fasl was stale, try to recompile and load (once).
(eval-when (:load-toplevel)
  (defmethod asdf:perform :around
    ((o asdf:load-op) (c asdf:cl-source-file))
    (handler-case (call-next-method o c)
      ;; If a fasl was stale, try to recompile and load (once).
      (sb-ext:invalid-fasl ()
        (asdf:perform (asdf:make-operation 'asdf:compile-op) c)
        (call-next-method)))))


;; Set up the editor function for SBCL, usage:
(example (ed :clesh))
(when (or swank-loader:*started-from-emacs* swank::*emacs-connection*)
  #+sbcl
  (progn
    (defun edfn (arg)
      (when (keywordp arg)
        (swank:ed-in-emacs
         (accesses (find-package arg)
           'sb-impl::source-location
           'sb-c:definition-source-location-namestring)))
      ;; returning nil means the object was not found, so other functions in
      ;; sb-ext:*ed-functions* will be tried.
      t)

    (pushnew #'edfn sb-ext:*ed-functions*)))
