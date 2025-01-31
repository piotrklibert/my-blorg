;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'async)


(setf async-child-init "/home/cji/.emacs.d/config/async-setup.el")

(cl-defun bl ()
  (async-start
  (lambda ()
    (load "/home/cji/priv/blog/build.el"))
  (lambda (result) (message "Async setup done: %s" result))))

(defvar bl-render-all-async-active nil)
(cl-defun bl-render-all-async ()
  "Render all posts and the index using a background Emacs process."
  (interactive)
  (save-some-buffers)
  (unless bl-render-all-async-active
    (setq bl-render-all-async-active t)
    (mt-in-thread
     (unwind-protect
         (mt-process
          '("nice" "-n" "19"
            "emacs" "--iconic" "-l" "build.el"
            "--eval" "(progn (bl-render-all) (kill-emacs))"))
       (setq bl-render-all-async-active nil)
       (mt-message "Export done.")))))

(provide 'bl-async)
