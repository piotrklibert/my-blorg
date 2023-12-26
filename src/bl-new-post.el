;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'f)
(require 'dash)
(require 'yasnippet)
(require 'my-org-custom-id)


(defvar bl-blog-root)

(defconst bl-posts-dir (f-join bl-blog-root "posts/"))


(cl-defun bl-new-post (name)
  "Create a new post with the given name. Interactively, ask for the name."
  (interactive "sPost (file) name: ")
  (unless (s-ends-with-p ".org" name)
    (setq name (concat name ".org")))
  (let ((fpath (f-join bl-posts-dir name)))
    (switch-to-buffer-other-window (find-file-noselect fpath))
    (setq-local default-directory bl-posts-dir)
    (save-buffer)
    (org-mode)
    (bl-render-on-save)
    (yas-expand-snippet (yas-lookup-snippet "post"))))

(cl-defun bl-after-save-render-hook ()
  (bl-render-post (buffer-file-name)))

(cl-defun bl-render-on-save ()
  "Enable render on save in the current buffer."
  (interactive)
  (add-hook 'before-save-hook #'be-regenerate-custom-ids nil t)
  (add-hook 'after-save-hook #'bl-after-save-render-hook nil t)
  )


(provide 'bl-new-post)

;; Local Variables:
;; End:
