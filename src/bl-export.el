;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; NOTE: `org-attach-use-inheritance' value of 'selective means to defer to the
;; other variable `org-use-property-inheritance' to determine which properties
;; should be inherited. `org-attach' is only insterested in `ID', so in
;; principle this:
;;
;;   (setq org-attach-use-inheritance 'selective
;;         org-use-property-inheritance '("ID"))
;;
;; would also work. However, setting `org-attach-use-inheritance' has
;; performance consequences. Enabling inheritance for `org-attach' only looks
;; like a better choice.

(use-package org-attach
  :custom
  (org-attach-use-inheritance t)
  (org-attach-dir-relative t))

;; (setq org-attach-use-inheritance t)
;; (setq org-attach-dir-relative t)        ; we don't use DIR, though

;; See: `org-attach-expand-links'
(defun my-org-attach-expand-links-override (_)
  "Expand links in current buffer.
It is meant to be added to `org-export-before-parsing-hook'."
  (save-excursion
    (while (re-search-forward "attachment:" nil t)
      (let ((link (org-element-context)))
	    (when (and (org-element-type-p link 'link)
		           (string-equal "attachment"
				                 (org-element-property :type link)))
	      (let* ((description (and (org-element-contents-begin link)
				                   (buffer-substring-no-properties
				                    (org-element-contents-begin link)
				                    (org-element-contents-end link))))
		         (file (org-element-property :path link))
		         (new-link (org-link-make-string
			                (s-replace "/home/cji/priv/blog/posts/" "./" (org-attach-expand file))
			                description)))
	        (goto-char (org-element-end link))
	        (skip-chars-backward " \t")
	        (delete-region (org-element-begin link) (point))
	        (insert new-link)))))))

;; (add-hook 'org-export-before-parsing-hook #'my-org-attach-expand-links-override)
(advice-add 'org-attach-expand-links :override #'my-org-attach-expand-links-override)
;; (advice-remove 'org-attach-expand-links #'my-org-attach-expand-links-override)
;; org-export-before-parsing-hook

(provide 'bl-export)
