;; -*- mode: emacs-lisp; lexical-binding: t -*-

(require 'async-await)
(require 'promise)

(cl-defun bl-async-rsync (cmd)
  (let ((default-directory bl-blog-root))
    (promise-then
        (promise:make-process-with-handler (list "bash" "-c" cmd) nil t)
      (lambda (result)
        (promise-resolve (list :success t :message result :reason nil :cmd cmd)))
      (lambda (reason)
        (promise-resolve (list :success nil
                               :cmd cmd
                               :reason (s-trim (car reason))
                               :message (cadr reason)))))))


(async-defun bl-copy-data-files-1 ()
  (let ((res (await (promise-all
                     (list (bl-async-rsync "rsync -av posts/*.el build/posts/")
                           (bl-async-rsync "rsync --dry-run -crav posts/data build/posts/")
                           (bl-async-rsync "rsync --dry-run -crav statics build/"))))))
    (let ((fails (cl-loop for r across res when (not (plist-get r :success))
                          collect r)))
      (if fails
          (promise-reject fails)
        (promise-resolve t)))))

(cl-defun bl-copy-data--format-error (fail)
  (let ((msg (->> (plist-get fail :message) (s-split "\n") (--map (s-concat "    " it)) (s-join "\n"))))
    (message "  %s: %s\n\n%s\n    ======\n"
             (plist-get fail :cmd)
             (plist-get fail :reason)
             msg)))

(cl-defun bl-copy-data-files ()
  (interactive)
  (message "Handling data files...\n")
  (promise-then (bl-copy-data-files-1)
    (lambda (_) (message "All files transferred."))
    (lambda (fails)
      (message "Failed to copy some data files:\n")
      (cl-loop for f in fails do (bl-copy-data--format-error f)))))


(provide 'bl-statics)

;; Local Variables:
;; read-symbol-shorthands: (("mt-" . "my-threading-"))
;; End:
