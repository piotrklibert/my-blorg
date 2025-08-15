;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'my-utils)
(require 'async-await)
(require 'promise)
(require 'generator)
(require 'bl-async)


(defconst bl-copy-paths-spec
  '(("posts/*.el"                      "build/posts/")
    ;; glob ^^^^ patterns resolved by the shell
    ("posts/data"                      "build/posts/")
    ("statics"                         "build/")
    ("../klibert_pl/articles/*.html"   "build/pages/" "-vac" "--exclude='*raw*'")
    ;;                               optional flags:  ^^^^^^^^^^^^^^^^^^^^^^^^^^
    ("../klibert_pl/output/"           "build/legacy")
    ;;^^ paths relative to `bl-blog-root' (ie. /home/cji/priv/blog/)
    ("../klibert_pl/org/timeline.png"  "build/statics/")
    ("../klibert_pl/statics/smalltalk" "build/statics/")))

(defvar bl-copy-data-verbose-logging t)

(comment
 (let ((args (car bl-copy-paths-spec)))
   (promise-chain (bl--async-rsync (car args) (cadr args) :flags "-crav")
     (thena (message "res: %S" result))
     (catcha (message "err: %S" reason)))))



(async-defun bl-copy-data-files-sequential ()
  (let* ((paths bl-copy-paths-spec)
         (fails (cl-loop for (from to . flags) in paths
                         do (message "Copying %s to %s" from to)
                         for res = (await (bl--async-rsync
                                           from to
                                           :flags (if flags flags "-crav")))
                         do (when bl-copy-data-verbose-logging
                              (message "res: %s \n" (plist-get res :message)))
                         when (not (plist-get res :success))
                         collect res)))
    (if fails (promise-reject fails) (promise-resolve t))))

(async-defun bl-copy-data-files-concurrent ()
  (cl-labels ((make-rsync-promise (from to flags)
                (promise-then (bl--async-rsync from to :flags flags)
                  (lambda (res)
                    (when bl-copy-data-verbose-logging
                      (message "Copying %s to %s" from to)
                      (message "res: %s \n" (or (map-elt res :message)
                                                (map-elt res :reason))))
                    res))))
    (let* ((paths bl-copy-paths-spec)
           (promises (cl-loop for (from to . flags) in paths
                              for flags = (if flags flags "-rav")
                              collect (make-rsync-promise from to flags)))
           (results (await (promise-all promises)))
           (fails (cl-loop for res in results
                           when (not (plist-get res :success))
                           collect res)))
      (if fails (promise-reject fails) (promise-resolve t)))))



(cl-defun bl-copy-data--format-error (fail)
  (message "  %s: %s\n\n%s\n    ======\n"
           (concat "rsync" (plist-get fail :cmd))
           (plist-get fail :reason)
           (my-indent-lines
            (plist-get fail :message))))


;; (bl-copy-data-files)
(cl-defun bl-copy-data-files ()
  (interactive)
  (message "Copying data files...\n")
  (promise-then (bl-copy-data-files-concurrent)
    (lambda (_) (message "✅ All files transferred!"))
    (lambda (fails)
      (message "🧨 Failed to copy some data files: 🧨\n")
      (cl-loop for f in fails do (bl-copy-data--format-error f)))))


(provide 'bl-statics)

;; Local Variables:
;; read-symbol-shorthands: (("mt-" . "my-threading-"))
;; End:
