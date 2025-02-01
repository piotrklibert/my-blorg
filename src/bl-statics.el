;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'my-utils)
(require 'async-await)
(require 'promise)
(require 'generator)

(cl-defun bl--async-wrap-rsync-result (rsync-promise cmd)
  (promise-then rsync-promise
    (lambda (res)
      (seq-let (stdout stderr) res
        (promise-resolve
         (list :success t :message stdout :reason nil :cmd cmd))))
    (lambda (err)
      (seq-let (event stdout stderr) err
        (promise-resolve
         (list :success nil :cmd cmd :reason (s-trim event) :message stdout))))))


;; (bl--async-rsync (list "posts/*.el" "posts/*.css") "build/posts/")
;; (bl--async-rsync "posts/*.el" "build/posts/" :flags '("-v" "-a"))
;; (bl--async-rsync "posts/*.el" "build/posts/" :flags "-a")
(cl-defun bl--async-rsync (source dest &key (flags '("-av")))
  (cl-assert (not (listp dest)) t "can be only one destination")
  (let ((args (append (ensure-list flags)
                      (ensure-list source)
                      (list dest))))
    (-> (bl-run-shell (cons "rsync" args))
      (bl--async-wrap-rsync-result (prin1-to-string args)))))


(defconst bl-copy-paths-spec
  '(("posts/*.el" "build/posts/")
    ("posts/data" "build/posts/")
    ("statics" "build/")
    ("../klibert_pl/articles/*.html" "build/pages/" "--exclude=*raw*")
    ("../klibert_pl/output/" "build/legacy")
    ("../klibert_pl/org/timeline.png" "build/statics/")
    ("../klibert_pl/statics/smalltalk" "build/statics/")
    ))

(defvar bl-copy-data-verbose-logging t)

(async-defun bl-copy-data-files-1 ()
  (let* ((paths bl-copy-paths-spec)
         (fails (cl-loop for (from to . flags) in paths
                         do (message "Copying %s to %s" from to)
                         for res = (await (bl--async-rsync
                                           from to
                                           :flags (if flags flags "-crav")))
                         do (when bl-copy-data-verbose-logging
                              (message "res: %S" (plist-get res :message)))
                         when (not (plist-get res :success))
                         collect res)))
    (if fails (promise-reject fails) (promise-resolve t))))


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
  (promise-then (bl-copy-data-files-1)
    (lambda (_) (message "✅ All files transferred!"))
    (lambda (fails)
      (message "🧨 Failed to copy some data files: 🧨\n")
      (cl-loop for f in fails do (bl-copy-data--format-error f)))))


(provide 'bl-statics)

;; Local Variables:
;; read-symbol-shorthands: (("mt-" . "my-threading-"))
;; End:
