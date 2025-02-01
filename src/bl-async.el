;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'async)
(require 'async-await)
(require 'generator)
(require 'my-utils)
(require 'promise)


(setf async-child-init "/home/cji/.emacs.d/config/async-setup.el")

(defconst bl-build-script-path "/home/cji/priv/blog/build.el")


(cl-defun bl-run-shell (command-line)
  (setq command-line (cl-etypecase command-line
                       (string command-line)
                       (list (s-join " " command-line))))
  (bl-run-program "bash" (list "-c" command-line)))


;; (bl-run-program "ls" '("-l"))
;; (bl-run-program 9 '("-l"))
(cl-defun bl-run-program (program args)
  (cl-check-type program string)
  (cl-check-type args list)
  (let ((default-directory bl-blog-root))
    (promise:make-process-with-handler (cons program args) nil t)))
(put 'bl-run-program 'lisp-indent-function 1)


(cl-defun bl-run-program* (program &rest args)
  (bl-run-program program args))



;;; Rsync

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



(cl-defun bl-start-background-render ()
  "Start a background Emacs process to render all posts and the index. It starts
minimized, but it cannot be headless, because then the colors (eg. for syntax
highlighting) are not rendered during export."
  (bl-run-program*
   "emacs" "--iconic"
   "-l" bl-build-script-path
   "--eval" "(progn (bl-render-all) (kill-emacs))"))

(defvar bl-render-all-is-running-p nil)
(async-defun bl-render-all-async-1 ()
  (save-some-buffers)
  (unless bl-render-all-is-running-p
    (setq bl-render-all-is-running-p t)
    (unwind-protect
        (await (bl-start-background-render))
      (setq bl-render-all-is-running-p nil))
    (message "Rendered all posts and index.")))

(cl-defun bl-render-all-async ()
  "Render all posts and the index using a background Emacs process."
  (interactive)
  (bl-render-all-async-1))


;; (bl-upload-files-async)
(cl-defun bl-upload-files-async ()
  (interactive)
  (let* ((src-root (f-join bl-blog-root "build/"))
         (promise (bl--async-rsync src-root "linode:www"
                                   :flags '("-Lcrav" "--dry-run"))))
    (promise-then promise
      (lambda (res) (let ((msg (-> res (map-elt :message) (my-indent-lines 4))))
                 (message "Upload done:\n%s" msg)))
      (lambda (reason) (message "Upload failed: %s" reason)))))


(provide 'bl-async)
