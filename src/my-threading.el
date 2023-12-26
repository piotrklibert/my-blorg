;; -*- mode: emacs-lisp; lexical-binding: t; read-symbol-shorthands: (("my-threading-" . "my-threading-") ("imp-" . "my-threading-imports-")); -*-
(require 'cl-lib)
(require 'named-timer)
(require 'b)
(require 's)
(require 'dash)


(defvar bl-blog-root)

;; TODO: use a queue instead of a list for the mailbox
;; TODO: use a mutex for the mailbox
(defvar my-threading-mailbox nil)


;; (my-threading-dispatcher)
(cl-defun my-threading-dispatcher ()
  "A function called on the main thread from a timer. It executes function calls
from the mailbox, queued there by other threads."
  (when my-threading-mailbox
    (pcase-let ((`(,thread ,fun ,args) (pop my-threading-mailbox)))
      (condition-case err
          (apply fun args)
        (error (message "Execution of %S from thread %s died with error: %s"
                        (cl-list* fun args) thread err))))))

(cl-defun my-threading-stop ()
  "Stop the main thread dispatcher. See `my-threading-dispatcher'."
  (interactive)
  (named-timer-cancel :recv))

;; (my-threading-on-main (lambda () (message "hello")))
(cl-defun my-threading-on-main (fun &rest args)
  "Execute FUN on the main thread at some later time. FUN is called with ARGS."
  (setq my-threading-mailbox (-snoc my-threading-mailbox `(,(current-thread) ,fun ,args))))

(cl-defun my-threading-message (fmt &rest args)
  "Like `message', but goes through `my-threading-on-main', which see."
  (apply #'my-threading-on-main #'message fmt args))

(cl-defun my-threading-thread (fun)
  "Start a new thread that executes FUN. Start `my-threading-dispatcher' if needed."
  (unless (named-timer-get :recv)
    (named-timer-run :recv 1 1 #'my-threading-dispatcher))
  (cl-labels
      ((wrapped ()
         (condition-case err
             (funcall fun)
           (error (message "Thread %s died with error: %s"
                           (current-thread) err)))))
    (with-current-buffer (get-buffer-create "*thread*")
      (make-thread #'wrapped))))
;; (my-threading-thread (lambda () (my-threading-on-main (lambda (a) (message "hello %s %s" a (current-thread))) (current-thread))))

(cl-defmacro my-threading-run-in-thread (&rest body)
  "Execute BODY in a new thread. See `my-threading-thread'."
  `(my-threading-thread (lambda () ,@body)))

(cl-defun my-threading-process (command)
  (let* ((finished nil)
         (buf (generate-new-buffer (concat " *" (symbol-name 'my-threading-process) "*")))
         (default-directory bl-blog-root)
         (proc (make-process
                :name "test"
                :command command
                :buffer buf
                :sentinel (lambda (_process event)
                            (setq finished (s-trim event))))))
    (unwind-protect
        (progn (while (not finished) (accept-process-output proc 0 50))
               (if (string= finished "finished")
                   (b-string-no-properties buf)
                 finished))
      (kill-buffer buf))))

(cl-defun my-threading-shell (cmd &rest args)
  (my-threading-process (list "bash" "-c" (s-trim (apply #'format cmd args)))))

;; (my-threading-run-in-thread
;;  (let ((cmd "echo %s") (args '("s")))
;;    (my-threading-message "%S" (s-trim (apply #'my-threading-shell cmd args)))))

;; (my-threading-shell-async "echo %s %s" "hello" "world")
(cl-defun my-threading-shell-async (cmd &rest args)
  (my-threading-run-in-thread
   (my-threading-message "%S" (apply #'my-threading-shell cmd args))))


(provide 'my-threading)

;; Local Variables:
;; eval: (add-to-list 'read-symbol-shorthands '("mt-" . "my-threading-"))
;; End:
