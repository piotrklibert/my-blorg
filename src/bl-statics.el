;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'my-threading)

(cl-defun bl-copy-data-files ()
  (interactive)
  (mt-thread
   (lambda ()
     (mt-message
      (concat
       "Handling data files...\n"
       (mt-shell "rsync -cav posts/*.el build/posts/")
       (mt-shell "rsync -crav posts/data build/posts/")
       (mt-shell "rsync -crav statics build/")
       "Handling data files...done\n")))))


(provide 'bl-statics)

;; Local Variables:
;; read-symbol-shorthands: (("mt-" . "my-threading-"))
;; End:
