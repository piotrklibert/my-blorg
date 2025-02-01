;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defconst bl-docker-image "openresty/openresty:latest")
(defconst bl-docker-container "blog-nginx")
(defconst bl-docker-port 32769)
(defconst bl-docker-mount "/site")
(defconst bl-docker-conf "/etc/nginx/conf.d/default.conf")
(defconst bl-nginx-conf "/home/cji/priv/blog/server/server.conf")


(cl-defun bl-docker-mount-format (from to &optional (ro t))
  (unless (f-absolute-p from)
    (setq from (f-join bl-blog-root from)))
  (format "%s:%s:%s" from to (if ro "ro" "rw")))


(defconst bl-docker-run-cmd
  (list "docker" "run" "--rm" "--detach"
        "--name" bl-docker-container
        "-v" (bl-docker-mount-format bl-nginx-conf bl-docker-conf)
        "-v" (bl-docker-mount-format "build" bl-docker-mount nil)
        "-p" "80:80"
        bl-docker-image))

;; (bl-start-serving-files)
(cl-defun bl-start-serving-files ()
  (interactive)
  (start-process-shell-command "run-docker" "*run-docker*"
    (s-join " " bl-docker-run-cmd))
  (message "Serving files at http://localhost:80 - use M-x docker to stop it"))

;; (bl-stop-serving-files)
(cl-defun bl-stop-serving-files ()
  (interactive)
  (start-process-shell-command "stop-docker" "*stop-docker*"
    (format "docker stop %s" bl-docker-container)))

(provide 'bl-nginx)
