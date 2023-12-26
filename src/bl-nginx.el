;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defconst bl-docker-image "openresty/openresty:alpine-fat")
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
  (list
   "docker" "run" "--rm" "--name" bl-docker-container
   "-v" (bl-docker-mount-format bl-nginx-conf bl-docker-conf)
   "-v" (bl-docker-mount-format "build" bl-docker-mount)
   "-p" "32769:80"
   bl-docker-image))
;; (s-join " " bl-docker-run-cmd)
;; docker run --rm --name blog-nginx -v /home/cji/priv/blog/server/server.conf:/etc/nginx/conf.d/default.conf:ro -v /home/cji/priv/blog/build:/site:ro -p 32769:80 openresty/openresty:alpine-fat


(provide 'bl-nginx)
