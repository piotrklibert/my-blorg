;; -*- mode: emacs-lisp; lexical-binding: t -*-


(defconst bl-docker-image "openresty/openresty:latest")
(defconst bl-docker-container "blog-nginx")
(defconst bl-docker-port 32769)
(defconst bl-docker-mount "/site")
(defconst bl-docker-conf "/etc/nginx/conf.d/default.conf")
(defconst bl-nginx-conf "/home/cji/priv/blog/server/server.conf")


(cl-defun bl-nginx--format-volume (from to &optional (ro t))
  (unless (f-absolute-p from)
    (setq from (f-join bl-blog-root from)))
  (format "%s:%s:%s" from to (if ro "ro" "rw")))


(defconst bl-docker-run-cmd
  (list "docker" "run" "--rm" "--detach"
        "--name" bl-docker-container
        "-v" (bl-nginx--format-volume bl-nginx-conf bl-docker-conf)
        "-v" (bl-nginx--format-volume "build" bl-docker-mount nil)
        "-p" "80:80"
        bl-docker-image))



;; (promise-catch (bl-nginx-start) (lambda (err) (message "%s" err)))
(cl-defun bl-nginx-start ()
  (interactive)
  (promise-then (bl-run-shell (s-join " " bl-docker-run-cmd))
    (lambda (_)
      (message "%s" (concat "Serving files at http://localhost:80 - \n"
                            "use M-x bl-nginx-stop    to stop the server, \n"
                            "or  M-x bl-nginx-restart to reload config, \n"
                            "or  M-x docker           to monitor the container.")))
    (-lambda ((event stdout stderr))
      (promise-reject
       (format "Error starting Docker container:\n%s" stdout)))))


;; (promise-catch (bl-nginx-stop) (lambda (err) (message "%s" err)))
(cl-defun bl-nginx-stop ()
  (interactive)
  (promise-then (bl-run-shell (format "docker stop %s" bl-docker-container))
    (lambda (_)
      (message "Docker container stopped."))
    (-lambda ((event stdout stderr))
      (promise-reject
       (format "Error stopping Docker container:\n%s" stdout)))))



;; (bl-nginx-restart-1)
(async-defun bl-nginx-restart-1 ()
  (message "Restarting Nginx...")
  (ignore-errors
    (await (bl-nginx-stop)))
  (condition-case err
      (await (bl-nginx-start))
    (error (message "%s" (cl-second err)))))


;; (bl-nginx-restart)
(defun bl-nginx-restart ()
  "Restart the Docker container with Nginx - can take a few seconds."
  (interactive)
  (bl-nginx-restart-1))


;; (bl-nginx-reload)
;; (defun bl-nginx-reload ()  ;; doesn't work with the Docker image I'm using
;;   (interactive)
;;   (start-process-shell-command "reload-nginx" "*reload-nginx*"
;;     (format "docker exec -ti %s nginx -s reload" bl-docker-container)))


(provide 'bl-nginx)
