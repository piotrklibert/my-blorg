;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'my-org-blog)
(require 'my-org-config)

(require 'b)
(require 'f)
(require 'map)
(require 'templatel)
(require 'weblorg)
(require 'my-org-custom-id)

(defconst bl-blog-root "/home/cji/priv/blog/")
(add-to-list 'load-path (f-join bl-blog-root "src/"))
(require 'bl-statics)
(require 'bl-export)
(require 'bl-new-post)
(require 'bl-nginx)
(require 'my-threading)

(defconst bl-template-paths (list (expand-file-name "templates" bl-blog-root))
  "List of directories that contain our templates.")

(defconst bl-post-paths (list (expand-file-name "posts" bl-blog-root))
  "List of directories that contain our posts.")

(cl-defun bl-signal-not-found (name directories)
  (let ((user-msg (format "File `%s' not found in any of: %S" name directories)))
    (signal 'file-missing (list "" "File not found" user-msg))))

;; (bl-find-file-on-path bl-template-paths "base.html")
;; (bl-find-file-on-path bl-template-paths "base.htmlasd")
;; (bl-find-file-on-path bl-template-paths "/home/cji/priv/blog/templates/base.html")
;; (bl-find-file-on-path '() "base.html")
(cl-defun bl-find-file-on-path (directories name)
  (when (f-absolute-p name)
    (cl-return-from bl-find-file-on-path name))
  (cl-assert (listp directories) t "Directories must be a list")
  (cl-assert (not (null directories)) nil "Directories must not be empty")
  (dolist (dir directories)
    (let ((path (f-join dir name)))
      (when (and (f-exists-p path) (f-file-p path))
        (cl-return-from bl-find-file-on-path path))))
  (bl-signal-not-found name directories))

;; (bl-find-template "base.html")
(defalias 'bl-find-template
  (apply-partially #'bl-find-file-on-path bl-template-paths))

;; (bl-find-post "abomination-found-in-code.org")
(defalias 'bl-find-post
  (apply-partially #'bl-find-file-on-path bl-post-paths))


(defun bl-template-import (env name)
  "Import template NAME within environment ENV."
  (->> (templatel-new-from-file (bl-find-template name))
    (templatel-env-add-template env name)))

(cl-defstruct bl-post-data
  path
  raw-content
  keywords
  content)
;; (make-bl-post-data :path nil :raw-content nil :keywords nil :content nil)

;; (bl-get-post-export "abomination-found-in-code.org")
(defun bl-get-post-export (post-name)
  (save-selected-window
    (save-excursion
      (let* ((post (bl-find-post post-name))
             (keywords nil)
             (exported-buf (with-temp-buffer
                             ;; (setq-local buffer-file-name post)
                             (setq-local org-attach-id-dir (f-join bl-blog-root "posts/data/"))
                             (insert (f-read post))
                             (setq keywords (my-collect-org-keywords))
                             (my-org-html-export-as-html))))
        (prog1 (make-bl-post-data
                :content (b-string-no-properties exported-buf)
                :keywords keywords)
          (kill-buffer exported-buf))))))


(cl-defun bl-map-post-buffers (fn)
  (let ((fn (lambda (post)
              (with-temp-buffer
                (insert-file-contents post)
                (cons post (funcall fn))))))
    (mapcar fn (bl-get-post-list))))


(cl-defun bl-state-publishable-p (state)
  (pcase state
    ('nil t)
    ((pred (not stringp)) nil)
    ((app s-downcase (or "draft" "hidden" "wip")) nil)
    (_ t)))

;; (bl-assemble-publishable-posts)
(cl-defun bl-assemble-publishable-posts ()
  (let ((posts (bl-map-post-buffers #'my-collect-org-keywords)))
    (cl-loop for (post . keywords) in posts
             if (bl-state-publishable-p (map-elt keywords "state"))
             collect post)))

(defun my-collect-org-keywords ()
  "Collect all keywords in the current Org buffer."
  (interactive)
  (let ((parsed-org (org-element-parse-buffer))
        keywords)
    (org-element-map parsed-org 'keyword
      (lambda (element)
        (let ((key (s-downcase (org-element-property :key element)))
              (value (org-element-property :value element)))
          (push (cons key value) keywords))))
    keywords))


;; (bl-render-post "abomination-found-in-code.org")
;; (bl-render-post "select-other-dired-window.org")
;; (bl-render-post "blog-reorganization-announcement.org")
(cl-defun bl-render-post (post-name &optional (template "post.html") (write-page t))
  (interactive (list (buffer-file-name)))
  (let* ((env (templatel-env-new :importfn 'bl-template-import))
         (template-body (templatel-new-from-file (bl-find-template template)))
         (post-data (bl-get-post-export post-name))
         (post-content (bl-post-data-content post-data))
         (post-keywords (bl-post-data-keywords post-data))
         (post-title (cdr (assoc "title" post-keywords)))
         (post-slug (weblorg--slugify post-title))
         (post-url (concat "/posts/" post-slug ".html")))
    (when (bl-state-publishable-p (map-elt post-keywords "state"))
      (message "Rendering post: %s" post-title)
      (templatel-env-set-autoescape env t)
      (templatel-env-add-template env template template-body)
      (let ((rendered-post (templatel-env-render
                            env template
                            `(("content" . ,post-content)
                              ("slug" . ,post-slug)
                              ("post_url" . ,post-url)
                              ,@post-keywords))))
        (when write-page
          (with-temp-file (f-join bl-blog-root "build/posts/" (concat post-slug ".html"))
            (insert rendered-post)))
        rendered-post))))

(cl-defun bl-get-post-list ()
  (let ((post-list nil))
    (dolist (path bl-post-paths)
      (dolist (post (directory-files path))
        (when (s-ends-with? ".org" post)
          (push (f-join path post) post-list))))
    post-list))

;; (bl-render-index)
(cl-defun bl-render-index ()
  (let* ((env (templatel-env-new :importfn 'bl-template-import))
         (template "index.html")
         (template-body (templatel-new-from-file (bl-find-template template)))
         (post-list (bl-assemble-publishable-posts))
         (org-attach-id-dir (f-join bl-blog-root "posts/data/"))
         posts)
    (templatel-env-set-autoescape env t)
    (templatel-env-add-template env template template-body)
    (dolist (post post-list)
      (push (cons (map-elt (bl-post-data-keywords (bl-get-post-export post)) "date")
                  (bl-render-post post "raw-post.html" nil))
            posts))
    (setq posts (mapcar #'cdr (sort posts (lambda (a b) (string< (car b) (car a))))))
    (with-temp-file (f-join bl-blog-root "build/index.html")
      (insert (templatel-env-render env template `(("posts" . ,posts)))))))


(cl-defun bl-upload-files ()
  (interactive)
  (mt-in-thread
   (mt-shell "rsync -Lcrav %s/build/ linode:www" bl-blog-root)))


(cl-defun bl-render-all ()
  "Render all posts and the index."
  (interactive)
  (save-some-buffers)
  (bl-render-index)
  (bl-copy-data-files)
  (dolist (post (bl-get-post-list))
    (bl-render-post post)))

(defvar bl-render-all-async-active nil)
(cl-defun bl-render-all-async ()
  "Render all posts and the index using a background Emacs process."
  (interactive)
  (save-some-buffers)
  (unless bl-render-all-async-active
    (setq bl-render-all-async-active t)
    (mt-in-thread
     (unwind-protect
         (mt-process
          '("nice" "-n" "19"
            "emacs" "--iconic" "-l" "build.el"
            "--eval" "(progn (bl-render-all) (kill-emacs))"))
       (setq bl-render-all-async-active nil)
       (mt-message "Export done.")))))


(cl-defun bl-make-all ()
  (interactive)
  (bl-render-all)
  (bl-upload-files))


;; Local Variables:
;; read-symbol-shorthands: (("mt-" . "my-threading-"))
;; End:
