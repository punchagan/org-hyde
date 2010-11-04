;;; org-hyde.el --- Export hyde-ready posts from org-mode entries
;;;
;;; Author: Puneeth Chaganti <punchagan+org-hyde@gmail.com>
;;; 
;;; org-hyde is a port of org-jekyll -- 
;;;
;;; Author: Juan Reyero
;;; Version: 0.3
;;; Home page: http://juanreyero.com/open/org-jekyll/
;;; Repository: http://github.com/juanre/org-jekyll
;;; Public clone: git://github.com/juanre/org-jekyll.git
;;; 
;;; Summary
;;; -------
;;; 
;;; Extract subtrees from your org-publish project files that have
;;; a :blog: keyword and an :on: property with a timestamp, and
;;; export them to a subdirectory _posts of your project's publishing
;;; directory in the year-month-day-title.html format that Hyde
;;; expects.  Properties are passed over as yaml front-matter in the
;;; exported files.  The title of the subtree is the title of the
;;; entry.  The title of the post is a link to the post's page.  

(defvar org-hyde-category nil
  "Specify a property which, if defined in the entry, is used as
a category: the post is written to category/_posts. Ignored if
nil. Use \"lang\" if you want to send posts in different
languages to different directories.")


(defvar org-hyde-localize-dir nil
  "If non-nil and the lang property is set in the entry,
org-hyde will look for a lang.yml file in this directory and
include it in the front matter of the exported entry.")

(defvar org-hyde-new-buffers nil
  "Buffers created to visit org-publish project files looking for blog posts.")

(defun org-hyde-publish-dir (project &optional category)
  "Where does the project go, by default a :blog-publishing-directory 
   entry in the org-publish-project-alist."
  (let ((pdir (plist-get (cdr project) :blog-publishing-directory)))
    (unless pdir
      (setq pdir (plist-get (cdr project) :publishing-directory)))
    (concat pdir 
            (if category (concat category "/") "")
            "content/")))

(defun org-hyde-site-root (project)
  "Site root, like http://yoursite.com, from which blog
  permalinks follow.  Needed to replace entry titles with
  permalinks that RSS agregators and google buzz know how to
  follow.  Looks for a :site-root entry in the org-publish-project-alist."
  (or (plist-get (cdr project) :site-root)
      ""))

(defun org-get-hyde-file-buffer (file)
  "Get a buffer visiting FILE.  If the buffer needs to be
  created, add it to the list of buffers which might be released
  later.  Copied from org-get-agenda-file-buffer, and modified
  the list that holds buffers to release."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
        buf
      (progn (setq buf (find-file-noselect file))
             (if buf (push buf org-hyde-new-buffers))
             buf))))

(defun org-hyde-slurp-yaml (fname)
  (remove "---" (if (file-exists-p fname)
                    (split-string (with-temp-buffer
                                    (insert-file-contents fname)
                                    (buffer-string))
                                  "\n" t))))

(defun ensure-directories-exist (fname)
  (let ((dir (file-name-directory fname)))
    (unless (file-accessible-directory-p dir)
      (make-directory dir t)))
  fname)

(defun org-hyde-sanitize-string (str project)
  (if (plist-get (cdr project) :hyde-sanitize-permalinks)
      (progn (setq str (downcase str))
             (dolist (c '(("á" . "a")
                          ("é" . "e")
                          ("í" . "i")
                          ("ó" . "o")
                          ("ú" . "u")
                          ("à" . "a")
                          ("è" . "e")
                          ("ì" . "i")
                          ("ò" . "o")
                          ("ù" . "u")
                          ("ñ" . "n")
                          ("ç" . "s")
                          ("\\$" . "S")
                          ("€" . "E")))
               (setq str (replace-regexp-in-string (car c) (cdr c) str)))
             (setq str (replace-regexp-in-string 
                        "\\(^-\\|-$\\)" "" 
                        (replace-regexp-in-string 
                         "-+" "-" 
                         (replace-regexp-in-string 
                          "[^a-z0-9-]" "" 
                          (replace-regexp-in-string " +" "-" str))))))
    str))

(defun org-hyde-convert-pre (html)
  "Replace pre blocks with syntax blocks for pygments."
  (save-excursion
    (let (pos info params src-re code-re)
      (with-temp-buffer
        (insert html)
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward 
                  "<pre\\(.*?\\)>\\(\\(.\\|[[:space:]]\\|\\\n\\)*?\\)</pre.*?>"
                  nil t 1)
            (setq code (match-string-no-properties 2))
            (if (save-match-data 
                  (string-match "example" (match-string-no-properties 1)))
                (setq lang "html")
              (setq lang (substring 
                          (match-string-no-properties 1) 16 -1))
              ;; handling emacs-lisp separately. pygments raises error when language 
              ;; is unknown. list of languages variable should be added?
              (if (string= "emacs-lisp" lang)
                  (setq lang "common-lisp")))
            (save-match-data
              (setq code (replace-regexp-in-string "<.*?>" "" code)))
            (replace-match 
             (format "\n{%% syntax %s %%}\n%s\n{%% endsyntax %%}" lang code)
             nil t)))
        (setq html (buffer-substring-no-properties (point-min) (point-max))))))
  html)
    
(defun org-hyde-export-entry (project)
  (let* ((props (org-entry-properties nil 'standard))
         (time (or (org-entry-get (point) "POST_DATE")
                   (org-entry-get (point) "SCHEDULED")
                   (org-entry-get (point) "DEADLINE")
                   (org-entry-get (point) "TIMESTAMP_IA")))
         (lang (cdr (or (assoc "lang" props)
                        (assoc "LANG" props))))
         (category (if org-hyde-category
                       (cdr (assoc org-hyde-category props))
                     nil)))
    (when time
      (let* ((heading (org-get-heading t))
             ;; Get the tags from the headline
             (tags (concat "[" 
                           (mapconcat
                            (lambda (tag) tag)
                            (sort 
                             (mapcar
                              'downcase
                              (delete "noexport" 
                                      (delete "blog" 
                                              (org-get-tags-at 
                                               (point) nil)))) 
                             'string<) 
                            ", ")
                           "]" ))
             (title (replace-regexp-in-string "[:=\(\)\?/.&'!#\"]" ""
                                              (replace-regexp-in-string
                                               "[ \t/]" "-" heading)))

             ;; Save the time used as POST_DATE. SCHEDULED etc may change.
             (str-time 
              (format-time-string "%Y-%m-%d %T" 
                                  (if time
                                      (apply 'encode-time 
                                             (org-parse-time-string time))
                                    (current-time)
                                    (org-entry-put (point) 
                                                   "POST_DATE" cur-time))))
             (to-file (format "%s.html"
                              (org-hyde-sanitize-string title project)))
             (org-buffer (current-buffer))
             (yaml-front-matter (cons (cons "title"
                                            (if (string-match "[#:]" heading)
                                                (concat "'" heading "'")
                                              heading)) nil))
             (yaml-front-matter (cons (cons "categories" tags)
                                      yaml-front-matter))
             (yaml-front-matter (cons (cons "created" str-time)
                                      yaml-front-matter))
             html)
        (org-narrow-to-subtree)
        (let ((level (- (org-reduced-level (org-outline-level)) 1))
              (contents (buffer-substring (point-min) (point-max)))
              (site-root (org-hyde-site-root project)))
          ;; Without the promotion the header with which the headline
          ;; is exported depends on the level.  With the promotion it
          ;; fails when the entry is not visible (ie, within a folded
          ;; entry).
          (dotimes (n level nil) (org-promote-subtree))
          (setq html 
                (replace-regexp-in-string 
                 "<h2 id=\"sec-1\">\\(.+\\)</h2>"
                 (concat "<h2 id=\"sec-1\"><a href=\"" site-root
                         "{{ page.url }}\">\\1</a></h2>")
                 (org-export-region-as-html
                   (1+ (and (org-back-to-heading) (line-end-position)))
                   (org-end-of-subtree)
                   t 'string)))
          (set-buffer org-buffer)
          (setq html (org-hyde-convert-pre html))
          (delete-region (point-min) (point-max))
          (insert contents)
          (save-buffer))
        (widen)
        (with-temp-file (ensure-directories-exist
                         (expand-file-name 
                          to-file (org-hyde-publish-dir project category)))
          (when yaml-front-matter
            (insert "{% extends \"_post.html\" %}\n")
            (insert "{%hyde \n")
            (mapc (lambda (pair) 
                    (insert (format "  %s: %s\n" (car pair) (cdr pair))))
                  yaml-front-matter)
            (if (and org-hyde-localize-dir lang)
                (mapc (lambda (line)
                        (insert (format "%s\n" line)))
                      (org-hyde-slurp-yaml (concat org-hyde-localize-dir
                                                     lang ".yml"))))
            (insert "%}\n\n"))
          (insert "{% block article %}\n{%article%}\n")
          (insert html)
          (insert "{%endarticle%}\n{% endblock %}\n\n"))))))

; Evtl. needed to keep compiler happy:
(declare-function org-publish-get-project-from-filename "org-publish"
                  (filename &optional up))

(defun org-hyde-export-current-entry ()
  (interactive)
  (save-excursion
    (let ((project (org-publish-get-project-from-filename buffer-file-name)))
      (org-back-to-heading t)
      (org-hyde-export-entry project))))

(defun org-hyde-export-blog (&optional filename)
  "Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. "
  (interactive)
  (save-excursion
    (setq org-hyde-new-buffers nil)
    (let ((project (org-publish-get-project-from-filename 
                    (if filename
                        filename
                      (buffer-file-name)))))
     (mapc 
      (lambda (jfile)
        (if (string= (file-name-extension jfile) "org")
            (with-current-buffer (org-get-hyde-file-buffer jfile)
              ;; It fails for non-visible entries, CONTENT visibility
              ;; mode ensures that all of them are visible.
              (org-content)
              (org-map-entries (lambda () (org-hyde-export-entry project))
                               "blog|BLOG"))))
      (org-publish-get-base-files project)))
    (org-release-buffers org-hyde-new-buffers)))

(provide 'org-hyde)

