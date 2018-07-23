+++
title = "Sharing code position from emacs"
author = ["Alexey Lebedeff"]
date = 2018-07-23T00:00:00+02:00
tags = ["emacs"]
draft = false
+++

There are times when you want to share an exact code location with
somebody. Maybe you've just shown them something and want to follow
up with an IM message. Or you want to add something to a bug
tracker issue.

The thing is that a code is opened in your favorite
editor, so you know everything about a commit, a file and a line
number. But to share a link you need to go to some kind of a web
git UI and search for exact same things there.

That's why I've made a small helper function for emacs that tries
to guess a correct web UI from a git remote, and points a browser
to an exact location where the cursor in emacs currently is:

{{< highlight emacs-lisp>}}
;; this depends on:
;; - s.el
;; - dash.el
;; - anaphora
;; - magit
;; - projectile

(defun binarin/open-web-link-to-source-code ()
  (interactive)
  (aif (block loop
           (dolist (url (binarin/get-git-remote-urls))
             (anaphoric-cond
              ((s-match "\\(gitlab\\.[^:]+\\):\\(.*\\)\\.git" url)
               (return (binarin/make-gitlab-link (nth 1 it) (nth 2 it)))))))
      (browse-url it)
    (message "Failed to generate a link from that file")))

(defun binarin/get-git-remote-urls ()
  (with-temp-buffer
    (magit-git-insert "remote" "-v")
    (-remove #'null
             (-map #'(lambda (a)
                       (nth 1 (s-split "[ \t]+" a)))
                   (s-lines (buffer-string))))))

(defun binarin/get-head-commit-sha ()
  (with-temp-buffer
    (magit-git-insert "rev-parse" "HEAD")
    (s-trim (buffer-string))))

(defun binarin/make-gitlab-link (base project)
  (let ((commit-sha (binarin/get-head-commit-sha))
        (filename-relative
         (car (projectile-make-relative-to-root (list (buffer-file-name)))))
        (line-number (string-to-number (format-mode-line "%l"))))
    (format "https://%s/%s/blob/%s/%s#L%d"
            base project commit-sha filename-relative line-number)))
{{< /highlight >}}
