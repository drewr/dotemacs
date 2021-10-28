(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (cl-flet ((addsymbols (symbol-list)
                          (when (listp symbol-list)
                            (dolist (symbol symbol-list)
                              (let ((name nil) (position nil))
                                (cond
                                 ((and (listp symbol) (imenu--subalist-p symbol))
                                  (addsymbols symbol))

                                 ((listp symbol)
                                  (setq name (car symbol))
                                  (setq position (cdr symbol)))

                                 ((stringp symbol)
                                  (setq name symbol)
                                  (setq position (get-text-property 1 'org-imenu-marker symbol))))

                                (unless (or (null position) (null name))
                                  (add-to-list 'symbol-names name)
                                  (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun clean-up-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun clean-up-golang-buffer ()
  (interactive)
  (gofmt))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun aar/transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (message (number-to-string arg))
  (let ((this-win (window-buffer))
        (next-win (window-buffer (next-window))))
    (set-window-buffer (selected-window) next-win)
    (set-window-buffer (next-window) this-win)
    (when (= arg 1)
      (select-window (next-window)))))

(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

(defvar growlnotify-command (executable-find "growlnotify")
  "The path to growlnotify")

(defun growl (title message)
  "Shows a message through the growl notification system using
 `growlnotify-command` as the program."
  (cl-flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
    (let* ((process (start-process "growlnotify" nil
                                   growlnotify-command
                                   (encfn title)
                                   "-a" "Emacs"
                                   "-n" "Emacs")))
      (process-send-string process (encfn message))
      (process-send-string process "\n")
      (process-send-eof process)))
  t)

(defun notify-send (title message)
  (start-process "notify" nil
                 "notify-send"
                 title
                 message))

(defun notify (title message)
  (case system-type
    ('darwin (growl title message))
    ('gnu/linux (notify-send title message))
    ('windows-nt 'wtf)))

(defun ledger-clean-up-transaction (start end)
  (interactive "r")
  (shell-command-on-region start end "ledger -E -f - print" "*ledger-clean*" t))

(defun ledger-bal-region (start end)
  (interactive "r")
  (shell-command-on-region start end "ledger -f - bal" "*ledger-bal*"))

(defun aar/irc-home-sonian (&optional arg)
  "Keyboard macro."
  (interactive "p")
  (kmacro-exec-ring-item
   (quote
    ("1b#search32b#84115oo2b#safeob#devopsoo" 0 "%d"))
   arg))

(defun aar/irc-home (&optional arg)
  "Keyboard macro."
  (interactive "p")
  (kmacro-exec-ring-item
   (quote
    ("1b#elasticsearch32b#84115oo2b#clojureob#nashdloo" 0 "%d"))
   arg))

(defun aar/irc-go-to-balls (&optional arg)
  (interactive "p")
  (aar/irc-home)
  (select-window
   (get-buffer-window "#84115")))

(defun aar/irc-go-to-notes (&optional arg)
  "Keyboard macro."
  (interactive "p")
  (kmacro-exec-ring-item (quote ("13bES.org" 0 "%d")) arg))

(defun aar/pretty-json (&optional arg) "Keyboard macro."
       (interactive "p")
       (kmacro-exec-ring-item
        (quote ("|python -mjson.tool" 0 "%d")) arg))

(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-buffer args)))

(defun browse-latest-url (&optional arg)
  (interactive "p")
  (save-excursion
    (search-backward-regexp "https?://" nil nil arg)
    (browse-url-at-point)))

(defun take-string (n str)
  (let* ()
    (if (<= 0 n (string-bytes str))
        (string-join
         (subseq
          (map 'list #'char-to-string (string-to-list str))
          0 n)
         "")
      str)))

(defun aar/org-insert-github-link (url)
  "Takes a link like:

    https://github.com/org/project/issues/123

and returns an org-formatted link:

    \[\[https://github.com/org/project/issues/123\]\[project\#123\]\]
"
  (interactive "sGitHub link: ")
  (let* ((parsed-url (url-generic-parse-url url))
         (file-parts (split-string (url-filename parsed-url) "/"))
         (project (elt file-parts 2))
         (link-type (elt file-parts 3))
         (ordinal (elt file-parts 4))
         (org-link (lambda (proj ord)
                     (let ((description (string-join (list proj ord) "#")))
                       (format "[[%s][%s]]" url description))))
         (short-sha (lambda (sha)
                      (take-string 7 sha))))
    (insert
     (pcase link-type
       ("issues" (apply org-link project (list ordinal)))
       ("pull" (apply org-link project (list ordinal)))
       ("commit" (apply org-link project (list (apply short-sha ordinal nil))))
       (_ url)))))

(defun aar/org-agenda-save ()
  (interactive)
  (org-save-all-org-buffers)
  (if (not (org-clocking-p))
    (when (> (call-process "saveorg" nil "*saveorg*") 0)
      (error "saveorg failed!!"))
    (message "no saveorg while clock running")))

(defun aar/convert-org-buffer-to-gmail ()
  (interactive)
  (let ((md-buffer (concat (buffer-name) ".md")))
    (shell-command-on-region
     (point-min) (point-max)
     (string-join ["pandoc"
                   "-f org"
                   "-t gfm"
                   "--wrap=none"
                   ]
                  " ")
     md-buffer)
    (set-buffer md-buffer)
    (normal-mode)
    (kill-ring-save (point-min) (point-max))))

(defun aar/org-journal-today-start ()
  (interactive)
  (insert
   (concat "** " (upcase (format-time-string "%Y-%m-%d %a")) "\n*** ")))

;; https://stackoverflow.com/a/2478549/3227
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))
