(setq gnus-select-method '(nnnil ""))

(setq gnus-secondary-select-methods
      '((nnimap "aaraines"
                (nnimap-address "imap.aaraines")
                (nnimap-authenticator login))
        (nnimap "testdroid1000"
                (nnimap-address "imap.testdroid1000")
                (nnimap-authenticator login))
        (nnml ""
              (nnir-search-engine swish-e)
              (nnir-swish-e-remove-prefix "/home/aar/Mail/")
              (nnir-swish-e-index-file "/home/aar/Mail/index.swish-e"))
        (nnfolder "mboxes"
                  (nnfolder-directory   "~/Mail/mboxes")
                  (nnfolder-active-file "~/Mail/mboxes/active")
                  (nnfolder-get-new-mail nil)
                  (nnfolder-inhibit-expiry nil))
        (nntp "news.gmane.org"
              (nntp-marks-is-evil t))))

(defun aar-message-mode-setup ()
  (setq fill-column 69)
  (turn-on-auto-fill)
  (require 'footnote)
  (flyspell-mode))

(add-hook 'message-mode-hook 'aar-message-mode-setup)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

(defun gnus-user-format-function-d (headers)
  (let ((time (gnus-group-timestamp gnus-tmp-group)))
    (format-time-string "%H%M %b %d" time)))

(setq gnus-group-line-format "%7L%12ud %M%S%5U%6y: %(%G%):%-6R\n")

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        ((gnus-seconds-year) . "%b %d") (t . "%y%m%d")))

(setq gnus-face-9 'font-lock-warning-face)
(setq gnus-face-10 'shadow)
(defun aar/gnus-summary-line-format-ascii nil
  (interactive)
  (setq
   gnus-sum-thread-tree-single-indent   "o "
   gnus-sum-thread-tree-false-root      "x "
   gnus-sum-thread-tree-root            "* "
   gnus-sum-thread-tree-vertical        "| "
   gnus-sum-thread-tree-leaf-with-other "|-> "
   gnus-sum-thread-tree-single-leaf     "+-> " ;; "\\" is _one_ char
   gnus-sum-thread-tree-indent          "  ")
  (gnus-message 5 "Using ascii tree layout."))

(defun aar/gnus-summary-line-format-unicode nil
  (interactive)
  (setq
   gnus-sum-thread-tree-single-indent   "◎ "
   gnus-sum-thread-tree-false-root      "x "
   gnus-sum-thread-tree-root            "┌ "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─>"
   gnus-sum-thread-tree-single-leaf     "└─>"
   gnus-sum-thread-tree-indent          "  ")
  (gnus-message 5 "Using ascii tree layout with unicode chars."))

(aar/gnus-summary-line-format-unicode)

(defun gnus-user-format-function-@ (header)
  "Display @ for message with attachment in summary line.

You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."
  (let ((case-fold-search t)
        (ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
                   "text/plain"))
        indicator)
    (cond
     ((string-match "^multipart" ctype) "@")
     (t " "))))

