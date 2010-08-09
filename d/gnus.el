(setq  user-full-name "Drew Raines"
       user-mail-address "aaraines@gmail.com"
       message-user-fqdn "id.draines.com"
       gnus-check-bogus-newsgroups nil
       gnus-check-new-newsgroups nil
       gnus-article-skip-boring nil
       gnus-delay-default-delay "2h"
       gnus-gcc-mark-as-read nil
       gnus-gcc-externalize-attachments "all"
       gnus-home-score-file 'gnus-hierarchial-home-score-file
       gnus-large-newsgroup 250
       gnus-score-find-score-files-function 'gnus-score-find-hierarchical
       gnus-treat-display-smileys t
       gnus-treat-wash-html nil
       gnus-treat-fill-long-lines nil   ;90
       gnus-version-expose-system t
       gnus-cited-lines-visible '(3 . 6)
       gnus-article-skip-boring nil
       gnus-treat-hide-citation t
       message-cite-function 'message-cite-original-without-signature
       message-dont-reply-to-names
       (concat "\\(aaraines\\|me\\)@\\(gmail\\|draines\\)")
       message-from-style 'angles
       message-forward-as-mime nil
       message-make-forward-subject-function 'message-forward-subject-fwd
       message-subject-trailing-was-query nil
       message-wash-forwarded-subjects t
       gnus-auto-select-next 'almost-quietly
       gnus-use-full-window nil ;; trying this while I keep org in the
                                ;; other half of the screen
       gnus-inhibit-mime-unbuttonizing t
       nntp-marks-is-evil nil
       nnml-marks-is-evil nil
       gnus-use-cache t
       gnus-user-agent '(emacs gnus config)
       message-sendmail-envelope-from 'header
       gnus-visible-headers
       '("^From:" "^Newsgroups:" "^Subject:"
         "^Date:" "^Followup-To:" "^Reply-To:"
         "^Organization:" "^Summary:" "^Keywords:"
         "^To:" "^Cc:" "^Posted-To:" "^Mail-Copies-To:"
         "^Apparently-To:" "^Gnus-Warning:" "^Resent-From:"
         "^X-Sent:" "^User-Agent:" "^X-Mailer:"
         "^X-Newsreader:" "^X-Accept-Language:"
         "^X-Delivery-Agent:"
         "^X-Spambayes-Classification:" "^X-Spambayes-Trained:"
         "^X-Bogosity:" "^X-Gnus-Delayed:"
         "^Content-Type:"))

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

