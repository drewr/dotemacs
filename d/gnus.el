(setq user-full-name "Drew Raines"
      user-mail-address "drew@raines.me"
      message-user-fqdn "mid.raines.me"
      gnus-check-bogus-newsgroups nil
      gnus-check-new-newsgroups nil
      gnus-article-skip-boring nil
      gnus-delay-default-delay "2h"
      gnus-gcc-mark-as-read nil
      gnus-gcc-externalize-attachments "all"
      gnus-home-score-file 'gnus-hierarchial-home-score-file
      gnus-large-newsgroup 1000
      gnus-score-find-score-files-function 'gnus-score-find-hierarchical
      gnus-treat-display-smileys t
      gnus-treat-wash-html nil
      gnus-treat-fill-long-lines nil
      gnus-version-expose-system t
      gnus-cited-lines-visible '(3 . 6)
      gnus-article-skip-boring t
      gnus-treat-hide-citation t
      message-cite-function 'message-cite-original-without-signature
      message-dont-reply-to-names
      (concat "\\(aaraines\\|me\\)@\\(gmail\\|draines\\)")
      message-from-style 'angles
      message-forward-as-mime t
      message-make-forward-subject-function 'message-forward-subject-fwd
      message-subject-trailing-was-query nil
      message-wash-forwarded-subjects t
      gnus-auto-select-next 'almost-quietly
      gnus-use-full-window t
      gnus-inhibit-mime-unbuttonizing t
      gnus-use-cache t
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
        "^Content-Type:" "^Archived-At:")
      gnus-agent-go-online t
      gnus-agent-synchronize-flags t
      gnus-agent-long-article 1500
      gnus-agent-short-article 1000
      mm-discouraged-alternatives '("text/html" "text/richtext"))

(add-hook 'message-sent-hook 'gnus-score-followup-thread)

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (summary 0.5 point)
               (article 1.0))))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "~/bin/mail")

(setq gnus-select-method '(nnnil ""))

(setq gnus-secondary-select-methods
      '((nnimap "aaraines"
                (nnimap-address "imap.aaraines")
                (nnimap-authenticator login))
        (nnimap "drewres"
                (nnimap-address "imap.drewres")
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
        (nntp "news.gmane.org")
        (nntp "news.gwene.org")))

(defun aar/message-mode-setup ()
  (setq fill-column 69)
  (turn-on-auto-fill)
  (require 'footnote))

(add-hook 'message-mode-hook 'aar/message-mode-setup)
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
(aar/gnus-summary-line-format-ascii)

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

(defun aar/alter-summary-line-format ()
  "My alter summary line format."
  (if (string-match ".*mail\\.outbox.*" gnus-newsgroup-name)
      (setq gnus-summary-line-format "%U%R%z %d %4L:%I%(%[%-25,25f%]%) %s\n")
    (if (string-match "nnrss:.*" gnus-newsgroup-name)
        (setq gnus-summary-line-format
              "%U %6&user-date;:%I%(%[%-15,15f%]%) %s\n")
      (setq gnus-summary-line-format
            (concat
             "%0{%U%R%z%}"
             "%10{│%}"
             "%6&user-date;"
             "%10{│%}%*"
             "%9{%u&@;%}"
             "%(%-15,15f%) "
             "%10{│%}"
             "%4k"
             "%10{│%}"
             "%10{%B%}"
             "%s\n")))))
(add-hook 'gnus-summary-mode-hook 'aar/alter-summary-line-format)

(defun aar/sync-imap ()
  (interactive)
  (save-excursion
    (shell-command "offlineimap" "*offlineimap*" "*offlineimap*")))

(defun aar/get-new-news-and-disconnect (&optional arg)
  "Plug in, send, receive, plug out."
  (interactive "P")
  (gnus-group-save-newsrc)
  ;; (aar/sync-imap)
  (gnus-agent-toggle-plugged t)
  (gnus-group-send-queue)
  (gnus-group-get-new-news arg)
  (gnus-agent-fetch-session)
  (gnus-group-save-newsrc)
  (gnus-agent-toggle-plugged nil))

(defun aar/inject-queue ()
  "Plug in, send the queue, and plug out."
  (interactive)
  (gnus-agent-toggle-plugged t)
  (gnus-group-send-queue)
  (gnus-agent-toggle-plugged nil))

(add-hook 'gnus-group-mode-hook
          '(lambda ()
             (define-key gnus-group-mode-map "g"
               'aar/get-new-news-and-disconnect)
             (define-key gnus-group-mode-map "GG"
               'aar/inject-queue)))

(defun aar/message-from-only-name (fromline)
  "Return the name on a From: line, or the email
address if only it was present."
  (let* ((parsed (mail-header-parse-address fromline))
         (addr (car parsed))
         (name (cdr parsed)))
    (or name addr)))

(defun aar/message-insert-citation-line ()
  "Insert a simple Drew-ized citation line."
  (when message-reply-headers
    (insert
     (aar/message-from-only-name (mail-header-from message-reply-headers))
     " wrote:\n\n")))

(setq message-citation-line-function 'aar/message-insert-citation-line)

(defun aar/delay-send (prefix)
  "Dang, C-c C-c is easy to hit."
  (interactive "p")
  (if (= 1 prefix)
      (message "Prefix with C-u to send...")
    (message-send-and-exit)))
(define-key message-mode-map "\C-c\C-c" 'aar/delay-send)
(define-key message-mode-map "\C-c\C-s" 'aar/delay-send)

(setq gnus-posting-styles
      '((".*"
         ("X-PGP-Key" "http://draines.com/pubkey.asc.txt")
         (address "aaraines@gmail.com")
         (Face (gnus-face-from-file "~/.face-48.png")))
        ("drewres:.*"
         (address "drew.raines@elasticsearch.com"))))

;; format=flowed
;; http://article.gmane.org/gmane.emacs.gnus.user/14508

(defun harden-newlines ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (put-text-property (1- (point)) (point) 'hard t))))

(setq fill-flowed-display-column nil)

(add-hook 'message-setup-hook
          (lambda ()
            (when message-this-is-mail
              (turn-off-auto-fill)
              (setq
               truncate-lines nil
               word-wrap t
               use-hard-newlines t))))

(add-hook 'message-send-hook
          (lambda ()
            (when use-hard-newlines
              (harden-newlines))))

(add-hook 'gnus-article-mode-hook
          (lambda ()
            (setq
             truncate-lines nil
             word-wrap t)))

;; -----------
