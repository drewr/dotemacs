;; -*- emacs-lisp -*-

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
       (concat "\\(aaraines\\|aa\\|drew\\|\\.raines\\|me\\|draines\\)@"
               "\\(d?raines\\|pobox\\|[Vv]and\\|gmail\\|prov\\|pwm"
               "\\|vt\\.\\|cabedge\\)"
               "\\|\\(draines@notifymd\\)")
       message-from-style 'angles
       message-forward-as-mime nil
       message-make-forward-subject-function 'message-forward-subject-fwd
       message-subject-trailing-was-query nil
       message-wash-forwarded-subjects t
       gnus-auto-select-next 'almost-quietly
       gnus-use-full-window t
       gnus-inhibit-mime-unbuttonizing t
       nntp-marks-is-evil nil
       nnml-marks-is-evil nil
       gnus-use-cache t
       gnus-user-agent '(emacs gnus config)
       message-sendmail-envelope-from 'header)

;;(setq w3m-display-inline-image t)
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-text-html-renderer 'mm-inline-text-html-render-with-w3m)
(setq mm-inline-text-html-renderer 'w3m)

(if (eq system-type 'windows-nt)
    (setq imap-ssl-program
          '("c:\\cygwin\\bin\\openssl s_client -quiet -ssl3 -connect %s:%p")))

;;(setq message-generate-headers-first t)

;; gnus-delay
(gnus-delay-initialize)

;; This helps with archiving spam at the end of the month
(add-to-list 'gnus-extra-headers 'Received)
(add-to-list 'gnus-extra-headers 'X-Bogosity)
(add-to-list 'gnus-extra-headers 'X-Spambayes-Classification)
(setq nnmail-extra-headers gnus-extra-headers)

;; Better to use wget so nnrss works all the time.
                                        ;(setq mm-url-use-external t)

(setq gnus-ignored-from-addresses
      (concat "\\(\\(aa\\|drew\\|me\\)[^@]*@d?raines\\)"
              "\\|\\(\\([Aa]?[Nn]?[Dd]rew\\|a\\)\\.[Rr]aines"
              "@[Vv]ander.*\\)\\|\\(aaraines@pobox\\.com\\)"
              "\\|\\(aaraines@gmail\\.com\\)"
              "\\|\\(draines@notifymd\\.com\\)"))
(setq message-forward-ignored-headers
      (concat "^Original-Received:\\|^Content-Transfer-Encoding:"
              "\\|^X-Gnus\\|^Return-Path:\\|^Sender:\\|^[GB]cc:"
              "\\|^X-\\|^Xref:\\|^Received:\\|^Precedence:"
              "\\|^Delivered-To:\\|^User-Agent:\\|^References:"
              "\\|^Lines:\\|^Path:\\|^NNTP-Posting-Host:"
              "\\|^NNTP-Posting-Date:\\|^Cancel-Lock:\\|^Mail-Copies-To:"
              "\\|^Return-Receipt-To:\\|^Disposition-Notification-To:"
              "\\|^Old-\\|^content-class:\\|^Thread-"))
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '((nnml ""
              (nnir-search-engine swish-e)
              (nnir-swish-e-remove-prefix "/home/aar/Mail/")
              (nnir-swish-e-index-file "/home/aar/Mail/index.swish-e"))
        (nnfolder "mboxes"
                  (nnfolder-directory   "~/Mail/mboxes")
                  (nnfolder-active-file "~/Mail/mboxes/active")
                  (nnfolder-get-new-mail nil)
                  (nnfolder-inhibit-expiry nil))
        ;;         (nnimap "gmail"
        ;;                 (nnimap-address "imap.gmail.com")
        ;;                 (nnimap-stream ssl))
        (nnimap "notifymd"
                (nnimap-address "imap.notifymd"))
        (nntp "news.gmane.org"
              (nntp-marks-is-evil t))
        (nntp "news.individual.net"
              (nntp-marks-is-evil t))))


(setq nnimap-authinfo-file (expand-file-name "~/.authinfo"))

(setq starttls-use-gnutls nil
      smtpmail-smtp-service 587
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-debug-info nil
      smtpmail-smtp-server "smtp.raines.ws"
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)
                                      ("mail.hcoop.net" 26 nil nil)))
(when (equal system-type 'darwin)
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/sbin/sendmail"))


(setq gnus-message-archive-method
      '(nnfolder "archive"
                 (nnfolder-directory   "~/Mail/archive")
                 (nnfolder-active-file "~/Mail/archive/active")
                 (nnfolder-get-new-mail nil)
                 (nnfolder-inhibit-expiry t)
                 (nnir-search-engine swish-e)))

(defun aar-gnus-outgoing-message-group ()
  (let ((archive "nnfolder+archive:mail.outbox.all")
        (outbox "nnml:mail.outbox"))
    (cond ((and (message-news-p)
                (not (string-match "\\.test" gnus-newsgroup-name)))
           outbox)
          ((and gnus-newsgroup-name
                (stringp gnus-newsgroup-name)
                (string-match "^\\(nnml:mail\\)\\|nnimap" gnus-newsgroup-name)
                (not (string-match "\\.list\\." gnus-newsgroup-name))
                (not (string-match "[a-z.]+\\.its\\." gnus-newsgroup-name)))
           (list gnus-newsgroup-name archive)))))
(setq gnus-outgoing-message-group 'aar-gnus-outgoing-message-group)

(defun passwd-from-authinfo (machine port)
  (let* ((x (netrc-parse "~/.authinfo"))
         (y (netrc-machine x machine)))
    (netrc-get y "password")))

(setq mail-sources
      '(;(pop :server "mail.draines.com"
                                        ;                  :user "me@draines.com"
                                        ;                  :password (passwd-from-authinfo "mail.draines.com"))
                                        ;(imap :server "imap.gmail.com"
                                        ;      :port 993
                                        ;      :user "aaraines"
                                        ;      :password (passwd-from-authinfo "imap.gmail.com" 993)
                                        ;      :stream ssl
                                        ;      :fetchflag "\\Seen")
        ))
(setq nnmail-crosspost nil)
(setq nnmail-split-methods
      '(("mail.rss.mjd" "From:.*The Universe of Discourse")
        ("mail.rss.dougwils" "From:.*BLOG and MABLOG")
        ("mail.rss" "User-Agent: rss2email")
        ("mail.list.misc.nlug" "^Sender:.*nlug-")
        ("mail.list.misc.svlug" "^Sender:.*svlug-")
        ("mail.list.misc.web-o-trust" "^Mailing-List: contact discuss-help@web-o-trust")
        ("mail.list.misc.hop" "^Mailing-List: contact hop-discuss")
        ("mail.list.misc.macosx-emacs" "^Sender: <macosx-emacs")
        ("mail.list.misc.nashdl" "^List-Id: <nashdl.googlegroups.com>")
        ("mail.list.misc.frisbee" "^List-Id: <nashville-frisbee")
        ("mail.list.hcoop.announce" "^List-Id:.*hcoop-announce")
        ("mail.list.hcoop.discuss" "^List-Id:.*hcoop-discuss")
        ("mail.list.hcoop.misc" "^List-Id:.*hcoop-misc")
        ("mail.list.hcoop.support" "^From:.*Hcoop \\(Support\\|Portal\\)")
        ("mail.list.sw.prjware" "^Mailing-List: contact prjware-")
        ("mail.list.sw.ledger" "^Sender:.*ledger-discuss")
        ("mail.list.sw.rails.cookbook" "^Sender: rails-cookbook")
        ("mail.list.sw.hpricot" "^List-Id: <hpricot")
        ("mail.grace" "^From: .*@gccnashville\\.org")
        ("mail.grace" "^Mailing-List: contact gcc-[^, ]+\\@list\\.raines\\.ws")
        ("mail.grace.cww" "^\\(To\\|Cc\\):.*hopecommunications@groups\\.mac\\.com")
        ("mail.loveinc" "^Subject:.*\\[[^]]+Love INC[^]]+\\]")
        ("mail.cabedge.exceptions" "^To:.*app-exceptions@cabedge\\.com")
        ("mail.cabedge.commits" "^To:.*svn-commits@cabedge\\.com")
        ("mail.cabedge.network" "^To:.*network@cabedge\\.com")
        ("mail.cabedge.deploy" "^To:.*deploy@okra\\.cabedge\\.net")
        ("mail.cabedge" "^Delivered-To:.*drew@cabedge\\.com")
        ("mail.providence" "^\\(To\\|Cc\\):.*draines@\\(providence\\|pwm\\)")
        ("mail.belmont" "^\\(To\\|Cc\\):.*rainesa@pop\\.belmont\\.edu")
        ("mail.personal" "^\\(To\\|Cc\\):.*drew@raines\\.ws\\|aaraines@\\(pobox\\|gmail\\)\\.com")
        ("mail.misc" ".*")))

(defun aar-message-mode-setup ()
  (setq fill-column 69)
  (turn-on-auto-fill)
  (require 'footnote)
  (flyspell-mode))

(add-hook 'message-mode-hook 'aar-message-mode-setup)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)

(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-discouraged-alternatives "text/richtext")))

(setq gnus-visible-headers
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


;;
;; automatic mail scan without manual effort.
;;
;; level-specified group scanner.
(defun gnus-demon-scan-mail-or-news-and-update (level)
  "Scan for new mail, updating the *Group* buffer."
  (let ((win (current-window-configuration)))
    (unwind-protect
        (save-window-excursion
          (save-excursion
            (when (gnus-alive-p)
              (save-excursion
                (set-buffer gnus-group-buffer)
                (gnus-group-get-new-news level)))))
      (set-window-configuration win))))
;;
;; level 2: only mail groups are scanned.
(defun gnus-demon-scan-mail-and-update ()
  "Scan for new mail, updating the *Group* buffer."
  (gnus-demon-scan-mail-or-news-and-update 2))
                                        ;(gnus-demon-add-handler 'gnus-demon-scan-mail-and-update 5 t)
;;
;; level 3: mail and local news groups are scanned.
(defun gnus-demon-scan-news-and-update ()
  "Scan for new mail, updating the *Group* buffer."
  (gnus-demon-scan-mail-or-news-and-update 3))
                                        ;(gnus-demon-add-handler 'gnus-demon-scan-news-and-update 30 t)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
                                        ;(setq gnus-group-line-format "%M%S%6R%5U%5y: %(%-35,35G%)(%2T%2i) %ud %2L %-2I\n")
(setq gnus-group-line-format "%7L%12ud %M%S%5U%6y: %(%G%):%-6R\n")
(defun gnus-user-format-function-d (headers)
  (let ((time (gnus-group-timestamp gnus-tmp-group)))
    (format-time-string "%H%M %b %d" time)))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        ((gnus-seconds-year) . "%b %d") (t . "%y%m%d")))

(defun aar-alter-summary-line-format ()
  "My alter summary line format."
  (if (string-match ".*mail\\.outbox.*" gnus-newsgroup-name)
      (setq gnus-summary-line-format "%U%R%z %d %4L:%I%(%[%-25,25f%]%) %s\n")
    (if (string-match "nnrss:.*" gnus-newsgroup-name)
        (setq gnus-summary-line-format "%U %6&user-date;:%I%(%[%-15,15f%]%) %s\n")
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
(add-hook 'gnus-summary-mode-hook 'aar-alter-summary-line-format)

                                        ; Tired of sending messages before I'm ready...
(defun aar/delay-send (prefix)
  "Dang, C-c C-c is easy to hit."
  (interactive "p")
  (if (= 1 prefix)
      (message "Prefix with C-u to send...")
    (message-send-and-exit)))
(define-key message-mode-map "\C-c\C-c" 'aar/delay-send)
(define-key message-mode-map "\C-c\C-s" 'aar/delay-send)

(setq face-directory "~/.faces/")
(defun message-insert-face ()
  "Insert a random Face header."
  (interactive)
  (when (file-exists-p face-directory)
    (let* ((files (directory-files face-directory t "\\.face$"))
           (file (nth (random (length files)) files)))
      (when file
        (save-excursion
          (goto-char (point-min))
          (search-forward mail-header-separator)
          (beginning-of-line nil)
          (insert "Face: ")
          (insert-file (expand-file-name file)))))))
                                        ;(add-hook 'message-send-hook 'message-insert-face)

(defun aar-start-tunnels ()
  "Make sure SSH Tunnel Manager is running."
  (interactive)
  (shell-command "osascript ~/bin/start-tunnels.applescript"))
                                        ;(add-hook 'message-send-hook 'aar-start-tunnels)

;;; Hm, let's try the regex in the manual first.
                                        ;(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;;; Insert the Face: *after* sending
(setq message-required-news-headers
      '(From Newsgroups Subject Date Message-ID
             (optional . Organization)
             (optional . User-Agent))
      message-required-mail-headers
      '(From Subject Date
             (optional . In-Reply-To)
             Message-ID
             (optional . User-Agent)))
(setq message-required-news-headers
      (nconc message-required-news-headers
             (list '(Face . (lambda ()
                              (gnus-face-from-file "~/.face.jpg"))))))
;; (setq message-required-mail-headers
;;       (nconc message-required-mail-headers
;;           (list '(Face . (lambda ()
;;                            (gnus-face-from-file "~/.face.jpg"))))))
(setq message-field-fillers
      `((Face ignore)
        (To message-fill-field-address)
        (Cc message-fill-field-address)
        (From message-fill-field-address)))

(setq gnus-posting-styles
      '((".*"
         ("X-PGP-Key" "http://draines.com/pubkey.asc.txt")
         (signature nil))
        ("nnml.*list\\."
         (address "aaraines@gmail.com"))
        ("nashdl"
         (address "aaraines@gmail.com"))
        ("frisbee"
         (address "aaraines@gmail.com"))
        ("mephisto"
         (address "aaraines@gmail.com"))
        ("providence"
         (address "draines@pwministries.net")
         (bcc "draines@pwministries.net"))
        ("\\.*\\(^vu\\.\\|university\\|vanderbilt\\|vampire\\|vuexchange\\)"
         (signature-file "~/.sig-vandy")
         ("X-PGP-Key" "http://people.vanderbilt.edu/~drew.raines/pubkey.asc")
         (address "drew.raines@vanderbilt.edu"))
        ("INBOX\\.personal.*"
         (signature nil))
        ("gmane\\.discuss\\.subscribe"
         (signature "Drew Raines <http://gmane.org>"))
        ("INBOX\\.class\\.vt.*"
         (address "aaraines@VT.EDU")
         (signature nil))
        (message-this-is-news
         (address "aaraines@gmail.com")
         (reply-to nil)
         ("Mail-Copies-To" "never"))
        ("alt\\.test"
         (name "Jim JIMMERSON")
         (address "jimmy@jimmerson.dom")
         ("Organization" "Jimmersonian Institute")
         (signature "http://jim.jimmerson.dom")
         ("X-PGP-Key" "I hate encryption"))
        ("\\.hcoop"
         (name "Drew Raines")
         (address "drewr@hcoop.net")
         (signature nil))
        ("nnml.*\\.belmont"
         (name "Andrew Raines")
         (address "rainesa@pop.belmont.edu")
         (signature nil))
        ("nnml.*\\.loveinc"
         (name "Andrew Raines")
         (reply-to "aaraines@nashvilleloveinc.org")
         (signature "http://nashvilleloveinc.org"))
        ("nnml.*\\.cabedge"
         (name "Drew Raines")
         (address "drew@cabedge.com")
         (reply-to nil)
         (signature "drew@cabedge.com * http://cabedge.com * 615-550-2407"))
        ("nnimap\\+notifymd"
         (name "Drew Raines")
         (address "draines@notifymd.com")
         (reply-to nil)
                                        ; (signature "draines@notifymd.com * http://notifymd.com * 615-778-6730")
         (signature nil))))

(defun aar-message-from-only-name (fromline)
  "Return the name on a From: line, or the email
address if only it was present."
  (let* ((parsed (mail-header-parse-address fromline))
         (addr (car parsed))
         (name (cdr parsed)))
    (or name addr)))
(defun aar-message-insert-citation-line ()
  "Insert a simple Drew-ized citation line."
  (when message-reply-headers
    (insert
     (aar-message-from-only-name (mail-header-from message-reply-headers))
     " wrote:\n\n")))
(setq message-citation-line-function 'aar-message-insert-citation-line)

(setq gnus-agent-go-online t
      gnus-agent-synchronize-flags t
      gnus-agent-long-article 1500
      gnus-agent-short-article 1000)

(setq gc-cons-threshold 3500000)
(gnus-compile)

(defun aar-get-new-news-and-disconnect (&optional arg)
  "Plug in, send, receive, plug out."
  (interactive "P")
  (gnus-save-newsrc-file)
  (gnus-agent-toggle-plugged t)
  (gnus-group-send-queue)
  (gnus-group-get-new-news arg)
  (gnus-agent-fetch-session)
  (gnus-save-newsrc-file)
  (gnus-agent-toggle-plugged nil))

(defun aar-inject-queue ()
  "Plug in, send the queue, and plug out."
  (interactive)
  (gnus-agent-toggle-plugged t)
  (gnus-group-send-queue)
  (gnus-agent-toggle-plugged nil))

(add-hook 'gnus-group-mode-hook
          '(lambda ()
             (define-key gnus-group-mode-map "g"
               'aar-get-new-news-and-disconnect)
             (define-key gnus-group-mode-map "GG"
               'aar-inject-queue)))

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

;; nnrss doesn't like Atom

(require 'mm-url)
(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
                           nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max)
                         "xsltproc"
                         t t nil
                         (expand-file-name "~/elisp/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))

(ad-activate 'mm-url-insert)
