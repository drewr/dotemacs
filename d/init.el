;; startup

(defmacro load-custom (f)
  "Load a local configuration file in ~/.emacs.d/."
  `(load (concat ,(expand-file-name "~/.emacs.d/") ,f)))

(defmacro add-lisp-dir (d)
  "Add dir to load-path."
  `(add-to-list 'load-path (concat ,(expand-file-name "~/.emacs.d/lisp/") ,d)))

(require 'cl) ; need this for (case)
(load-custom
 (case system-type
   ('darwin "darwin")
   ('gnu/linux "linux")
   ('windows-nt "win32")))
(load-custom "funs")
(load-custom "sonian")

(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(let ((d (expand-file-name "~/.nix-profile/share/emacs/site-lisp")))
  (when (file-exists-p d)
    (add-to-list 'load-path d)))

(add-to-list 'default-frame-alist '(width . 103))
(add-to-list 'default-frame-alist '(height . 35))

(when (and (>= (string-to-number emacs-version) 23)
           (fboundp 'server-running-p))
  (server-start))
(require 'ffap)
(require 'saveplace)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(recentf-mode 1)
(ido-mode t)
(setq ido-mode-everywhere t)
(column-number-mode 1)
(setq display-time t
      display-time-24hr-format t)
(display-time)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-n") 'clean-up-buffer)
(global-set-key (kbd "C-c C-j") 'aar/pretty-json)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x F") 'find-function)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x O")
                (lambda ()
                  (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o")
                (lambda ()
                  (interactive) (other-window 2))) ;; forward two
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x M-m") 'shell)
(global-set-key (kbd "C-x 4 t") 'aar/transpose-windows)
(global-set-key (kbd "C-x 4 o") 'aar/irc-home)
(global-set-key (kbd "C-x 4 8") 'aar/irc-go-to-balls)
(global-set-key (kbd "C-x 4 l") 'aar/irc-go-to-notes)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(when (equalp "DUMB" (getenv "TERM"))
  (setenv "PAGER" "cat"))

(eval-after-load 'diff-mode
  '(progn (set-face-foreground 'diff-added "green4")
          (set-face-foreground 'diff-removed "red3")))

;; bbdb

(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-user-mail-names
      (regexp-opt '("aaraines@gmail.com" "drew@raines.me")))
(setq bbdb-complete-name-allow-cycling t)
(setq bbdb-use-pop-up nil)

;; pre-gnus
;;(add-to-list 'load-path (expand-file-name "~/src/gnus/lisp"))
;;(add-to-list 'Info-default-directory-list (expand-file-name "~/src/gnus/texi/"))
(setq gnus-home-directory "~/.gnus.d/")
(setq gnus-directory (concat gnus-home-directory "News/"))
(setq message-directory (concat gnus-home-directory "Mail/"))
(require 'gnus)
(require 'info)

;; elmer

(require 'elmer)
(setq elmer-paste-bin "zsh <(curl -s p.draines.com/sh)")
(global-set-key (kbd "C-c C-e") 'elmer)

;; scpaste

(autoload 'scpaste "scpaste" "Paste the current buffer." t nil)
(setq scpaste-http-destination "http://draines.com/tmp"
      scpaste-scp-destination "draines:/www/htdocs/draines/tmp")

;; magit

(global-set-key "\C-xg" 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; lisp

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defun aar/lispy-parens ()
  "Set up parens for lispish modes."
  (require 'paredit)
  (paredit-mode 1)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)
  (show-paren-mode 1))

(defun aar/massage-nrepl-bindings ()
  (define-key paredit-mode-map (kbd "C-c C-n") 'clean-up-buffer))

(add-hook 'emacs-lisp-mode-hook 'aar/lispy-parens)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'clojure-mode-hook 'aar/lispy-parens)
(add-hook 'clojure-mode-hook 'aar/massage-nrepl-bindings)
(add-hook 'clojure-mode-hook 'whitespace-mode)

;; clojure

(eval-after-load 'cider
  '(progn
     (setq cider-repl-history-size 10000
           cider-repl-history-file "~/.cider.history.el"
           cider-test-infer-test-ns '(lambda (ns) ns))
     (define-key cider-mode-map (kbd "C-c C-n") nil)
     (define-key cider-mode-map (kbd "C-c n") 'cider-eval-ns-form)))

;; haskell

(require 'flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(defun aar/haskell-mode-hook ()
  (hindent-mode)
  (interactive-haskell-mode)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation))

(add-hook 'haskell-mode-hook 'aar/haskell-mode-hook)

(setq
 haskell-process-auto-import-loaded-modules t
 haskell-process-suggest-language-pragmas nil
 haskell-process-log t
 haskell-process-type 'ghci
 haskell-process-path-ghci "stack"
 haskell-process-args-ghci '("ghci")
 haskell-process-suggest-remove-import-lines nil)

(require 'hpaste)

;; ocaml

(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

;; rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c TAB") #'rust-format-buffer)))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'flycheck-mode)

;; nix
(require 'nix-mode)  ;; from ~/.nix-profile/.../site-lisp above
;; (nix-env -i emacs)

;; go
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-n") #'clean-up-golang-buffer)
            (local-set-key (kbd "M-.") #'godef-jump)
            (setq tab-width 2)
            (setq gofmt-command "goimports")
            ;;(add-hook 'before-save-hook #'gofmt-before-save)
            ))

;; python
(require 'python)

;; javascript
(setq js-indent-level 2)

;; org

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key [(control meta ?r)] 'org-capture)
(setq org-directory "~/.org"
      org-agenda-files (quote ("~/.org/PERSONAL.org"
                               "~/.org/clients"))
      org-log-done t
      org-agenda-show-all-dates t
      org-agenda-ndays 7
      org-agenda-start-on-weekday nil
      org-agenda-start-with-log-mode t
      org-export-publishing-directory "~/.org/export"
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-reverse-note-order t
      org-clock-modeline-total 'current
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull "~/.org/mobile-inbox.org")
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DEFERRED"
                  "|" "DONE" "DELEGATED" "CANCELED")))

(setq org-export-html-postamble "<p class=\"postamble\">%d %a</p>")

(setq org-default-notes-file "~/.org/notes.org")

(setq org-capture-templates
      '(("p" "Personal task" entry
         (file+headline "PERSONAL.org" "Tasks")
         "* TODO %?\n  %u" :prepend t)
        ("t" "Trinity" entry
         (file+headline "clients/TRINITY.org" "Tasks")
         "* TODO %?\n  %u" :prepend t)
        ("e" "Elasticsearch" entry
         (file+headline "clients/ES.org" "Tasks")
         "* TODO %?\n  %u" :prepend t)
        ("n" "Note" entry
         (file+headline "notes.org" 'bottom)
         "* %u %?" :prepend t)))

(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'auto-fill-mode)

(require 'org-journal)
(eval-after-load 'org-journal
  '(progn
     (setq org-journal-dir "~/src/org/journal/")))

;; ledger

(require 'ledger)
(eval-after-load 'ledger
  '(progn
     (define-key ledger-mode-map
       [(control ?c) (control ?n)] 'ledger-clean-up-transaction)
     (define-key ledger-mode-map
       [(control ?c) (control ?b)] 'ledger-bal-region)))


;; erc

(require 'erc)
(require 'erc-highlight-nicknames)
(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'highlight-nicknames)

(setq erc-server "irc.us.freenode.net"
      erc-port 6667
      erc-nick "drewr"
      erc-user-full-name "Drew Raines"
      erc-email-userid "drew"       ; for when ident is not activated
      erc-fill-column 86
      erc-fill-prefix "   "
      erc-auto-query 'window-noselect
      erc-prompt-for-nickserv-password nil
      erc-nickserv-passwords nil
      erc-auto-discard-away t
      erc-autoaway-idle-seconds 600
      erc-log-insert-log-on-open nil
      erc-log-channels-directory "~/tmp/irc/log"
      erc-kill-queries-on-quit nil
      erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT" "TOPIC"
                                "NAMES" "324" "329" "332" "333" "353" "477"
                                "NOTICE")
      erc-encoding-coding-alist '(("#emacs" . utf-8))
      erc-current-nick-highlight-type 'nick
      erc-max-buffer-size 100000
      erc-join-buffer 'bury
      erc-server-send-ping-interval 45
      erc-server-send-ping-timeout 86400
      erc-server-reconnect-timeout 60
      erc-server-reconnect-attempts 10
      erc-prompt (lambda () (concat (erc-default-target) ">"))
      erc-favorite-channel
      "\\(84115\\|elast\\|pallet\\|lein\\|nash\\)")

(define-key erc-mode-map (kbd "<f2>") 'browse-latest-url)

(setq-default erc-ignore-list '("^xah_?" "^jordanb_?"))

(load "~/.erc-auth.el")

(defun aar/you-rang? (mat nick message)
  (let ((buf (buffer-name (current-buffer)))
        (msg (concat "<" (car (erc-parse-user nick)) "> " message)))
    (when (eq mat 'current-nick)
      (unless (posix-string-match "^\\** *Users on #" message)
        (notify buf msg)
        nil))))

(defun aar/erc-me (proc parsed)
  (let* ((chan (car (erc-response.command-args parsed)))
         (nickraw (erc-response.sender parsed))
         (nick (car (erc-parse-user nickraw)))
         (msg (concat "<" nick "> " (erc-response.contents parsed))))
    (when (and (or (string-match erc-favorite-channel chan)
                   (string= chan (erc-current-nick)))
               (not (erc-ignored-user-p nickraw)))
      (notify chan msg)
      nil)))

;;(add-hook 'erc-text-matched-hook 'aar/you-rang?)
;;(add-hook 'erc-server-PRIVMSG-functions 'aar/erc-me)

;; puppet
(eval-after-load 'puppet-mode
  '(define-key puppet-mode-map (kbd "$") 'self-insert-command))

;; edit-server
(require 'edit-server)

;; demo slides
(require 'demo)

;; D
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;; PureScript
(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(calc-float-format (quote (fix 2)) t)
 '(canlock-password "9a765f623398f9e6a768c158f2fca8588d732be7")
 '(cua-mode nil nil (cua-base))
 '(current-language-environment "UTF-8")
 '(cursor-in-non-selected-windows nil)
 '(dired-recursive-deletes (quote top))
 '(enable-recursive-minibuffers t)
 '(safe-local-variable-values
   (quote
    ((prompt-to-byte-compile))))
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil nil (menu-bar))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(scroll-bar-mode nil)
 '(sh-indentation 2)
 '(show-paren-mode nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode nil)
 '(truncate-lines nil)
 '(line-move-visual nil)
 '(require-final-newline t)
 '(whitespace-style '(face trailing lines space-before-tab
                           indentation space-after-tab))
 '(whitespace-line-column 80)
 '(hfy-default-face-def
   (quote ((t :foreground "black" :background "white" :family "monospace"))))
 '(hfy-display-class
   (quote ((type . x-toolkit) (class . color) (background . light)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(circe-my-message-face ((t (:foreground "DarkRed")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
