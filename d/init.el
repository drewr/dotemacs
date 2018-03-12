;; startup

(defmacro load-custom (f)
  "Load a local configuration file in ~/.emacs.d/."
  `(load (concat ,(expand-file-name "~/.emacs.d/") ,f)))

(defmacro add-lisp-dir (d)
  "Add dir to load-path."
  `(add-to-list 'load-path (concat ,(expand-file-name "~/.emacs.d/lisp/") ,d)))

;; For stack-installed utils
(setenv "PATH" (concat (expand-file-name "~/.local/bin") ":" (getenv "PATH")))

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

(when (and (>= (string-to-number emacs-version) 23)
           (fboundp 'server-running-p))
  (server-start))
(require 'ffap)
(require 'saveplace)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(require 'use-package)

(use-package adoc-mode    :ensure t :pin "melpa")
(use-package arduino-mode :ensure t :pin "melpa")
(use-package bbdb         :ensure t :pin "melpa")
(use-package cargo        :ensure t :pin "melpa")

(use-package cider
  :ensure t
  :pin "melpa-stable"
  :bind
  (:map cider-mode-map
        ("C-c C-n" . nil)
        ("C-c n" . cider-eval-ns-form))
  :config
  (setq cider-repl-history-size 10000
        cider-repl-history-file "~/.cider.history.el"
        cider-test-infer-test-ns '(lambda (ns) ns)
        cider-repl-display-help-banner nil
        cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"))

(use-package clojure-mode
  :ensure t
  :pin "melpa"
  :config
  (add-hook 'clojure-mode-hook 'aar/lispy-parens)
  (add-hook 'clojure-mode-hook 'aar/massage-nrepl-bindings)
  (add-hook 'clojure-mode-hook 'whitespace-mode))

(use-package inf-clojure
  :ensure t
  :init
  (defun figwheel ()
    (interactive)
    (run-clojure "lein figwheel"))
  :config
  (add-hook 'clojurescript-mode-hook 'inf-clojure-minor-mode))

(use-package company           :ensure t :pin "melpa")
(use-package company-cabal     :ensure t :pin "melpa")
(use-package company-ghci      :ensure t :pin "melpa")

(use-package deft              :ensure t :pin "melpa")
(use-package erlang            :ensure t :pin "melpa")
(use-package es-mode
  :ensure t
  :pin "melpa"
  :config
  (setq es-always-pretty-print t
        es-warn-on-delete-query nil))
(use-package flycheck          :ensure t :pin "melpa")
(use-package flycheck-haskell  :ensure t :pin "melpa")
(use-package flycheck-rust     :ensure t :pin "melpa")
(use-package geiser            :ensure t :pin "melpa")
(use-package gist              :ensure t :pin "melpa")
(use-package gnuplot           :ensure t :pin "melpa")

(use-package go-mode
  :ensure t
  :bind
  (:map go-mode-map
        ("C-c C-n" . clean-up-golang-buffer)
        ("M-." . godef-jump))
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq gofmt-command "goimports")
              ;; (add-hook 'before-save-hook #'gofmt-before-save)
              )))

(use-package graphviz-dot-mode :ensure t :pin "melpa")
(use-package groovy-mode       :ensure t :pin "melpa")
(use-package hamlet-mode       :ensure t :pin "melpa")
(use-package htmlize           :ensure t :pin "melpa")

;; Haskell

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :config
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(use-package haskell-emacs     :ensure t :pin "melpa")

(use-package hindent :ensure t :pin "melpa")

(use-package haskell-mode
  :ensure t
  :after (hindent)
  :pin "melpa"
  :config
  (add-hook 'haskell-mode-hook 'hindent-mode))

;; ===================================================================

(use-package ido-completing-read+ :ensure t :pin "melpa")
(use-package idris-mode        :ensure t :pin "melpa")
(use-package intero            :ensure t :pin "melpa")
(use-package js2-mode          :ensure t :pin "melpa")

(use-package ledger-mode
  :ensure t
  :bind (:map ledger-mode-map
              ("C-c C-n" . ledger-clean-up-transaction)
              ("C-c C-b" . ledger-bal-region)))

(use-package lua-mode       :ensure t :pin "melpa")
(use-package magit          :ensure t :pin "melpa")
(use-package magit-gh-pulls :ensure t :pin "melpa")
(use-package markdown-mode  :ensure t :pin "melpa")
(use-package nim-mode       :ensure t :pin "melpa")
(use-package nix-mode       :ensure t :pin "melpa")

(use-package ox-pandoc      :ensure t)

(use-package ox-reveal      :ensure t :pin "melpa")

(use-package org
  :ensure org-plus-contrib
  :pin "org"
  :after (org-agenda)
  :bind
  (:map
   org-mode-map
   ("C-c C-g" . aar/org-insert-github-link)
   :map
   org-agenda-mode-map
   ("s" . aar/org-agenda-save))
  :config
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (setq fill-column 80)
  ;;             (auto-fill-mode)
  ;;             (remove-hook 'haskell-mode-hook 'flycheck-mode t)
  ;;             ))
  (setq org-publish-project-alist
        '(("intro-to-infra-reveal"
           :base-directory "~/.org/talks/intro-to-infra"
           :recursive t
           :publishing-directory "/tmp"
           :publishing-function org-reveal-publish-to-reveal
           :section-numbers nil
           :with-toc nil
           :html-head "<link href=\"https://fonts.googleapis.com/css\?family=Oswald|Rammetto+One|Roboto\" rel=\"stylesheet\">")
          ("intro-to-infra-attach"
           :base-directory "~/.org/talks/intro-to-infra"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
           :recursive t
           :publishing-directory "/tmp"
           :publishing-function org-publish-attachment
           )
          ("intro-to-infra" :components ("intro-to-infra-reveal"
                                         "intro-to-infra-attach")))
        org-reveal-title-slide "<h2>%t</h2>
"))

(use-package org-journal
  :ensure t
  :pin "melpa"
  :config (setq org-journal-dir "~/src/org/journal/"))

(use-package paredit
  :ensure t
  :pin "melpa"
  :bind (:map paredit-mode-map
              ("M-)" . paredit-forward-slurp-sexp)
              ("M-(" . paredit-forward-barf-sexp)))

(use-package popwin
  :ensure t
  :pin "melpa"
  :config
  (popwin-mode 1)
  (push '("^\\*ES:.*\\*$"
          :regexp t
          :position right
          :stick t
          :width 80) popwin:special-display-config))

(use-package puppet-mode     :ensure t :pin "melpa")
(use-package purescript-mode :ensure t :pin "melpa")

(use-package psc-ide
  :ensure t
  :pin "melpa"
  :config
  (add-hook 'purescript-mode-hook
            '(lambda ()
               (psc-ide-mode)
               (company-mode)
               (flycheck-mode)
               (turn-on-purescript-indentation))))

(use-package rainbow-delimiters  :ensure t :pin "melpa")
(use-package rainbow-identifiers :ensure t :pin "melpa")

(use-package rust-mode
  :ensure t
  :pin "melpa"
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c TAB") #'rust-format-buffer)))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'flycheck-mode))

(use-package racer
  :ensure t
  :config
  (setq racer-cmd "~/.cargo/bin/racer")

  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package smex           :ensure t :pin "melpa")
(use-package textile-mode   :ensure t :pin "melpa")
(use-package terraform-mode :ensure t :pin "melpa")
(use-package tuareg         :ensure t :pin "melpa")
(use-package utop           :ensure t :pin "melpa")
(use-package merlin         :ensure t :pin "melpa")
(use-package yaml-mode      :ensure t :pin "melpa")
(use-package yasnippet      :ensure t :pin "melpa")

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
(setq abbrev-file-name "~/.emacs.d/abbrev_defs"
      save-abbrevs t)

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
;;(add-to-list 'Info-default-directory-list
;;     (expand-file-name "~/src/gnus/texi/"))

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
  (show-paren-mode 1))

(defun aar/massage-nrepl-bindings ()
  (define-key paredit-mode-map (kbd "C-c C-n") 'clean-up-buffer))

(add-hook 'emacs-lisp-mode-hook 'aar/lispy-parens)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)

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

;; nix
(require 'nix-mode)  ;; from ~/.nix-profile/.../site-lisp above
;; (nix-env -i emacs)

;; python
(require 'python)

;; javascript
(setq js-indent-level 2)

;; org

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(setq org-directory "~/.org"
      org-agenda-files (quote ("~/.org/PERSONAL.org"
                               "~/.org/clients"
                               "~/.org/orgs/elastic/people"
                               "~/.org/orgs/elastic/projects"
                               "~/.org/orgs/elastic/teams"))
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
        ("j" "Journal" entry (file+datetree "~/.org/notes.org")
         "* %?\nEntered on %U")
        ("n" "Note" entry
         (file+headline "~/.org/notes.org" "Notes")
         "* %u %?" :prepend t)))

;; erc

(require 'erc)
(require 'erc-highlight-nicknames)
(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'highlight-nicknames)

(setq erc-server "irc.freenode.net"
      erc-port 6697
      erc-nick "drewr"
      erc-user-full-name "Drew Raines"
      erc-email-userid "drew"       ; for when ident is not activated
      erc-fill-column 93
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
      erc-hide-list '("JOIN" "QUIT")
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
      "\\(84115\\|elast\\|pallet\\|lein\\|nash\\)"
      erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#84115" "#elasticsearch"
         "#purescript" "#haskell" "#haskell-beginners" "#s6"
         "#nixos")))

(define-key erc-mode-map (kbd "<f2>") 'browse-latest-url)

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

(defun aar/erc ()
  (interactive)
  (erc-tls :server "irc.freenode.net"
           :port 6697
           :nick "drewr"))

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
