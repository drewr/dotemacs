;; startup

(defmacro load-custom (f)
  "Load a local configuration file in ~/.emacs.d/."
  `(load (concat ,(expand-file-name "~/.emacs.d/") ,f)))

(defmacro add-lisp-dir (d)
  "Add dir to load-path."
  `(add-to-list 'load-path (concat ,(expand-file-name "~/.emacs.d/lisp/") ,d)))

;; For stack-installed utils
(setenv "PATH" (concat (expand-file-name "~/.local/bin") ":" (getenv "PATH")))

(load-custom
 (cl-case system-type
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
(require 'keychain-environment)
(keychain-refresh-environment)
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(require 'use-package)

;; https://github.com/jwiegley/use-package/issues/768#issuecomment-1918158963
(defun use-package-require (name &optional no-require body)
  (if use-package-expand-minimally
      (use-package-concat
       (unless no-require
         (list (use-package-load-name name)))
       body)
    (if no-require
        body
      (use-package-with-elapsed-timer
          (format "Loading package %s" name)
        `((if (not ,(use-package-load-name name))
              (display-warning 'use-package
                               (format "Cannot load %s" ',name)
                               :error)
            ,@body))))))

;; https://github.com/jwiegley/use-package/issues/768#issuecomment-1229781184
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(use-package adoc-mode    :ensure t :pin "melpa")
(use-package arduino-mode :ensure t :pin "melpa")
(use-package async        :ensure t :pin "melpa")
(use-package bbdb         :ensure t :pin "melpa")
(use-package cargo
  :ensure t
  :pin "melpa"
  :hook (rust-mode . cargo-minor-mode))

(use-package cider
  :ensure t
  :pin "melpa"
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
  :after (flycheck-clj-kondo)
  :config
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook 'aar/lispy-parens)
  (add-hook 'clojure-mode-hook 'aar/massage-nrepl-bindings)
  (add-hook 'clojure-mode-hook 'whitespace-mode)
  (put-clojure-indent 'assoc 1)
  (put-clojure-indent 'assoc-in 1)
  (put-clojure-indent 'match 1))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package inf-clojure
  :ensure t
  :init
  (defun figwheel ()
    (interactive)
    (run-clojure "lein figwheel"))
  :config
  (add-hook 'clojurescript-mode-hook 'inf-clojure-minor-mode))

(use-package company
  :ensure t
  :pin "melpa")

(use-package company-cabal     :ensure t :pin "melpa")
(use-package company-ghci      :ensure t :pin "melpa")

(use-package deft
  :ensure t
  :pin "melpa"
  :bind
  ("C-x d" . deft)
  :custom
  (deft-extensions '("txt" "tex" "org" "md"))
  (deft-directory "~/Sync/Notes")
  (deft-org-mode-title-prefix t)
  (deft-use-filename-as-title t)
  (deft-default-extension "org")
  (deft-file-naming-rules
    '((noslash . "-")
      (nospace . "-"))))

(use-package zetteldeft
  :ensure t
  :after deft
  :config (zetteldeft-set-classic-keybindings))

(use-package denote
  :ensure t
  :custom
  (denote-directory "~/Sync/Notes"))
(use-package denote-agenda  :ensure t)
(use-package denote-explore :ensure t)
(use-package denote-journal :ensure t)
(use-package denote-org     :ensure t)

(use-package avy  :ensure t)
(use-package ace-window  :ensure t)

(use-package dhall-mode        :ensure t :mode "\\.dhall\\'")
(use-package erlang            :ensure t :pin "melpa")

(use-package es-mode
  :ensure t
  :pin "melpa"
  :config
  (setq es-always-pretty-print t
        es-warn-on-delete-query nil))

(use-package flycheck          :ensure t :pin "melpa")
(use-package flycheck-rust     :ensure t :pin "melpa")
(use-package fzf               :ensure t :pin "melpa")
(use-package geiser            :ensure t :pin "melpa")
(use-package gist              :ensure t :pin "melpa")
(use-package gnuplot           :ensure t :pin "melpa")

(use-package go-mode
  :ensure t
  :bind
  (:map go-mode-map
        ("C-c C-n" . clean-up-golang-buffer)
        ("M-." . godef-jump))
  :init
  (setq gofmt-command "goimports")
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (add-hook 'before-save-hook 'gofmt-before-save))))

(use-package go-playground     :ensure t :pin "melpa")

(use-package graphviz-dot-mode :ensure t :pin "melpa")
(use-package groovy-mode       :ensure t :pin "melpa")
(use-package hamlet-mode       :ensure t :pin "melpa")
(use-package htmlize           :ensure t :pin "melpa")

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((haskell-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)

;; Haskell
(use-package lsp-haskell  :ensure t)
(use-package haskell-mode :ensure t)
(use-package flycheck-haskell  :ensure t :pin "melpa")



(use-package idris-mode        :ensure t :pin "melpa")
(use-package js2-mode          :ensure t :pin "melpa")

(use-package ledger-mode
  :ensure t
  :bind (:map ledger-mode-map
              ("C-c C-n" . ledger-clean-up-transaction)
              ("C-c C-b" . ledger-bal-region)))

(use-package lua-mode       :ensure t :pin "melpa")
(use-package magit          :ensure t :pin "melpa")
;;(use-package magit-gh-pulls :ensure t :pin "melpa")
(use-package markdown-mode  :ensure t :pin "melpa")
(use-package mustache-mode  :ensure t :pin "melpa")
(use-package nim-mode       :ensure t :pin "melpa")
(use-package nix-mode       :ensure t :pin "melpa")

;; only available from https://github.com/mattt-b/odin-mode/blob/master/odin-mode.el
(require 'odin-mode)
(use-package odin-mode
  :mode ("\\.odin\\'" . odin-mode)
  :hook (odin-mode . lsp))

;; Set up OLS as the language server for Odin, ensuring lsp-mode is loaded first
(with-eval-after-load 'lsp-mode
  ;; Helps find the ols.json file with Projectile or project.el
  (setq-default lsp-auto-guess-root t)
  (setq lsp-language-id-configuration
        (cons '(odin-mode . "odin") lsp-language-id-configuration))

  (lsp-register-client
   ;; Install manually after building https://github.com/DanielGavin/ols
   (make-lsp-client :new-connection (lsp-stdio-connection "/usr/local/bin/ols")
                    :major-modes '(odin-mode)
                    :server-id 'ols
                    ;; Ensures lsp-mode sends "workspaceFolders" to the server
                    :multi-root t)))

(use-package org-mime
  :ensure t
  :pin "melpa"
  :custom
  (org-mime-export-options '(:section-numbers nil
                                              :with-author nil
                                              :with-toc nil))
  (org-mime-export-ascii 'utf-8))

(use-package ox-pandoc      :ensure t)
(use-package ox-reveal      :ensure t :pin "melpa")

(use-package org
  :ensure org-contrib
  :pin gnu
  :after (org-agenda)
  :config
  (add-to-list 'org-modules 'org-habit)
  (require 'org-protocol)
  :bind
  (:map
   org-mode-map
   ("C-c C-g" . aar/org-link)
   :map
   org-agenda-mode-map
   ("s" . aar/org-agenda-save)))

(use-package org-journal
  :ensure t
  :pin "melpa"
  :init (setq org-journal-dir "~/.org/journal/"))

(use-package paredit
  :ensure t
  :pin "melpa"
  :bind (:map paredit-mode-map
              ("M-)" . paredit-forward-slurp-sexp)
              ("M-(" . paredit-forward-barf-sexp)))

(use-package popup
  :ensure t
  :pin "melpa")

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
  :hook ((flycheck-mode . flycheck-rust-setup)
         (rust-mode . (lambda ()
                        (local-set-key (kbd "C-c TAB") #'rust-format-buffer)))
         (rust-mode . flycheck-mode)
         (rust-mode . company-mode))
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer")))

(use-package rust-playground :ensure t :pin "melpa")

(use-package smex           :ensure t :pin "melpa")
(use-package textile-mode   :ensure t :pin "melpa")
(use-package terraform-mode :ensure t :pin "melpa")
(use-package toml-mode      :ensure t :pin "melpa")
(use-package yaml-mode      :ensure t :pin "melpa")
(use-package yasnippet      :ensure t :pin "melpa")
(use-package vundo          :ensure t :pin "gnu")
(use-package which-key      :ensure t)
(use-package zig-mode       :ensure t :pin "melpa")

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(recentf-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
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
(use-package tuareg         :ensure t :pin "melpa")
(use-package utop           :ensure t :pin "melpa")
(use-package merlin         :ensure t :pin "melpa")

(let* ((ocaml-lisp "~/.opam/default/share/emacs/site-lisp")
       (tuareg-site-file
        (concat ocaml-lisp "/tuareg-site-file.el")))
  (if (file-exists-p ocaml-lisp)
      (progn
        (add-to-list 'load-path ocaml-lisp)
        (require 'ocamlformat)
        (add-hook 'before-save-hook 'ocamlformat-before-save))
    (message "opam site-lisp doesn't exist yet"))
  (if (file-exists-p tuareg-site-file)
      (progn
        (load tuareg-site-file)
        (add-hook 'tuareg-mode-hook 'merlin-mode))
    (message "run `opam install tuareg`")))

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; nix
(require 'nix-mode)  ;; from ~/.nix-profile/.../site-lisp above
;; (nix-env -i emacs)

;; python
(require 'python)

;; javascript
(setq js-indent-level 2)

;; org

;; Hack to fix the #+TITLE stuff with ox-publish
;; https://github.com/yjwen/org-reveal/issues/171#issuecomment-168372894
;; ...except it started causing recursive loading, so you have to do
;; it after Emacs starts.
(defun aar/reload-org ()
  (let ((current-prefix-arg 1))
    (call-interactively 'org-reload)))

;; Unfortunately this doesn't work on startup
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook
             'compilation-finish-functions 'aar/reload-org nil 'local)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(setq org-directory "~/.org"
      org-refile-targets '((nil :maxlevel . 4)
                           (org-agenda-files :maxlevel . 4))
      org-refile-use-outline-path t
      org-log-done t
      org-agenda-files "~/.org/agenda-files.txt"
      org-agenda-span 'day
      org-agenda-show-all-dates t
      org-agenda-ndays 7
      org-agenda-start-on-weekday nil
      org-agenda-start-with-log-mode t
      org-agenda-start-with-follow-mode nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-with-clockreport-mode nil
      org-export-publishing-directory "~/tmp/org/export"
      org-reverse-note-order t
      org-clock-modeline-total 'current
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull "~/.org/mobile-inbox.org"
      org-attach-use-inheritance t
      org-adapt-indentation nil)

;; Adapted from https://emacs.stackexchange.com/a/71951
(defun aar/org-attach (f &rest args)
  (if (string-match "EQUINIX.org" (buffer-file-name))
      (let ((org-attach-id-dir "orgs/equinix/data/"))
        (funcall f))
    (funcall f)))
(advice-add 'org-attach :around #'aar/org-attach)

;; To use reveal.js:
;;
;;   cd ~/tmp \
;;     && git clone --depth=1 https://github.com/hakimel/reveal.js.git \
;;     && cd reveal.js \
;;     && npm install \
;;     && http-server
(setq org-publish-project-alist
      '(("intro-to-infra-reveal"
         :base-directory "~/.org/talks/intro-to-infra"
         :recursive t
         :publishing-directory "~/tmp/reveal.js"
         :publishing-function org-reveal-publish-to-reveal
         :section-numbers nil
         :with-toc nil
         :html-head "<link href=\"https://fonts.googleapis.com/css\?family=Oswald|Rammetto+One|Roboto\" rel=\"stylesheet\">")
        ("intro-to-infra-attach"
         :base-directory "~/.org/talks/intro-to-infra"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
         :recursive t
         :publishing-directory "~/tmp/reveal.js"
         :publishing-function org-publish-attachment
         )
        ("intro-to-infra" :components ("intro-to-infra-reveal"
                                       "intro-to-infra-attach")))
      org-reveal-title-slide "<h1>%t</h1>")

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DEFERRED"
                  "|" "DONE" "STALE" "DELEGATED" "CANCELED")))

(setq org-export-html-postamble "<p class=\"postamble\">%d %a</p>")

(setq org-default-notes-file "~/.org/notes.org")

(setq org-capture-templates
      `(("p" "Personal tasks")
        ("pp" "Personal task - clocked-in" entry
         (file+headline "PERSONAL.org" "Tasks")
         "* %? %^G\n"
         :jump-to-captured t
         :empty-lines-before 1
         :clock-in t
         :clock-keep t
         :prepend t)
        ("pt" "Personal TODO" entry
         (file+headline "PERSONAL.org" "Tasks")
         "* TODO %?\n  %T\n" :prepend t)
        ("e" "Equinix")
        ("ee" "Equinix task - clocked-in" entry
         (file+olp+datetree "EQUINIX.org" "Journal")
         "* %? %^G"
         :jump-to-captured t
         :empty-lines-before 1
         :clock-in t
         :clock-keep t
         :tree-type week)
        ("e1" "Equinix 1-1" entry
         (file+olp+datetree "EQUINIX.org" "Journal")
         "* %? (%(format-time-string \"%Y%m%d\")) :1on1:"
         :jump-to-captured t
         :empty-lines-before 1
         :clock-in t
         :clock-keep t
         :tree-type week)
        ("et" "Equinix TODO"
         entry (file+olp+datetree "EQUINIX.org" "Journal")
         "* TODO %? %^G\n  %T"
         :tree-type week)
        ("ej" "Equinix Journal" entry
         (file+olp+datetree "EQUINIX.org" "Journal")
         (function aar/org-capture-journal)
         :empty-lines-before 1
         :jump-to-captured t
         :tree-type week)
        ("t" "Trinity" entry
         (file+headline "clients/TRINITY.org" "Tasks")
         "* TODO %?\n  %u"
         :empty-lines-before 1
         :prepend t)
        ("j" "Journal" entry
         (file+datetree "journal.org")
         (function aar/org-capture-journal)
         :empty-lines-before 1
         :jump-to-captured t)
        ("n" "New note" plain
         (file denote-last-path)
         #'denote-org-capture
         :no-save t
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured t)
        ;; For use with https://github.com/sprig/org-capture-extension
        ("p" "Protocol" entry
         (file+datetree "journal.org")
         ,(concat "* %?\n%T\n[[%:link][%:description]]\n\n"
                  "#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n")
         :empty-lines-before 1)
        ("L" "Protocol Link" entry
         (file+datetree "journal.org")
         "* %? [[%:link][%:description]] \nCaptured: %T"
         :empty-lines-before 1)))

(add-hook 'org-mode-hook
          (lambda ()
            (setq fill-column 80)
            (remove-hook 'haskell-mode-hook 'flycheck-mode t)))

;; erc

(require 'erc)
(require 'erc-sasl)
(add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")

(require 'erc-highlight-nicknames)
(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'highlight-nicknames)

(setq erc-server "irc.libera.chat"
      erc-port 6697
      erc-nick "drewr"
      erc-user-full-name "Drew Raines"
      erc-email-userid "drew"       ; for when ident is not activated
      erc-fill-column 84
      erc-fill-prefix "   "
      erc-auto-query 'window-noselect
      erc-prompt-for-nickserv-password nil
      erc-nickserv-passwords nil
      erc-auto-discard-away t
      erc-autoaway-idle-seconds 600
      erc-log-insert-log-on-open nil
      erc-log-channels-directory "~/tmp/irc/log"
      erc-log-write-after-insert t
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
      '(("libera.chat" "#emacs" "#84115" "#elasticsearch"
         "#purescript" "#haskell" "#haskell-beginners" "#s6"
         "#nixos" "##rust" "#nim" "#fennel" "#zig"
         "#janet")))

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
  (erc-tls :server "irc.libera.chat"
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

;; Stopwatch
(require 'stopwatch)

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
 '(circe-my-message-face ((t (:foreground "DarkRed"))))
 '(org-scheduled-previously ((t (:foreground "dark orchid"))))
 '(org-warning ((t (:foreground "dark blue"))))
 '(org-upcoming-deadline ((t (:foreground "hot pink"))))
 '(org-upcoming-distant-deadline ((t (:foreground "light pink"))))
 '(variable-pitch ((t nil))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
