;; startup

(defmacro load-custom (f)
  "Load a local configuration file in ~/.emacs.d/."
   `(load-file
     (concat ,(expand-file-name "~/.emacs.d/") ,f ".el")))

(require 'cl) ; need this for (case)
(load-custom
  (case system-type
   ('darwin "darwin")
   ('gnu/linux "linux")
   ('windows-nt "win32")))
(load-custom "funs")
(load-custom "sonian")

(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(add-to-list 'default-frame-alist '(width . 103))
(add-to-list 'default-frame-alist '(height . 35))

(require 'ffap)
(require 'package)
(package-initialize)
(toggle-uniquify-buffer-names)
(recentf-mode 1)
(ido-mode t)
(column-number-mode 1)
(setq display-time t
      display-time-24hr-format t)
(display-time)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-x C-i") 'ido-imenu)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-n") 'clean-up-buffer)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
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

(when (equalp "DUMB" (getenv "TERM"))
  (setenv "PAGER" "cat"))

(eval-after-load 'diff-mode
  '(progn (set-face-foreground 'diff-added "green4")
          (set-face-foreground 'diff-removed "red3")))

;; magit

(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(eval-after-load 'magit
  '(progn (set-face-background 'magit-item-highlight "white")
          (set-face-foreground 'magit-diff-add "green3")
          (set-face-foreground 'magit-diff-del "red3")))


;; lisp

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defun aar/lispy-parens ()
  "Set up parens for lispish modes."
  (require 'paredit)
  (paredit-mode 1)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)
  (show-paren-mode 1))
(add-hook 'emacs-lisp-mode-hook 'aar/lispy-parens)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'clojure-mode-hook 'aar/lispy-parens)
(add-hook 'clojure-mode-hook 'whitespace-mode)

;; clojure

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/slime"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/clojure-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/swank-clojure"))
(require 'slime)
(slime-setup '(slime-repl))
(require 'clojure-mode)
(require 'clojure-test-mode)
(load "swank-clojure")
(setq slime-net-coding-system 'utf-8-unix)
(setq swank-clojure-binary (expand-file-name "~/bin/clojure"))
(require 'assoc)  ;; swank-clojure-project fails without

;; org

(org-remember-insinuate)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key [(control meta ?r)] 'org-remember)
(setq org-directory "~/.org"
      org-agenda-files (quote ("~/.org/PERSONAL.org"
                               "~/.org/clients"))
      org-log-done t
      org-agenda-show-all-dates t
      org-agenda-ndays 7
      org-agenda-start-on-weekday nil
      org-export-publishing-directory "~/.org/export"
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-reverse-note-order t
      org-clock-modeline-total 'current)
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DEFERRED"
                  "|" "DONE" "DELEGATED" "CANCELED")))

(setq org-default-notes-file "~/.org/notes.org")

(setq org-remember-templates
      '(("Personal task" 112 "* TODO %?\n  %u" "PERSONAL.org" 'top)
        ("NotifyMD" 109 "* TODO %?\n  %u" "clients/NMD.org" 'top)
        ("Trinity" 116 "* TODO %?\n  %u" "clients/TRINITY.org" 'top)
        ("Sonian" 115 "* TODO %?\n  %u" "clients/SONIAN.org" 'top)
        ("Note" 110 "* %u %?" "notes.org" 'bottom)))

;; ledger

(require 'ledger)


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
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil nil (menu-bar))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(safe-local-variable-values (quote ((prompt-to-byte-compile))))
 '(scroll-bar-mode nil)
 '(sh-indentation 2)
 '(show-paren-mode nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode nil)
 '(truncate-lines nil)
 '(require-final-newline t)
 '(whitespace-style '(trailing lines space-before-tab
                                indentation space-after-tab))
 '(whitespace-line-column 80))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(circe-my-message-face ((t (:foreground "DarkRed")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

