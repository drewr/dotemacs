; -*- emacs-lisp -*-

(require 'ruby-mode)
(require 'ruby-electric)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.mab$" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(require 'inf-ruby)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

; Rails
(add-to-list 'load-path (expand-file-name "~/elisp/emacs-rails"))
(require 'snippet)
(require 'find-recursive)
(defun try-complete-abbrev (old)
 (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
     '(try-complete-abbrev
       try-complete-file-name
       try-expand-dabbrev))
;(require 'rails)
;(setq rails-use-mongrel t
;      rails-ruby-command "/usr/local/bin/ruby")
(add-to-list 'load-path (expand-file-name "~/elisp/rhtml"))
(require 'rhtml-mode)
(require 'rhtml-erb)

; MMM
;;;; (add-to-list 'load-path (expand-file-name "~/elisp/mmm"))
;;;; (require 'mmm-mode)
;;;; (require 'mmm-auto)
;;;; (setq mmm-global-mode 'maybe)
;;;; (setq mmm-submode-decoration-level 2)
;;;; (set-face-background 'mmm-output-submode-face  "lightBlue2")
;;;; (set-face-background 'mmm-code-submode-face    "lightgoldenrod2")
;;;; (set-face-background 'mmm-comment-submode-face "DarkOliveGreen")
;;;; (mmm-add-classes
;;;;  '((erb-code
;;;;     :submode ruby-mode
;;;;     :match-face (("<%#" . mmm-comment-submode-face)
;;;;                  ("<%=" . mmm-output-submode-face)
;;;;                  ("<%"  . mmm-code-submode-face))
;;;;     :front "<%[#=]?"
;;;;     :back "%>"
;;;;     :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
;;;;              (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
;;;;              (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
;;;;     )))
;;;; (add-hook 'html-mode-hook
;;;;           (lambda ()
;;;;             (setq mmm-classes '(erb-code))
;;;;             (mmm-mode-on)))
;;;; (add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
;;;; (global-set-key [f8] 'mmm-parse-buffer)

