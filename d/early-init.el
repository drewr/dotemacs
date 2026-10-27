;;; early-init.el --- Prerun-performance settings -*- lexical-binding: t; -*-

;; Set this before anything else to avoid pop-ups after start.
(setq native-comp-async-report-warnings-errors 'silent)

;; Let init run fast: skip GC while starting up and avoid the cost of
;; remote/archive file handlers.  Both are restored after startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(defvar aar/default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore normal file handling, and GC settings tuned for Emacs 31's
;; native-comp runtime.  The stock 32 MB threshold caused a noticeable
;; ~50-100 ms garbage-collect hitch roughly every second while typing;
;; with native-compiled bytecode churning allocations faster, full GCs
;; fired too often.  256 MB keeps the stall rare while staying within
;; sane memory use.
(defun aar/restore-startup-tuning ()
  "Restore normal GC and file handling after startup."
  (run-at-time 5 nil
               (lambda ()
                 (setq gc-cons-threshold (* 256 1024 1024)
                       gc-cons-percentage 0.1
                       file-name-handler-alist aar/default-file-name-handler-alist))))

(add-hook 'emacs-startup-hook 'aar/restore-startup-tuning)

;; One consolidated autoloads file instead of one per ELPA package.
(setq package-quickstart t)

;; Don't let terminal capability queries (OSC fg/bg color) block startup
;; by the default 2s each in terminals that don't answer them (SSH/tmux).
(setq xterm-query-timeout 0.1)

;; Don't wait on frame layout during startup.
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here
