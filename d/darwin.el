(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/opt/local/sbin")
(add-to-list 'exec-path "/sw/bin")
(add-to-list 'exec-path "/sw/sbin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

;; for M-x shell
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setenv "PATH" (concat "/opt/local/bin" ":" (getenv "PATH")))
(setenv "PATH" (concat (expand-file-name "~/.nix-profile/bin") ":" (getenv "PATH")))
(setenv "PATH" (concat (expand-file-name "~/.local/bin") ":" (getenv "PATH")))
(setenv "PATH" (concat (expand-file-name "~/bin") ":" (getenv "PATH")))

(setq shell-file-name "zsh")

(when (>= emacs-major-version 23)
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier 'super)

  ;; or:

  (custom-set-variables
   '(ns-alternate-modifier (quote super))
   '(ns-command-modifier (quote meta)))

  ;; Or do neither of these and switch CMD/OPT in OS X keyboard prefs

  (when window-system
    (set-face-font 'default
                   "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
    (global-set-key "\M-`" 'other-frame)))

(set-face-attribute 'default nil :height 150)

(add-to-list 'default-frame-alist '(width . 103))
(add-to-list 'default-frame-alist '(height . 35))
