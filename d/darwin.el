(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/opt/local/sbin")
(add-to-list 'exec-path "/sw/bin")
(add-to-list 'exec-path "/sw/sbin")
(add-to-list 'exec-path "/usr/local/bin")

;; for M-x shell
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setenv "PATH" (concat "/opt/local/bin" ":" (getenv "PATH")))
(setenv "PATH" (concat (expand-file-name "~/bin") ":" (getenv "PATH")))

(setq shell-file-name "/opt/local/bin/zsh")
(setenv "SHELL" "/opt/local/bin/zsh")

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

(require 'erc)
(defun aar/you-rang? (mat nick message)
  (let ((buf (buffer-name (current-buffer)))
        (msg (concat "<" (car (erc-parse-user nick)) "> " message)))
    (when (eq mat 'current-nick)
      (unless (posix-string-match "^\\** *Users on #" message)
        (growl buf msg)
        nil))))

(defun aar/erc-me (proc parsed)
  (let* ((chan (car (erc-response.command-args parsed)))
         (nickraw (erc-response.sender parsed))
         (nick (car (erc-parse-user nickraw)))
         (msg (concat "<" nick "> " (erc-response.contents parsed))))
    (when (and (or (string-match erc-favorite-channels chan)
                   (string= chan (erc-current-nick)))
               (not (erc-ignored-user-p nickraw)))
      (growl chan msg)
      nil)))

(add-hook 'erc-text-matched-hook 'aar/you-rang?)
(add-hook 'erc-server-PRIVMSG-functions 'aar/erc-me)
