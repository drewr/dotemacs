(setenv "PATH" (concat (expand-file-name "~/.nix-profile/bin") ":"
                       (expand-file-name "~/.local/bin") ":"
                       (expand-file-name "~/bin") ":"
                       (getenv "PATH")))

(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

(set-face-attribute 'default nil :family "Inconsolata" :height 130)

(setq browse-url-chromium-program "google-chrome")
(setq browse-url-browser-function 'browse-url-chromium)
