(setenv "PATH" (concat (expand-file-name "~/bin") ":"
                       (getenv "PATH")))

(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

(set-face-attribute 'default nil :family "Inconsolata" :height 120)

(setq browse-url-chromium-program "google-chrome")
(setq browse-url-browser-function 'browse-url-chromium)
