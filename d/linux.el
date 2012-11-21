(setenv "PATH" (concat (expand-file-name "~/bin") ":"
                       (getenv "PATH")))

(set-face-attribute 'default nil :family "Inconsolata" :height 120)

(setq browse-url-chromium-program "google-chrome")
(setq browse-url-browser-function 'browse-url-chromium)
