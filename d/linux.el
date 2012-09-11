(setenv "PATH" (concat (expand-file-name "~/bin") ":"
                       (getenv "PATH")))

(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 90)

(setq browse-url-chromium-program "google-chrome")
(setq browse-url-browser-function 'browse-url-chromium)
