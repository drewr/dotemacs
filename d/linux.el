(setenv "PATH" (concat (expand-file-name "~/bin") ":"
                       (getenv "PATH")))

(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 102)

(setq browse-url-browser-function 'browse-url-chromium)
