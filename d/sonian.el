(let ((f (expand-file-name "~/src/sa-safe/.elisp/sonian-navigation.el")))
  (when (file-exists-p f)
    (load-file f)
    (with-eval-after-load 'clojure-mode
      (define-key clojure-mode-map (kbd "C-c C-o C-j") 'sonian-jump)
      (define-key clojure-mode-map (kbd "C-c C-o C-s") 'sonian-swap))))
