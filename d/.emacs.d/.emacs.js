; -*- emacs-lisp -*-

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . js2-mode))

(defun js-eval-buffer ()
  (interactive)
  (let ((f (make-temp-file "__js_exec_")))
    (write-region nil nil f)
    (shell-command
     (format "java -cp $HOME/tmp/src/jar/\\\* org.mozilla.javascript.tools.shell.Main %s" f))
    (delete-file f)))

(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-c\C-b" 'js-eval-buffer)))
