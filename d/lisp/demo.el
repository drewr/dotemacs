(defvar demo-slide-separator "^=-= ")

(defun demo-goto-pattern (pat)
  (let ((p (save-excursion
             (re-search-forward pat nil t))))
    (when p
      (goto-char p)
      (next-line)
      (recenter-top-bottom 0))))

(defun demo-goto-slide (&optional n)
  (interactive "P")
  (if n
      (progn
        (goto-char 0)
        (demo-goto-pattern
         (concat demo-slide-separator (number-to-string n) "$")))
    (demo-next-slide)))

(defun demo-next-slide ()
  (interactive)
  (demo-goto-pattern demo-slide-separator))

(defun demo-prev-slide ()
  (interactive)
  (let ((p (re-search-backward demo-slide-separator nil t 2)))
    (when p
      (demo-next-slide))))

(defvar demo-mode-map (make-sparse-keymap)
  "Keymap for the demo minor mode.")

(define-key demo-mode-map (kbd "C-c C-c") 'demo-goto-slide)
(define-key demo-mode-map (kbd "C-c C-n") 'demo-next-slide)
(define-key demo-mode-map (kbd "C-c C-p") 'demo-prev-slide)

(define-minor-mode demo-mode
  "Minor mode for slide-based presentation.  Meant for interactivity."
  :lighter " Demo")

(provide 'demo)
