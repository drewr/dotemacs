(defvar demo-slide-separator "^=-= ")

(defun demo-current-slide-number ()
  (interactive)
  (save-excursion
    (let ((thing (search-backward-regexp demo-slide-separator nil t 1)))
      (end-of-line)
      (backward-word)
      (when thing
        (string-to-number (thing-at-point 'word))))))

(defun demo-align ()
  (interactive)
  (let ((p (point)))
    (when (not (= 1 (demo-current-slide-number)))
      (demo-prev-slide)
      (demo-next-slide))
    (goto-char p)))

(defun demo-goto-pattern (pat)
  (let ((p (save-excursion
             (search-forward-regexp pat nil t))))
    (when p
      (goto-char p)
      (forward-line 1)
      (recenter-top-bottom 0))))

(defun demo-renumber-slides ()
  (interactive)
  (save-excursion
    (setq n 1)
    (goto-char 0)
    (while (search-forward-regexp demo-slide-separator nil t)
      (when (not (= (point) (line-end-position)))
        (kill-line))
      (insert (number-to-string n))
      (setq n (1+ n)))))

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
  (let ((p (search-backward-regexp demo-slide-separator nil t 2)))
    (when p
      (demo-next-slide))))

(defvar demo-mode-map (make-sparse-keymap)
  "Keymap for the demo minor mode.")

(define-key demo-mode-map (kbd "C-c C-c") 'demo-goto-slide)
(define-key demo-mode-map (kbd "C-c C-n") 'demo-next-slide)
(define-key demo-mode-map (kbd "C-c C-p") 'demo-prev-slide)
(define-key demo-mode-map (kbd "C-c C-l") 'demo-align)

(define-minor-mode demo-mode
  "Minor mode for slide-based presentation.  Meant for interactivity."
  :lighter " Demo")

(provide 'demo)
