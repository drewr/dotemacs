(setq package-archives
      '(("ELPA"           . "http://tromey.com/elpa/")
        ("SC"             . "http://joseito.republika.pl/sunrise-commander/")
        ("gnu"            . "http://elpa.gnu.org/packages/")
        ("marmalade"      . "http://marmalade-repo.org/packages/")
        ("melpa"          . "http://melpa.org/packages/")
        ("melpa-stable"   . "http://stable.melpa.org/packages/")
        ("org"            . "http://orgmode.org/elpa/"))
      package-pinned-packages
      '((arduino-mode        . "melpa")
        (cider               . "melpa-stable")
        (clojure-mode        . "melpa")
        (company             . "melpa")
        (company-cabal       . "melpa")
        (company-ghci        . "melpa")
        (deft                . "melpa")
	(w3m                 . "melpa")
        (flycheck-rust       . "melpa")
        (gist                . "melpa")
        (gnuplot             . "melpa")
        (graphviz-dot-mode   . "melpa")
        (hamlet-mode         . "melpa")
        (haskell-emacs       . "melpa")
        (haskell-mode        . "melpa")
        (hindent             . "melpa")
        (ido-ubiquitous      . "melpa")
        (idris-mode          . "melpa")
        (js2-mode            . "melpa")
        (magit               . "melpa")
        (magit-gh-pulls      . "melpa")
        (markdown-mode       . "melpa")
        (nix-mode            . "melpa")
        (org                 . "org")
;;        (org-ac              . "melpa")  these all seem broken
;;        (org-magit           . "melpa")
;;        (org-pandoc          . "melpa-stable")
;;        (org-trello          . "melpa-stable")
        (paredit             . "melpa")
        (rainbow-delimiters  . "melpa")
        (rainbow-identifiers . "melpa")
        (rust-mode           . "melpa")
        (smex                . "melpa")
        (textile-mode        . "melpa")
        (yaml-mode           . "melpa")
        (yasnippet           . "melpa")))

(defun bootstrap ()
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (package)
          (let ((pkg (car package)))
            (unless (require pkg nil t)
              (package-install pkg))))
        package-pinned-packages))

(require 'package)
(package-initialize t)
(bootstrap)
