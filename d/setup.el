(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)
(package-install 's)
(package-install 'dash)
(package-install 'pkg-info)
(package-install 'cider)
(package-install 'cider-nrepl)
(package-install 'clojure-mode)

