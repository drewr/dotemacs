(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://stable.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)
(package-install 's)
(package-install 'dash)
(package-install 'pkg-info)
(package-install 'cider)
(package-install 'clojure-mode)
(package-install 'haskell-mode)
(package-install 'magit)

