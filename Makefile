default:
	@echo you should make clean install

compile:
	bin/compile

gnus:
	cp d/gnus.el ~/.emacs.d/gnus.el

install:
	#emacs -Q --batch --eval '(setq package-user-dir "~/.emacs.d-build/elpa")' -l d/setup.el
	mkdir ~/.emacs.d-build
	mv ~/.emacs.d ~/.emacs.d.$(shell date -u +%s)
	mv ~/.emacs.d-build ~/.emacs.d
	bin/install

clean:
	find . -name \*.elc | xargs rm

publish:
	bin/publish

