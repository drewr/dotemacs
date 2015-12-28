default:
	@echo you should make clean install

compile:
	bin/compile

install:
	emacs -Q --batch --eval '(setq package-user-dir "~/.emacs.d-build/elpa")' -l d/setup.el
	mv ~/.emacs.d ~/.emacs.d.$(shell date -u +%s)
	mv ~/.emacs.d-build ~/.emacs.d
	bin/install

clean:
	find . -name \*.elc | xargs rm

publish:
	bin/publish

