default:
	@echo you should make clean install

compile:
	bin/compile

install:
	emacs -Q --batch --eval '(setq package-user-dir "~/.emacs.d-build/elpa")' -l d/setup.el
	rm -rf ~/.emacs.d
	mv ~/.emacs.d-build ~/.emacs.d
	emacs --batch -l install.el
	bin/install

clean:
	find . -name \*.elc | xargs rm

publish:
	bin/publish

