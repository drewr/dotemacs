default:
	@echo you should make clean install

compile:
	bin/compile

install: install-packages
	emacs --batch -l install.el
	bin/install

clean:
	find . -name \*.elc | xargs rm

publish:
	bin/publish

install-packages:
	rm -rf ~/.emacs.d/elpa
	emacs -Q --batch -l d/setup.el
