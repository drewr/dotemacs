default:
	@echo you should make clean install

compile:
	bin/compile

gnus:
	cp d/gnus.el ~/.emacs.d/gnus.el

install:
	#emacs -Q --batch --eval '(setq package-user-dir "~/.emacs.d-build/elpa")' -l d/setup.el
	#mkdir ~/.emacs.d-build
	#mv ~/.emacs.d ~/.emacs.d.$(shell date -u +%s)
	#mv ~/.emacs.d-build ~/.emacs.d
	bin/install
	emacs --batch --eval '(message "----> running init")' -l d/setup.el -l d/init.el
	bin/compile

clean:
	rm -rf ~/.emacs.d
	find . -name \*.elc | xargs rm -f

publish:
	bin/publish

