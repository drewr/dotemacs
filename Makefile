default:
	@echo you should make clean install

compile:
	bin/compile

install:
	emacs --batch -l install.el
	bin/install

clean:
	find . -name \*.elc | xargs rm

publish:
	bin/publish
