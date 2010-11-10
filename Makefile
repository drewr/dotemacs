default:
	@echo you should make clean install

compile:
	bin/compile

install:
	emacs --batch --eval '(byte-compile-file "d/lisp/js2-20090723b.el")'
	bin/install

clean:
	find . -name \*.elc | xargs rm

publish:
	bin/publish
