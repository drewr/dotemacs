default:
	@echo you should make clean install

# Compile LOCALLY
compile:
	bin/compile

install:
	bin/install

# This has side effects in ~/.emacs.d from the package installations
load:
	emacs --batch --eval '(message "----> running init")' -l d/setup.el -l d/init.el

# Copy everything and build it
install-and-compile: install load
	cd ~/.emacs.d && ./compile

clean:
	rm -rf ~/.emacs.d
	find . -name \*.elc | xargs rm -f

publish:
	bin/publish

