compile:
	bin/compile

install:
	( cd d; tar cf - . ) | ( mkdir -p ${HOME}/.emacs.d; cd ${HOME}/.emacs.d; tar xvf - )

clean:
	find . -name \*.elc | xargs rm

