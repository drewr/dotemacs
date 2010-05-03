compile:
	bin/compile

install:
	( cd d; tar cf - . ) | ( cd ${HOME}; tar xvf - )

clean:
	find . -name \*.elc | xargs rm
