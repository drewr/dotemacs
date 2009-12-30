deploy:
	( cd d; tar cf - . ) | ( cd ${HOME}; tar xvf - )
