format-all:
	brittany --write-mode=inplace app/*hs \
	&& brittany --write-mode=inplace test/*.hs \
	&& brittany --write-mode=inplace src/*.hs
