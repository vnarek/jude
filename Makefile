.PHONY : doc
doc :
	mkdir -p _build/.docdir
	dune build @doc
	cp -R _build/default/_doc/_html/* _build/.docdir