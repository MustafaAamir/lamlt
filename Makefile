all:
	dune exec lcaml
benchmark:
	dune build bench/bench.exe
	dune exec bench/bench.exe
