.PHONY: all benchmark tests clean
all:
	dune exec exe
benchmark:
	dune build bench/bench.exe
ifdef arg
	dune exec bench/bench.exe $(arg)
else
	dune exec bench/bench.exe -all
endif

tests:
	dune test
clean:
	dune clean
