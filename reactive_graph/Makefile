SHELL := /bin/sh

.PHONY: traditional traditional-run reactive-runtime reactive reactive-run clean

traditional:
	dune build --root . traditional_analysis/bin/traditional_main.exe

traditional-run: traditional
	dune exec --root . traditional_analysis/bin/traditional_main.exe -- data

reactive-runtime:
	$(MAKE) -C vendor/skip-ocaml build/libskip_reactive.a

reactive: reactive-runtime
	dune build --root . reactive_analysis/bin/reactive_main.exe

reactive-run: reactive
	dune exec --root . reactive_analysis/bin/reactive_main.exe -- data

clean:
	dune clean --root .
	$(RM) graph.rheap
	$(MAKE) -C vendor/skip-ocaml clean
