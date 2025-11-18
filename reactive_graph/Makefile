SHELL := /bin/sh

.PHONY: traditional traditional-run reactive-runtime reactive reactive-run clean

traditional:
	cd .. && dune build reactive_graph/traditional_analysis/bin/traditional_main.exe

traditional-run: traditional
	cd .. && dune exec reactive_graph/traditional_analysis/bin/traditional_main.exe -- reactive_graph/data

reactive-runtime:
	$(MAKE) -C vendor/skip-ocaml build/libskip_reactive.a

reactive: reactive-runtime
	cd .. && dune build reactive_graph/reactive_analysis/bin/reactive_main.exe

reactive-run: reactive
	cd .. && dune exec reactive_graph/reactive_analysis/bin/reactive_main.exe -- reactive_graph/data

clean:
	dune clean --root .
	$(RM) graph.rheap
	$(MAKE) -C vendor/skip-ocaml clean
