.PHONY: install uninstall clean examples benchmarks

install:	
	rm -rf opam/
	oasis2opam --local
	opam pin add sysyf . 

uninstall:	
	@if [ -d "examples/build" ]; then rm -rf examples/build; fi
	@if [ -d "benchmarks/build" ]; then rm -rf benchmarks/build; fi
	opam uninstall sysyf 
	opam unpin sysyf 

clean: 	
	@if [ -d "examples/build" ]; then rm -rf examples/build; fi
	@if [ -d "benchmarks/build" ]; then rm -rf benchmarks/build; fi
	@if [ -d "_build" ]; then rm -rf _build; fi
	@if [ -e "sysyf_ext.native" ]; then rm sysyf_ext.native; fi 

examples: 
	@make -C examples

benchmarks: 	
	@make -C benchmarks

translator: 
	@ocamlbuild -package compiler-libs.common ppx/sysyf_ext.native

translate:
	@ocamlc -dsource -ppx ./sysyf_ext.native $(SRC)