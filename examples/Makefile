.PHONY: all 

all: 
	@if [ -d "build" ]; then rm -rf build; fi
	@mkdir build
	@mkdir build/lib
	@mkdir build/linear_regression
	@mkdir build/neural_network
	@mkdir build/rnn
	@cp lib/* build/lib/

	@ocamlfind ocamlc -package sysyf,sysyf.ppx -c build/lib/sysyf_learning.mli 


	@cp linear_regression/* build/linear_regression/ 
	@for f in $(shell ls linear_regression/*.ml); do \
		NAME=$$(basename $${f} .ml); \
		echo "compiling $${NAME}.ml ....."; \
		ocamlfind ocamlopt -I build/lib/ -w -1..66 -package sysyf,sysyf.ppx,csv -o build/$${NAME} -linkpkg build/lib/sysyf_learning.ml build/linear_regression/$${NAME}.ml; \
	 done 

	@cp neural_network/* build/neural_network/ 
	@for f in $(shell ls neural_network/*.ml); do \
		NAME=$$(basename $${f} .ml); \
		echo "compiling $${NAME}.ml ....."; \
		ocamlfind ocamlopt -I build/lib/ -w -1..66 -package sysyf,sysyf.ppx,csv -o build/$${NAME} -linkpkg build/lib/sysyf_learning.ml build/neural_network/$${NAME}.ml; \
	 done 	

	@cp rnn/* build/rnn/ 
	@for f in $(shell ls rnn/*.ml); do \
		NAME=$$(basename $${f} .ml); \
		echo "compiling $${NAME}.ml ....."; \
		ocamlfind ocamlopt -I build/lib/ -w -1..66 -package sysyf,sysyf.ppx,csv -o build/$${NAME} -linkpkg build/lib/sysyf_learning.ml build/rnn/$${NAME}.ml; \
	 done 

	@for f in $(shell ls *.ml); do \
		NAME=$$(basename $${f} .ml); \
		echo "compiling $${NAME}.ml ....."; \
		cp $${f} build/; \
		ocamlfind ocamlopt -I build/lib/ -w -1..66 -package sysyf,sysyf.ppx -o build/$${NAME} -linkpkg build/lib/sysyf_learning.ml build/$${NAME}.ml; \
	 done 
	@echo "Finished!" 