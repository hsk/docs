all: vm.ml compile.ml
	ocamlc vm.ml compile.ml -o compile
	./compile

test: test.ml vm.ml
	ocamlc vm.ml test.ml -o test
	./test
	rm test
	

