all:
	coqc -R . Top abc/a   b
	rm -rf b.mli
	ocamlc -o b b.ml main.ml
	./b

Make:
	coq_makefile abc/a.v b.v > Make
	make -f Make

clean:
	rm -rf *.glob *.vo *.v.d b.mli b.ml *.cm* b .*.aux Make
	rm -rf abc/*.glob abc/*.vo abc/*.v.d abc/.*.aux

