all:
	coqc -R abc abc abc/a.v b.v
	rm -rf b.mli
	ocamlc -o b b.ml main.ml
	./b

clean:
	rm -rf *.glob *.vo b.mli b.ml *.cm* b .*.aux
	rm -rf abc/*.glob abc/*.vo abc/.*.aux
