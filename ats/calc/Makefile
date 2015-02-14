calc: calc.dats
	patscc -lgc -DATS_MEMALLOC_GCBDW calc.dats -o calc.js
calc_nogc: calc.dats
	patscc -DATS_MEMALLOC_LIBC calc.dats -o calc_nogc
calc_ref: calc_ref.dats
	patscc -DATS_MEMALLOC_LIBC calc_ref.dats -o calc_ref
calc_cpy: calc_cpy.dats
	patscc -DATS_MEMALLOC_LIBC calc_cpy.dats -o calc_cpy
calc_cpyrs: calc_cpyrs.rs
	rustc calc_cpyrs.rs
clean:
	rm -rf *_dats.c calc calc_nogc calc_ref calc_cpy calc_cpyrs

