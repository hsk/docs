calc: calc.dats
	patscc -lgc -DATS_MEMALLOC_LIBC calc.dats -o calc
calc_ref: calc_ref.dats
	patscc -DATS_MEMALLOC_LIBC calc_ref.dats -o calc_ref
calc_cpy: calc_cpy.dats
	patscc -DATS_MEMALLOC_LIBC calc_cpy.dats -o calc_cpy
clean:
	rm -rf *_dats.c calc calc_ref calc_cpy

