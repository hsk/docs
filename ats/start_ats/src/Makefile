hello: hello.dats
	patscc -o hello hello.dats
int: int.dats
	patscc -o int int.dats
int2: int2.dats
	patscc -o int2 int2.dats
fun: fun.dats
	patscc -o fun fun.dats
fun2: fun2.dats
	patscc -o fun2 fun2.dats -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -I$(PATSHOME) -I$(PATSHOME)/ccomp/runtime
misc: misc.dats
	patscc -o misc misc.dats -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -I$(PATSHOME) -I$(PATSHOME)/ccomp/runtime

clean:
	rm -f a.out *_dats.c hello int int2 fun fun2 misc