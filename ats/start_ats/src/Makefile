hello: hello.dats
	patscc -o hello hello.dats
int: int.dats
	patscc -o int int.dats
int2: int2.dats
	patscc -o int2 int2.dats
if: if.dats
	patscc -o if if.dats
fn: fn.dats
	patscc -o fn fn.dats
fun: fun.dats
	patscc -o fun fun.dats
poly: poly.dats
	patscc -o poly poly.dats
let: let.dats
	patscc -o let let.dats
case: case.dats
	patscc -o case case.dats
datatype: datatype.dats
	patscc -DATS_MEMALLOC_LIBC -o datatype datatype.dats
where: where.dats
	patscc -DATS_MEMALLOC_LIBC -o where where.dats
tuple: tuple.dats
	patscc -o tuple tuple.dats -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -I$(PATSHOME) -I$(PATSHOME)/ccomp/runtime
record: record.dats
	patscc -o record record.dats -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -I$(PATSHOME) -I$(PATSHOME)/ccomp/runtime
fun2: fun2.dats
	patscc -o fun2 fun2.dats -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -I$(PATSHOME) -I$(PATSHOME)/ccomp/runtime

clean:
	rm -f a.out *_dats.c hello int int2 fn fun fun2 if poly let case datatype where tuple record
