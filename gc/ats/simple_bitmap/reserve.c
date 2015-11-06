// test reserve and commit
#ifdef MINGW32
#define GetPageSize()  (64 * 1024)
#define ReservePageError  NULL
#define ReservePage(addr, size)	\
	VirtualAlloc(addr, size, MEM_RESERVE, PAGE_NOACCESS)
#define ReleasePage(addr, size) \
	VirtualFree(addr, size, MEM_RELEASE)
#define CommitPage(addr, size) \
	VirtualAlloc(addr, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
#define UncommitPage(addr, size) \
	VirtualFree(addr, size, MEM_DECOMMIT)
#else
#include <sys/mman.h>
#define GetPageSize()  getpagesize()
#define ReservePageError  ((void*)-1)
#define ReservePage(addr, size) \
	mmap(addr, size, PROT_NONE, MAP_ANON | MAP_PRIVATE, -1, 0)
#define ReleasePage(addr, size) \
	munmap(addr, size)
#define CommitPage(addr, size) \
	mprotect(addr, size, PROT_READ | PROT_WRITE)
#define UncommitPage(addr, size) \
	mmap(addr, size, PROT_NONE, MAP_ANON | MAP_PRIVATE | MAP_FIXED, -1, 0)
#endif /* MINGW32 */


#define ALIGNSIZE(x,y)  (((x) + (y) - 1) - ((x) + (y) - 1) % (y))
#include <stdio.h>

int main() {

	char *p = (char*)ReservePage(NULL, 1024*1024*32);// 32Mbyte

	//p[0]=1; // bus error
	CommitPage(p, 4096);
	p[0]=1; // ok
//	p[4096]=1; // bus error
	CommitPage(&p[4096], 4096);
	p[4096]=1; // ok


	for(int i = 0; i < 20; i++) {
		printf("%d %d\n", i, ALIGNSIZE(i, 8));
	}

	return 0;
}
