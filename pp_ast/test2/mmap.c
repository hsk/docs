#include <sys/mman.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
    char *addr[0x10000];
    int i;
    long length = 0x800000;
    for(i = 0; i < 0x10000; i++) {
    addr[i] = mmap(NULL, length, PROT_READ |PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
//    printf("%p %p\n", MAP_FAILED, addr[i]);
    addr[i][0] = i*10;
//    printf("write ok\n");
//    printf("%d\n", addr[i][0]);
    }
printf("ok\n");
//    munmap(addr[0], length);
    return 0; 
}

