#include <sys/fcntl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <signal.h>

#define HTTP_TCP_PORT 8088

void http(int sockfd);
int send_msg(int fd, char *msg);

int sockfd, new_sockfd;

static int sigsegvSignalHandler(int sig){
  printf("close\n");
  close(sockfd);
  return 0;
}

void initSignalHandler(){
    signal(SIGINT, &sigsegvSignalHandler);
    signal(SIGSEGV, &sigsegvSignalHandler);
}

int main() {
  int writer_len;
  struct sockaddr_in reader_addr, writer_addr;
  bzero((char *) &reader_addr, sizeof(reader_addr));
  reader_addr.sin_family = AF_INET;
  reader_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  reader_addr.sin_port = htons(HTTP_TCP_PORT);

  if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    fprintf(stderr, "error: socket()\n");
    exit(1);
  }

  int val = 1;
  setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val));

  struct linger ling;
  ling.l_onoff = 1;
  ling.l_linger = 0;
  printf("set %d\n", setsockopt(sockfd, SOL_SOCKET, SO_LINGER, &ling, sizeof(ling)));

  initSignalHandler();
  if (bind(sockfd, (struct sockaddr *)&reader_addr, sizeof(reader_addr)) < 0) {
    fprintf(stderr, "error: bind()\n");
    close(sockfd);
    exit(1);
  }

  if (listen(sockfd, 5) < 0) {
    fprintf(stderr, "error: listen()\n");
    close(sockfd);
    exit(1);
  }

  while(1) {
    if ((new_sockfd = accept(sockfd,(struct sockaddr *)&writer_addr, &writer_len)) < 0) {
      fprintf(stderr, "error: accepting a socket.\n");
      break;
    } else {
      fprintf(stderr, "accept\n");
      http(new_sockfd);
      close(new_sockfd);
    }
  }

  printf("close %d\n", sockfd);
  close(sockfd);
  exit(0);
}

#include <dlfcn.h>

int invoke_method(char *lib, char *method) {
  void *dl_handle;
  void (*func)();
  char *error;

  dl_handle = dlopen(lib, RTLD_LAZY);
  if (!dl_handle) {
    return 1;
  }

  func = dlsym(dl_handle, method);
  error = dlerror();
  if (error != NULL) {
    printf("HTTP/1.0 404 Not Found\n\n404 not found.\n");
    printf("dlsym error !!! %s\n", error);
    return 2;
  }

  printf("HTTP/1.0 200 OK\n");
  printf("text/html\n");
  printf("\n");
  (*func)();

  dlclose(dl_handle);
  return 0;
}

#include "gc.h"
#include <sys/stat.h>


void http(int sockfd) {

  int len;
  FILE* fp;
  char buf[1024];
  char meth_name[16];
  char uri_addr[256];
  char uri_lib[512];
  char http_ver[64];  char *uri_file;

  int stdoutno = fileno(stdout);
  int back_stdoutno = dup(stdoutno);
  dup2(sockfd, stdoutno);

  if (read(sockfd, buf, 1024) <= 0) {
    fprintf(stderr, "error: reading a request.\n");
    goto ret;
  }
  sscanf(buf, "%s %s %s", meth_name, uri_addr, http_ver);
  if (strcmp(meth_name, "GET") != 0) {
    printf("error2\n");
    printf("HTTP/1.0 501 Not Implemented");
    goto ret;
  }
  uri_file = uri_addr+1;


  if ((fp = fopen(uri_file, "r")) != NULL) {
    printf("HTTP/1.0 200 OK\n");
    printf("text/html\n");
    printf("\n");
    while((len = fread(buf, 1, 1024, fp)) > 0) {
      fwrite(buf, len, 1, stderr);
      if (fwrite(buf, 1, len, stdout) != len) {
        fprintf(stderr, "error: writing a response.\n");
        break;
      }
    }
    fclose(fp);
    goto ret;
  }


  sprintf(uri_lib, "%s.c", uri_file);

  struct stat fic;
  struct stat fiso;

  stat(uri_lib , &fic);
  sprintf(uri_lib, "%s.so", uri_file);
  stat(uri_lib , &fiso);

  if(fiso.st_mtime < fic.st_mtime) {
    fprintf(stderr, "changing\n");
    sprintf(uri_lib, "gcc -shared -fPIC -o %s.so %s.c gc.so", uri_file, uri_file);
    fprintf(stderr, "%s\n",uri_lib);
    system(uri_lib);
    sprintf(uri_lib, "%s.so", uri_file);
  }

  gc_init();
  if(invoke_method(uri_lib, "get_action") == 1) {
    sprintf(uri_lib, "gcc -shared -fPIC -o %s.so %s.c gc.so", uri_file, uri_file);
    system(uri_lib);
    sprintf(uri_lib, "%s.so", uri_file);

    if(invoke_method(uri_lib, "get_action") == 1) {
      printf("HTTP/1.0 404 Not Found\n\n404 not found.\n");
      printf("dlopen error !!! %s\n", dlerror());
    }
  }
  gc_free();

ret:
  fflush(stdout);
  dup2(back_stdoutno, stdoutno);
  close(back_stdoutno);

}
