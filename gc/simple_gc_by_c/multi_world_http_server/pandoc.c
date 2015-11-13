#include "gc.h"

enum {val_START, val_SIZE, val_END};
static void view(Object** val) {
  printf("HTTP/1.0 200 OK\n");
  printf("text/html\n");
  printf("Cache-Control: max-age=0\n\n");

  printf("<html>\n");
  printf("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />");
  printf("<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n");
  printf("<body>\n");
  system("pandoc -f markdown_github README.md");

  printf("<hr/>\n");
  printf("<a href=\"javascript:history.back()\">back</a>\n");
  printf("</body>\n");
  printf("</html>\n");
}

void get_action() {
  ENTER_FRAME_ENUM(val);
  view(val);
  LEAVE_FRAME(val);
}
