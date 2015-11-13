#include <stdio.h>

void get_action() {
  printf("HTTP/1.0 200 OK\n");
  printf("text/html\n");
  printf("Cache-Control: max-age=0\n\n");
  printf("<html>\n");
  printf("<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n");
  printf("<body>\n");
  printf("<h1>hello world</h1>");
  printf("hello world\n");
  printf("<hr/>\n");
  printf("<a href=\"javascript:history.back()\">back</a>\n");
  printf("</body>\n");
  printf("</html>\n");
}
