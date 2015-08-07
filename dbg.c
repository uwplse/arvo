#include "dbg.h"

static char time_buf[BUFSIZ];
char* get_time() {
  time_t t = time(NULL);
  struct tm* localtm = localtime(&t);
  strftime(time_buf, BUFSIZ, "%d %b %Y-%m-%d %H:%M:%S", localtm);
  return time_buf;
}
