#include "printing.h"
#include <printf.h>


int print_custom(FILE *stream, const struct printf_info *info, const void *const *args) {
  (void) info;

  const void* const data = * (void**) args[0];
  int (*printer)(FILE* stream, const void* const x) = *(void**)args[1];

  return printer(stream, data);
}

int print_custom_arginfo(const struct printf_info *info, size_t n, int *argtypes) {
  (void) info;

  if (n > 0) {
    argtypes[0] = PA_POINTER;
    if (n > 1)
      argtypes[1] = PA_POINTER;
  }
  return 2;
}

#ifdef __APPLE__

printf_domain_t domain;

void setup_printing() {
  domain = new_printf_domain();
  register_printf_domain_function(domain, 'W', print_custom, print_custom_arginfo, NULL);
}

#else


void setup_printing() {
  register_printf_function ('W', print_custom, print_custom_arginfo);
}

#endif
