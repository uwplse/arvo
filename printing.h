#ifndef PRINTING_H
#define PRINTING_H

#include <printf.h>

void setup_printing(void);

#ifdef __APPLE__
extern printf_domain_t domain;

#define printf(...) xprintf(domain, NULL, ##__VA_ARGS__)
#define fprintf(f, ...) fxprintf(f, domain, NULL, ##__VA_ARGS__)
#define asprintf(r, ...) asxprintf(r, domain, NULL, ##__VA_ARGS__)

#endif

#endif  // PRINTING_H
