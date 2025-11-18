// util.h
#ifndef UTIL_H
#define UTIL_H

#include <caml/mlvalues.h>

void print_ocaml_tag_info(value v);
void* set_high_bit(void* ptr);
void* clear_high_bit(void* ptr);
int is_high_bit_set(void* ptr);

#endif

