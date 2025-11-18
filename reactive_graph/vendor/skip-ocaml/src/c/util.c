// util.c
#include "util.h"
#include <stdio.h>
#include <stdint.h>
#include <caml/mlvalues.h>

void print_ocaml_tag_info(value v) {
  if (Is_block(v)) {
    tag_t tag = Tag_val(v);
    printf("Block tag: %d\n", tag);
  } else {
    printf("Immediate value: %ld\n", Long_val(v));
  }
}

void* set_high_bit(void* ptr) {
  uint64_t val = (uint64_t)ptr;
  val |= ((uint64_t)1 << 63);
  return (void*)val;
}

void* clear_high_bit(void* ptr) {
  uint64_t val = (uint64_t)ptr;
  val &= ~((uint64_t)1 << 63);
  return (void*)val;
}

int is_high_bit_set(void* ptr) {
  uint64_t val = (uint64_t)ptr;
  return (val & ((uint64_t)1 << 63)) != 0;
}
