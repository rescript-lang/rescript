/*
 * This API is designed for efficiently storing metadata associated with OCaml heap values.
 *
 * OCaml uses densely packed memory layouts with many pointers stored close together
 * in memory. To optimize for this pattern, we divide memory into user-defined virtual
 * pages of 4096 bytes and use a map to associate each page with an array of 512 pointers
 * (one per 8-byte-aligned word).
 *
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "map.h"

// Page size and word size definitions
#define PAGE_SIZE 4096
#define WORD_SIZE 8
#define WORDS_PER_PAGE (PAGE_SIZE / WORD_SIZE)

// Report an error and abort the program
static void report_error_and_abort(const char* msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  abort();
}

// Align down to the start of the page
static void* page_base(void* ptr) {
  uintptr_t addr = (uintptr_t)ptr;
  return (void*)(addr & ~(PAGE_SIZE - 1));
}

// Get word offset in the page (should be between 0 and 511)
static size_t page_word_offset(void* ptr) {
  uintptr_t addr = (uintptr_t)ptr;
  return (addr & (PAGE_SIZE - 1)) >> 3;
}

// The main API function
void write_to_virtual_page(VoidMap* map, void* ptr, void* value) {
  if (((uintptr_t)ptr % WORD_SIZE) != 0) {
    report_error_and_abort("Unaligned pointer! Must be 8-byte aligned.");
  }

  void* base = page_base(ptr);
  void** page = (void**)map_lookup(map, base);

  if (!page) {
    page = (void**)calloc(WORDS_PER_PAGE, sizeof(void*));
    if (!page) {
      report_error_and_abort("Memory allocation failed.");
    }
    map_add(map, base, page);
  }

  size_t offset = page_word_offset(ptr);
  page[offset] = value;
}

void* lookup_in_virtual_page(VoidMap* map, void* ptr) {
  if (((uintptr_t)ptr % WORD_SIZE) != 0) {
    report_error_and_abort("Unaligned pointer! Must be 8-byte aligned.");
  }

  void* base = page_base(ptr);
  void** page = (void**)map_lookup(map, base);

  if (!page) {
    return NULL;
  }

  size_t offset = page_word_offset(ptr);
  return page[offset];
}
