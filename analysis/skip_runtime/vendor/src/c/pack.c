// pack.c
#include "pack.h"
#include "util.h"
#include "map.h"
#include "page_table.h"
#include "memory.h"

#include <caml/fail.h>
#include <caml/memory.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

// Data structures used (should go in a header if shared)
typedef struct {
  void* key;
  void* value;
} PointerPair;

typedef struct {
  PointerPair* data;
  size_t size;
  size_t capacity;
} PointerPairList;

typedef struct {
  value* key;
  value value;
} Assoc;

typedef struct {
  Assoc* data;
  size_t size;
  size_t capacity;
} AssocList;

static void init_pair_list(PointerPairList* list) {
  list->size = 0;
  list->capacity = 1024;
  list->data = malloc(list->capacity * sizeof(PointerPair));
}

static void push_pair(PointerPairList* list, void* key, void* val) {
  if (list->size >= list->capacity) {
    list->capacity *= 2;
    list->data = realloc(list->data, list->capacity * sizeof(PointerPair));
  }
  list->data[list->size++] = (PointerPair){ key, val };
}

static int pop_pair(PointerPairList* list, void** key, void** val) {
  if (list->size == 0) return 0;
  list->size--;
  *key = list->data[list->size].key;
  *val  = list->data[list->size].value;
  return 1;
}

static void free_pair_list(PointerPairList* list) {
  free(list->data);
  list->data = NULL;
  list->size = list->capacity = 0;
}

static void init_assoc_list(AssocList* list) {
  list->size = 0;
  list->capacity = 1024;
  list->data = malloc(list->capacity * sizeof(Assoc));
}

static void push_assoc(AssocList* list, value* key, value val) {
  if (list->size >= list->capacity) {
    list->capacity *= 2;
    list->data = realloc(list->data, list->capacity * sizeof(Assoc));
  }
  list->data[list->size++] = (Assoc){ key, val };
}

static void free_assoc_list(AssocList* list) {
  for (size_t i = 0; i < list->size; i++) {
    *list->data[i].key = list->data[i].value;
  }
  free(list->data);
  list->data = NULL;
  list->size = list->capacity = 0;
}

// Tag values used for special blocks
#define NO_SCAN_TAG 251
#define STRING_TAG 252
#define DOUBLE_TAG 253
#define DOUBLE_ARRAY_TAG 254

// Set black bit (mark as visited)
static void set_black_bit(void* v) {
  header_t *header = (header_t *)((char *)v - sizeof(header_t));
  *header = *header | (3 << 8);
}

extern char* skip_heap;
extern size_t skip_heap_size;

value pack_loop(char* (*alloc)(void*, mlsize_t), void* alloc_param, value root) {
  CAMLparam1(root);
  CAMLlocal1(src);
  if (Is_long(root)) return root;

  PointerPairList stack;
  VoidMap* map = create_map();
  init_pair_list(&stack);

  value result;
  push_pair(&stack, (void*)root, (void*)&result);

  void *src_ptr, *dest_ptr;

  while (pop_pair(&stack, &src_ptr, &dest_ptr)) {
    src = (value)src_ptr;
    value* dest = (value*)dest_ptr;

    if((char*)src_ptr > skip_heap && (char*)src_ptr < skip_heap + skip_heap_size) {
      *dest = src;
      continue;
    }

    void* stored_copy = lookup_in_virtual_page(map, src_ptr);
    if(stored_copy != NULL) {
      *dest = (value)stored_copy;
      continue;
    }

    tag_t tag = Tag_val(src);
    mlsize_t size = Wosize_val(src);

    if (tag == Abstract_tag || tag == Custom_tag) {
      free_pair_list(&stack);
      free_map(map);
      caml_failwith("Cannot pack abstract or custom block");
    }

    char* mem = alloc(alloc_param, (size + 1) * sizeof(value));
    value* block = (value*)mem;
    block[0] = ((value*)src)[-1];
    value copy = (value)(block + 1);
    *dest = copy;
    set_black_bit((void*)copy);
    write_to_virtual_page(map, src_ptr, (void*)copy);

    if (tag == String_tag || tag == Double_tag || tag == Double_array_tag) {
      memcpy(block + 1, (void*)src, size * sizeof(value));
    } else {
      if (tag != 0) {
        free_pair_list(&stack);
        free_map(map);
        fprintf(stderr, "Cannot pack object\n");
        exit(2);
      }
      value* fields = (void*)copy;
      for (mlsize_t i = 0; i < size; i++) {
        value field = Field(src, i);
        if (Is_long(field)) {
          fields[i] = field;
        } else {
          push_pair(&stack, (void*)field, (void*)&Field(copy, i));
        }
      }
    }
  }

  free_pair_list(&stack);
  free_map(map);
  CAMLreturnT(value, result);
}

typedef struct region {
  char* mem;
  mlsize_t size;
  struct region* next;
} region;

typedef struct {
  region* head;
  mlsize_t total_size;
} region_list;

typedef struct {
  char* base;
  mlsize_t offset;
  mlsize_t capacity;
} block_allocator;

char* region_alloc(void* param, mlsize_t size) {
  region_list* list = (region_list*)param;

  char* mem = malloc(size);
  if (!mem) caml_failwith("region_alloc: malloc failed");

  region* r = malloc(sizeof(region));
  if (!r) caml_failwith("region_alloc: region struct malloc failed");

  r->mem = mem;
  r->size = size;
  r->next = list->head;
  list->head = r;

  list->total_size += size;
  return mem;
}

void free_region_list(region_list* list) {
  region* r = list->head;
  while (r) {
    free(r->mem);
    region* next = r->next;
    free(r);
    r = next;
  }
  list->head = NULL;
  list->total_size = 0;
}

char* block_alloc(void* param, mlsize_t size) {
  block_allocator* alloc = (block_allocator*)param;

  if (alloc->offset + size > alloc->capacity) {
    fprintf(stderr, "block_alloc: out of memory\n");
    *(volatile char*)0 = 0;
  }

  char* result = alloc->base + alloc->offset;
  alloc->offset += size;
  return result;
}

// External from Skip runtime
extern char* SKIP_createByteArray(value);
extern char* SKIP_ocamlCreateFile(char*, value);
extern char* SKIP_intern_shared(char*);
extern void sk_global_lock();
extern void sk_global_unlock();
extern void sk_decr_ref_count(char*);

typedef struct {
  region_list regions;
  value root;
} pack_state;

void* sk_ocaml_prepare_packing(value root) {
  pack_state* state = malloc(sizeof(pack_state));
  if (!state) {
    fprintf(stderr, "Error: Failed to allocate pack_state\n");
    exit(2);
  }
  state->regions = (region_list){ NULL, 0 };
  state->root = pack_loop(region_alloc, &state->regions, root);
  return state;
}

void sk_ocaml_free_prepared_packing(void* opaque_state) {
  pack_state* state = (pack_state*)opaque_state;
  free_region_list(&state->regions);
  free(state);
}

void* sk_ocaml_finalize_packing(void* opaque_state) {
  pack_state* state = (pack_state*)opaque_state;

  block_allocator final_alloc;
  final_alloc.capacity = state->regions.total_size;
  final_alloc.offset = 0;

  final_alloc.base = oskip_intern(
    SKIP_createByteArray(final_alloc.capacity)
  );

  value ocaml_root = pack_loop(block_alloc, &final_alloc, state->root);

  char* file = SKIP_ocamlCreateFile(final_alloc.base, ocaml_root);

  sk_ocaml_free_prepared_packing(state);
  return file;
}

void* sk_ocaml_pack(value root) {
  void* state = sk_ocaml_prepare_packing(root);
  return sk_ocaml_finalize_packing(state);
}

