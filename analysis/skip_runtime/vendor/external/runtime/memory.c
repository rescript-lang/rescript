#include "runtime.h"

#ifdef SKIP64

#ifdef MEMORY_CHECK
char sk_init_over = 0;
sk_htbl_t sk_malloc_table_holder;
sk_htbl_t* sk_malloc_table = &sk_malloc_table_holder;
#endif

void* malloc(size_t);
void free(void*);
#endif

/*****************************************************************************/
/* Free table. */
/*****************************************************************************/

typedef struct sk_size_info {
  size_t main;
  size_t remainder;
} sk_size_info_t;

#ifdef SKIP32

void* sk_ftable[SK_FTABLE_SIZE][SK_FTABLE_SIZE] = {0};

#if !(defined(__has_builtin) && __has_builtin(__builtin_stdc_bit_width))
static inline size_t __builtin_stdc_bit_width(size_t size) {
  return size ? (size_t)(sizeof(size_t) * 8UL - __builtin_clzl(size)) : 0;
}
#endif

static inline size_t sk_bit_size(size_t size) {
  return __builtin_stdc_bit_width(size - 1);
}

size_t sk_pow2_size(size_t size) {
  size = (size + (sizeof(void*) - 1)) & ~(sizeof(void*) - 1);
  return (1 << sk_bit_size(size));
}

size_t sk_obj_size(size_t size, sk_size_info_t* size_info) {
  size_t pow2_size = sk_pow2_size(size);
  size_t bit_size = sk_bit_size(size);
  if (pow2_size > size && bit_size > 8) {
    size_t main_size = 1 << (bit_size - 1);
    size_t remainder_size = sk_pow2_size(size - main_size);
    if (main_size != remainder_size) {
      size_info->main = bit_size - 1;
      size_info->remainder = sk_bit_size(remainder_size);
      return main_size + remainder_size;
    }
  }
  size_info->main = bit_size;
  size_info->remainder = 0;
  return pow2_size;
}

void sk_add_ftable(void* ptr, sk_size_info_t si) {
  *(void**)ptr = sk_ftable[si.main][si.remainder];
  sk_ftable[si.main][si.remainder] = ptr;
}

void* sk_get_ftable(sk_size_info_t si) {
  void** ptr = sk_ftable[si.main][si.remainder];
  if (ptr == NULL) {
    return ptr;
  }
  sk_ftable[si.main][si.remainder] = *(void**)sk_ftable[si.main][si.remainder];
  return ptr;
}

#endif

/*****************************************************************************/
/* Common primitives . */
/*****************************************************************************/

#ifdef MEMORY_CHECK
void free_size(void* ptr, size_t size) {
  free(ptr);
}
#endif

void sk_memory_check_init() {
#ifdef MEMORY_CHECK
  sk_htbl_init(sk_malloc_table, 20);
#endif
}

void sk_memory_check_init_over() {
#ifdef MEMORY_CHECK
  sk_init_over = 1;
#endif
}

/*****************************************************************************/
/* 32bits Memory allocation. */
/*****************************************************************************/

#ifdef SKIP32
extern unsigned char __heap_base;

unsigned char* bump_pointer = &__heap_base;
unsigned long total_size = 0;
unsigned char* decr_heap_end(size_t size);

void* sk_malloc(size_t size) {
  sk_size_info_t info;
  size = sk_obj_size(size, &info);
  total_size += size;
  void* res = sk_get_ftable(info);
  if (res != NULL) {
    return res;
  }
  char* result = (char*)bump_pointer;
  bump_pointer += size;
  return result;
}

void sk_free_size(void* ptr, size_t size) {
  sk_size_info_t info;
  size = sk_obj_size(size, &info);
  total_size -= size;
  sk_add_ftable(ptr, info);
}

void* sk_palloc(size_t size) {
  return sk_malloc(size);
}

void sk_pfree_size(void* ptr, size_t size) {
  sk_free_size(ptr, size);
}

void SKIP_memory_init() {}

void SKIP_load_context() {}

int memcmp(const void* ptr1, const void* ptr2, size_t num) {
  char* str1 = (char*)ptr1;
  char* str2 = (char*)ptr2;
  char* end1 = str1 + num;
  char* end2 = str2 + num;

  while (1) {
    if (str1 == end1 && str2 == end2) {
      return 0;
    }
    if (str1 == end1) {
      return -1;
    }
    if (str2 == end2) {
      return 1;
    }
    char c1 = *str1;
    char c2 = *str2;
    int diff = c1 - c2;
    if (diff != 0) return diff;
    str1++;
    str2++;
  }
}

#endif

/*****************************************************************************/
/* 64 bits Memory allocation. */
/*****************************************************************************/

#ifdef SKIP64
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

void* sk_malloc(size_t size) {
  void* result = malloc(size);
  if (result == NULL) {
    perror("malloc");
    SKIP_throw_cruntime(ERROR_OUT_OF_MEMORY);
  }

#ifdef MEMORY_CHECK
  static size_t alloc_count = 0;
  if (sk_init_over) {
    size_t capacity = 1 << sk_malloc_table->bitcapacity;

    if (sk_malloc_table->size >= capacity / 2) {
      fprintf(stderr, "MEMORY CHECK TABLE SIZE TOO SMALL\n");
      exit(ERROR_MEMORY_CHECK);
    }

    sk_htbl_add(sk_malloc_table, result, (uint64_t)alloc_count);
    alloc_count++;
  }
#endif
  return result;
}

void sk_free_size(void* ptr, size_t /* size */) {
#ifdef MEMORY_CHECK
  sk_htbl_remove(sk_malloc_table, ptr);
#endif
  free(ptr);
}
#endif

void SKIP_check_memory() {
#ifdef MEMORY_CHECK
  size_t capacity = 1 << sk_malloc_table->bitcapacity;
  size_t i;

  for (i = 0; i < capacity; i++) {
    if (sk_malloc_table->data[i].key != 0) {
      if (sk_malloc_table->data[i].value != TOMB) {
        fprintf(stderr, "FOUND A LEAK! %p %ld\n", sk_malloc_table->data[i].key,
                (size_t)sk_malloc_table->data[i].value);
      }
      sk_malloc_table->data[i].key = 0;
    }
  }
#endif
}
