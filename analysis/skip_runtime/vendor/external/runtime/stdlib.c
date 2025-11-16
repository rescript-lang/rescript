#include "runtime.h"

#ifdef SKIP64

#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

void* SKIP_exec(char* cmd) {
  sk_string_check_c_safe(cmd);

  FILE* fstream = popen(cmd, "w");

  if (fstream == NULL) {
    perror("Could not execute command");
    exit(ERROR_EXEC);
  }

  return fstream;
}

uint32_t SKIP_write_to_proc(FILE* f, char* str) {
  size_t size = SKIP_String_byteSize(str);
  size_t n = fwrite(str, 1, size, f);
  if (n != size) {
    return 1;
  }
  return 0;
}

uint32_t SKIP_wait_for_proc(FILE* f) {
  return pclose(f);
}

#endif

#ifdef SKIP32

void* memset(void* orig_ptr, int value, size_t size) {
  char* ptr = (char*)orig_ptr;

  if (value != 0) {
    // memset only implemented for zero
    SKIP_throw_cruntime(ERROR_NOT_IMPLEMENTED);
  }

  const char* end = ptr + size;
  const char* lend = ptr + (size / sizeof(long) * sizeof(long));

  while (ptr < lend) {
    *(long*)ptr = 0;
    ptr += sizeof(long);
  }

  while (ptr < end) {
    *ptr = 0;
    ptr++;
  }

  return orig_ptr;
}

void* memcpy(void* orig_dest, const void* orig_src, size_t size) {
  char* dest = (char*)orig_dest;
  const char* src = (const char*)orig_src;
  const char* end = src + size;
  const char* lend = src + (size / sizeof(long) * sizeof(long));

  while (src < lend) {
    *(long*)dest = *(long*)src;
    dest += sizeof(long);
    src += sizeof(long);
  }

  while (src < end) {
    *dest = *src;
    dest++;
    src++;
  }
  return orig_dest;
}

#endif

void sk_print_int(uint64_t x) {
  char str[256];
  SkipInt i = 255;
  if (x == 0) {
    SKIP_print_char('0');
    SKIP_print_char('\n');
    return;
  }
  while (x != 0) {
    if (x % 10 == 0) str[i] = '0';
    if (x % 10 == 1) str[i] = '1';
    if (x % 10 == 2) str[i] = '2';
    if (x % 10 == 3) str[i] = '3';
    if (x % 10 == 4) str[i] = '4';
    if (x % 10 == 5) str[i] = '5';
    if (x % 10 == 6) str[i] = '6';
    if (x % 10 == 7) str[i] = '7';
    if (x % 10 == 8) str[i] = '8';
    if (x % 10 == 9) str[i] = '9';
    i--;
    x = x / 10;
  }
  for (i++; i < 256; i++) {
    SKIP_print_char(str[i]);
  }
  SKIP_print_char('\n');
}

#ifdef SKIP64
void todo(char* err, char* msg) {
  fprintf(stderr, "%s: %s\n", err, msg);
  SKIP_throw_cruntime(ERROR_TODO);
}
#else
void todo(char*, char*) {
  SKIP_throw_cruntime(ERROR_TODO);
}
#endif

char* SKIP_read_line() {
  int32_t size = SKIP_read_line_fill();

  if (size < 0) {
    return NULL;
  }

  char* result = SKIP_Obstack_alloc(size);

  for (int32_t i = 0; i < size; i++) {
    result[i] = SKIP_read_line_get(i);
  }

  return sk_string_create(result, size);
}

char* SKIP_read_to_end() {
  int32_t size = SKIP_read_to_end_fill();

  char* result = SKIP_Obstack_alloc(size);

  for (int32_t i = 0; i < size; i++) {
    result[i] = SKIP_read_line_get(i);
  }

  return sk_string_create(result, size);
}
