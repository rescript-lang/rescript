// memory.h
#ifndef MEMORY_H
#define MEMORY_H

#include <caml/mlvalues.h>

value init_memory(value size_val, value fileName_val);
void oskip_fresh_obstack();
void protect_memory_ro();
void protect_memory_rw();
void install_sigsegv_handler(void);
void oskip_init_interning_data();
int oskip_in_skip_heap(char* ptr);
void oskip_free_all_interned();
void* oskip_intern(void* ptr);
void oskip_ensure_interning_capacity(size_t);

#endif

