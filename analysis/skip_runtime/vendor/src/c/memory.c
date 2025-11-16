// memory.c
#include "memory.h"
#include "interop.h"
#include "runtime.h"
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/address_class.h>
#include <signal.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <caml/callback.h>

char* skip_heap = NULL;
size_t skip_heap_size = 0;
int64_t is_init = 1;

static struct sigaction old_segv_action;
extern void* sk_create_mapping(char*, size_t, int);
extern int sk_load_mapping(char*);
extern char* SKIP_new_Obstack();
extern void SKIP_destroy_Obstack(char*);
extern void SKIP_initializeSkip();
extern void sk_persist_consts();
extern void* sk_palloc(size_t);
extern void sk_global_lock();
extern void sk_global_unlock();
extern void sk_init_external_pointers();
extern void* SKIP_intern_shared(void* ptr);
extern void sk_skip_set_init_mode();
extern void sk_pfree_size(void*, size_t);


int oskip_in_skip_heap(char* ptr) {
  return ptr > skip_heap && ptr < skip_heap + skip_heap_size;
}

static void segv_handler(int sig, siginfo_t *si, void *context) {
  void* fault_addr = si->si_addr;
  if (skip_heap != NULL && fault_addr >= (void*)skip_heap 
      && fault_addr < (void*)(skip_heap + skip_heap_size)) {
    caml_raise(*caml_named_value("memory_write_violation"));
  }
  if (old_segv_action.sa_flags & SA_SIGINFO && old_segv_action.sa_sigaction) {
    old_segv_action.sa_sigaction(sig, si, context);
  } else {
    old_segv_action.sa_handler(sig);
  }
}

void install_sigsegv_handler() {
  struct sigaction sa = { .sa_sigaction = segv_handler, .sa_flags = SA_SIGINFO };
  sigemptyset(&sa.sa_mask);
  if (sigaction(SIGSEGV, &sa, &old_segv_action) != 0) {
    perror("sigaction");
    exit(1);
  }
}

void protect_memory_ro() {
  if (!skip_heap) {
    fprintf(stderr, "Heap not initialized");
    exit(2);
  }
  long page_size = sysconf(_SC_PAGESIZE);
  if (mprotect(skip_heap + page_size, skip_heap_size - page_size, PROT_READ) != 0) {
    fprintf(stderr, "mprotect to READONLY failed");
    exit(2);
  }
}

void protect_memory_rw(void) {
  if (!skip_heap) {
    fprintf(stderr, "Skip_Heap not initialized");
  }
  long page_size = sysconf(_SC_PAGESIZE);
  if (mprotect(skip_heap + page_size, skip_heap_size - page_size, PROT_READ | PROT_WRITE) != 0) {
    fprintf(stderr, "mprotect to READWRITE failed");
  }
}

typedef struct {
  void** data;
  size_t count;
  size_t capacity;
} InternedPtrs;

char* saved_obstack = NULL;

value init_memory(value size_val, value fileName_val) {
  CAMLparam2(size_val, fileName_val);
  pid_t pid = getpid();
  setvbuf(stdout, NULL, _IONBF, 0);

  if(saved_obstack != NULL) {
    *(volatile char*)0 = 0;
  }  

  long size = Long_val(size_val);
  const char* filename = String_val(fileName_val);
  
  long page_size = sysconf(_SC_PAGESIZE);
  size = (size + page_size - 1) & ~(page_size - 1);

  FILE *file = fopen(filename, "r");
  if (file) {
    fclose(file);
    if (sk_load_mapping((char*)filename)) {
      is_init = 0;
    } else {
      fprintf(stderr,
              "Error: persistent heap '%s' is incompatible with this binary. "
              "Please delete the .rheap file and rerun.\n",
              filename);
      exit(ERROR_MAPPING_VERSION);
    }
  }
  else {
    sk_skip_set_init_mode();
    sk_create_mapping((char*)filename, size, 0);
    // Making sure the first page does not contain ocaml values
    sk_global_lock();
    sk_palloc(page_size);
    sk_global_unlock();
  }

  sk_init_external_pointers();
  skip_heap = SKIP_get_heap_base();

  size_t capacity_bytes = sk_current_capacity();
  if (capacity_bytes == 0) {
    capacity_bytes = (size_t)size;
  }
  skip_heap_size = capacity_bytes;
#ifndef OCAML5
  caml_page_table_add(In_static_data, skip_heap, skip_heap + skip_heap_size);
#endif
  saved_obstack = SKIP_new_Obstack();
  SKIP_initializeSkip();
  sk_persist_consts();
  SKIP_destroy_Obstack(saved_obstack);
  saved_obstack = SKIP_new_Obstack();

  oskip_init_context();

  install_sigsegv_handler();

  CAMLreturn(Val_unit);
}

void oskip_fresh_obstack() {
  SKIP_destroy_Obstack(saved_obstack);
  saved_obstack = SKIP_new_Obstack();
}

/*****************************************************************************/
// Making sure we free after sync all the objects that were interned.
/*****************************************************************************/

static InternedPtrs* interned_ptrs = NULL;

void oskip_init_interning_data() {
  sk_global_lock();
  if (!interned_ptrs) {
    interned_ptrs = sk_palloc(sizeof(InternedPtrs));
    if (!interned_ptrs) {
      sk_global_unlock();
      fprintf(stderr, "Failed to allocate interned_ptrs\n");
      exit(1);
    }
    interned_ptrs->data = NULL;
    interned_ptrs->count = 0;
    interned_ptrs->capacity = 0;
  }
  sk_global_unlock();
}

// Ensure the structure exists and has space
static void ensure_capacity() {
  if (interned_ptrs->count == interned_ptrs->capacity) {
    size_t new_capacity = interned_ptrs->capacity == 0 ? 8 : interned_ptrs->capacity * 2;
    void** new_data = sk_palloc(new_capacity * sizeof(void*));
    if (!new_data) {
      sk_global_unlock();
      fprintf(stderr, "Failed to grow interned_ptrs data\n");
      exit(1);
    }

    // Copy old data if exists and free old buffer
    if (interned_ptrs->data) {
      for (size_t i = 0; i < interned_ptrs->count; i++) {
        new_data[i] = interned_ptrs->data[i];
      }
      sk_pfree_size(interned_ptrs->data, interned_ptrs->capacity * sizeof(void*));
    }

    interned_ptrs->data = new_data;
    interned_ptrs->capacity = new_capacity;
  }
}

void oskip_free_all_interned() {
  sk_global_lock();

  if (interned_ptrs) {
    for (size_t i = 0; i < interned_ptrs->count; i++) {
      sk_free_root(interned_ptrs->data[i]);
    }

    if (interned_ptrs->data) {
      sk_pfree_size(interned_ptrs->data, interned_ptrs->capacity * sizeof(void*));
    }

    sk_pfree_size(interned_ptrs, sizeof(InternedPtrs));
    interned_ptrs = NULL;
  }

  sk_global_unlock();
}

void* oskip_intern(void* ptr) {
  sk_global_lock();
  void* interned = SKIP_intern_shared(ptr);
  sk_global_unlock();
  sk_global_lock();
  ensure_capacity();
  interned_ptrs->data[interned_ptrs->count++] = interned;
  sk_global_unlock();
  return interned;
}
