/*****************************************************************************/
/* Persistent memory storage. */
/*****************************************************************************/

#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdint.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include "runtime.h"
#include <stdarg.h>

#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_vm.h>
#endif


#define DEFAULT_CAPACITY (1024L * 1024L * 1024L * 16L)
#ifdef __APPLE__
#define BOTTOM_ADDR ((void*)0x0000000300000000ULL)
#else
#define BOTTOM_ADDR ((void*)0x0000001000000000ULL)
#endif

static char* heap_base = NULL;

char* SKIP_get_heap_base(void) {
  return heap_base;
}
#define FTABLE_SIZE 64

/*****************************************************************************/
/* Persistent constants. */
/*****************************************************************************/

void*** pconsts = NULL;

/*****************************************************************************/
/* Database capacity. */
/*****************************************************************************/

size_t* capacity = NULL;

/*****************************************************************************/
/* Gensym. */
/*****************************************************************************/

uint64_t* gid;

uint64_t SKIP_genSym(uint64_t largerThan) {
  uint64_t n = 1;
  uint64_t gid_value = __atomic_load_n(gid, __ATOMIC_RELAXED);

  if (largerThan > 1024L * 1024L * 1024L) {
    fprintf(stderr, "ID too large: %" PRIu64 "\n", largerThan);
    exit(2);
  }

  if (largerThan > gid_value) {
    n += largerThan - gid_value;
  }

  return __atomic_fetch_add(gid, n, __ATOMIC_RELAXED);
}

/*****************************************************************************/
/* The global information structure. */
/*****************************************************************************/

typedef struct ginfo {
  void* ftable[FTABLE_SIZE];
  void* context;
  char* head;
  char* end;
  char* fileName;
  size_t total_palloc_size;
} ginfo_t;

ginfo_t* ginfo = NULL;

/*****************************************************************************/
/* Debug logging helpers. */
/*****************************************************************************/

static int sk_palloc_debug_enabled = -1;

static int sk_palloc_should_debug(void) {
  if (sk_palloc_debug_enabled == -1) {
    const char* flag = getenv("SKIP_PALLOC_DEBUG");
    sk_palloc_debug_enabled = (flag != NULL && flag[0] != '\0');
  }
  return sk_palloc_debug_enabled;
}

static void sk_palloc_log(const char* fmt, ...) {
  if (!sk_palloc_should_debug()) {
    return;
  }
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "[palloc] ");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);
}

/*****************************************************************************/
/* Alignment helpers. */
/*****************************************************************************/

static size_t sk_align_up(size_t value, size_t alignment) {
  return (value + alignment - 1) & ~(alignment - 1);
}

#ifdef __APPLE__
extern void* __dso_handle;

static uintptr_t sk_current_image_base(void) {
  return (uintptr_t)__dso_handle;
}

static void* macos_reserve_address(void* desired, size_t size) {
  mach_vm_address_t addr = (mach_vm_address_t)desired;
  kern_return_t kr = mach_vm_allocate(
      mach_task_self(), &addr, size, VM_FLAGS_FIXED | VM_FLAGS_OVERWRITE);
  if (kr != KERN_SUCCESS || addr != (mach_vm_address_t)desired) {
    sk_palloc_log(
        "macos_reserve_address failed for %p (%zu bytes): %s",
        desired,
        size,
        mach_error_string(kr));
    return NULL;
  }
  sk_palloc_log("macos_reserve_address: reserved %p (%zu bytes)", desired, size);
  return (void*)addr;
}

static void macos_release_address(void* addr, size_t size) {
  if (addr == NULL || size == 0) {
    return;
  }
  mach_vm_deallocate(mach_task_self(), (mach_vm_address_t)addr, size);
  sk_palloc_log("macos_release_address: released %p (%zu bytes)", addr, size);
}
#endif


/*****************************************************************************/
/* Global locking. */
/*****************************************************************************/

pthread_mutexattr_t* gmutex_attr;
pthread_mutex_t* gmutex = (void*)1234;

// This is only used for debugging purposes
int sk_is_locked = 0;

int sk_has_global_lock() {
  return sk_is_locked;
}

void sk_check_has_lock() {
  if ((ginfo->fileName != NULL) && !sk_is_locked) {
    fprintf(stderr, "INTERNAL ERROR: unsafe operation\n");
    SKIP_throw_cruntime(ERROR_INTERNAL_LOCK);
  }
}

void sk_global_lock_init() {
  pthread_mutexattr_init(gmutex_attr);
  pthread_mutexattr_setpshared(gmutex_attr, PTHREAD_PROCESS_SHARED);
#ifndef __APPLE__
  pthread_mutexattr_setrobust(gmutex_attr, PTHREAD_MUTEX_ROBUST);
#endif
  pthread_mutex_init(gmutex, gmutex_attr);
}

void sk_global_lock() {
  if (ginfo->fileName == NULL) {
    return;
  }

  int code = pthread_mutex_lock(gmutex);
  sk_is_locked = 1;

  if (code == 0) {
    return;
  }

#ifndef __APPLE__
  if (code == EOWNERDEAD) {
    pthread_mutex_consistent(gmutex);
    return;
  }
#endif

  perror("Internal error: locking failed");
  exit(ERROR_LOCKING);
}

void sk_global_unlock() {
  if (ginfo->fileName == NULL) {
    return;
  }

  int code = pthread_mutex_unlock(gmutex);
  sk_is_locked = 0;

  if (code == 0) {
    return;
  }

  perror("Internal error: global unlocking failed");
  exit(ERROR_LOCKING);
}

/*****************************************************************************/
/* Mutexes/Condition variables. */
/*****************************************************************************/

void SKIP_mutex_init(pthread_mutex_t* lock) {
  if (sizeof(pthread_mutex_t) > 48) {
    fprintf(stderr, "Internal error: mutex object too large for this arch\n");
  }
  pthread_mutexattr_t mutex_attr_holder;
  pthread_mutexattr_t* mutex_attr = &mutex_attr_holder;
  pthread_mutexattr_init(mutex_attr);
  pthread_mutexattr_setpshared(mutex_attr, PTHREAD_PROCESS_SHARED);
#ifndef __APPLE__
  pthread_mutexattr_setrobust(mutex_attr, PTHREAD_MUTEX_ROBUST);
#endif
  pthread_mutex_init(lock, mutex_attr);
}

void SKIP_mutex_lock(pthread_mutex_t* lock) {
  int code = pthread_mutex_lock(lock);

  if (code == 0) {
    return;
  }

#ifndef __APPLE__
  if (code == EOWNERDEAD) {
    pthread_mutex_consistent(lock);
    return;
  }
#endif

  fprintf(stderr, "Internal error: locking failed\n");
  exit(ERROR_LOCKING);
}

void SKIP_mutex_unlock(pthread_mutex_t* lock) {
  int code = pthread_mutex_unlock(lock);

  if (code == 0) {
    return;
  }

  fprintf(stderr, "Internal error: unlocking failed, %d\n", code);
  exit(ERROR_LOCKING);
}

void SKIP_cond_init(pthread_cond_t* cond) {
  if (sizeof(pthread_mutex_t) > 48) {
    fprintf(stderr, "Internal error: mutex object too large for this arch\n");
  }
  pthread_condattr_t cond_attr_value;
  pthread_condattr_t* cond_attr = &cond_attr_value;
  pthread_condattr_init(cond_attr);
  pthread_condattr_setpshared(cond_attr, PTHREAD_PROCESS_SHARED);
  pthread_cond_init(cond, cond_attr);
}

void* SKIP_freeze_lock(void* x) {
  return x;
}

void* SKIP_unfreeze_lock(void* x) {
  return x;
}

void* SKIP_freeze_cond(void* x) {
  return x;
}

void* SKIP_unfreeze_cond(void* x) {
  return x;
}

void SKIP_cond_wait(pthread_cond_t* x, pthread_mutex_t* y) {
  pthread_cond_wait(x, y);
}

int32_t SKIP_cond_timedwait(pthread_cond_t* x, pthread_mutex_t* y,
                            uint32_t secs) {
  struct timeval tv;
  struct timespec ts;
  gettimeofday(&tv, NULL);
  ts.tv_sec = tv.tv_sec + secs;
  ts.tv_nsec = 0;
  return (int32_t)pthread_cond_timedwait(x, y, &ts);
}

int32_t SKIP_cond_broadcast(void* c) {
  return (int32_t)pthread_cond_broadcast(c);
}

/*****************************************************************************/
/* Debugging support for contexts. Set CTX_TABLE to 1 to use. */
/*****************************************************************************/

#ifdef CTX_TABLE
char* ctx_table[CTX_TABLE_CAPACITY];
size_t ctx_table_size = 0;

int sk_find_ctx(char* context) {
  int i;
  for (i = 0; i < ctx_table_size; i++) {
    if (context == ctx_table[i]) {
      return i;
    }
  }
  return -1;
}

void sk_clean_ctx_table() {
  int i = 0;
  int j = 0;
  for (; j < ctx_table_size; j++) {
    int rcount = sk_get_ref_count(ctx_table[j]);
    if (rcount < 0) {
      fprintf(stderr, "Error: CTX_TABLE found negative ref count");
      exit(ERROR_CONTEXT_CHECK);
    }
    if (rcount == 0) {
      continue;
    }
    if (i != j) {
      ctx_table[i] = ctx_table[j];
    }
    i++;
  }
  ctx_table_size = i;
}

void sk_print_ctx_table() {
  int i = 0;
  fprintf(stderr, "---- CTX TABLE BEGIN -----\n");
  for (; i < ctx_table_size; i++) {
    fprintf(stderr, "%p, REF_COUNT: %lu\n", ctx_table[i],
            sk_get_ref_count(ctx_table[i]));
  }
  fprintf(stderr, "---- CTX TABLE END -------\n");
}

void sk_add_ctx(char* context) {
  int i = sk_find_ctx(context);
  if (i < 0) {
    if (ctx_table_size >= CTX_TABLE_CAPACITY) {
      fprintf(stderr, "Error: CTX_TABLE reached maximum capacity");
      exit(ERROR_CONTEXT_CHECK);
    }
    ctx_table[ctx_table_size] = context;
    ctx_table_size++;
  }
}

#endif

/*****************************************************************************/
/* Global context access primitives. */
/*****************************************************************************/

char* SKIP_context_get_unsafe() {
  char* context = ginfo->context;

  if (context != NULL) {
    sk_incr_ref_count(context);
  }

  return context;
}

uint32_t SKIP_has_context() {
  sk_global_lock();
  char* context = ginfo->context;
  uint32_t result = context != NULL;
  sk_global_unlock();
  return result;
}

SkipInt SKIP_context_ref_count() {
  char* context = ginfo->context;

  if (context == NULL) {
    return (SkipInt)0;
  } else {
    return sk_get_ref_count(context);
  }
}

char* SKIP_context_get() {
  sk_global_lock();
  char* context = SKIP_context_get_unsafe();
  sk_global_unlock();

  return context;
}

void sk_context_set_unsafe(char* obj) {
  ginfo->context = obj;
#ifdef CTX_TABLE
  sk_add_ctx(obj);
#endif
}

void sk_context_set(char* obj) {
  sk_global_lock();
  ginfo->context = obj;
  sk_global_unlock();
}

/*****************************************************************************/
/* File name parser (from the command line arguments). */
/*****************************************************************************/

static char* parse_args(int argc, char** argv, int* is_init) {
  // FIXME
  if (argc > 0 && strcmp(argv[0], "skargo") == 0) {
    return NULL;
  }

  int i;
  int idx = -1;

  for (i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--data") == 0 || strcmp(argv[i], "--init") == 0) {
      if (strcmp(argv[i], "--init") == 0) {
        *is_init = 1;
      }
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --data/--init expects a file name");
        exit(ERROR_ARG_PARSE);
      }
      if (idx != -1) {
        fprintf(stderr, "Error: incompatible --data/--init options");
        exit(ERROR_ARG_PARSE);
      }
      idx = i + 1;
    }
  }

  if (idx == -1) {
    return NULL;
  } else {
    return argv[idx];
  }
}

size_t parse_capacity(int argc, char** argv) {
  int i;

  for (i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--capacity") == 0) {
      if (i + 1 < argc) {
        if (argv[i + 1][0] >= '0' && argv[i + 1][0] <= '9') {
          int j = 0;

          while (argv[i + 1][j] != 0) {
            if (argv[i + 1][j] >= '0' && argv[i + 1][j] <= '9') {
              j++;
              continue;
            }
            fprintf(stderr, "--capacity expects an integer\n");
            exit(2);
          }
          return atol(argv[i + 1]);
        } else if (argv[i + 1][0] == '-') {
          return DEFAULT_CAPACITY;
        } else {
          fprintf(stderr, "--capacity expects an integer\n");
          exit(2);
        }
      }
    }
  }
  return DEFAULT_CAPACITY;
}

/*****************************************************************************/
/* Staging/commit. */
/*****************************************************************************/

void sk_commit(char* new_root, uint32_t sync) {
  if (ginfo->fileName == NULL) {
    sk_context_set_unsafe(new_root);
    return;
  }

  __sync_synchronize();
  if (sync) {
    msync(heap_base, *capacity, MS_SYNC);
  }
  sk_context_set_unsafe(new_root);
  if (sync) {
    msync(heap_base, *capacity, MS_SYNC);
  }
}

/*****************************************************************************/
/* Disk-persisted state, a.k.a. file mapping. */
/*****************************************************************************/

typedef struct file_mapping file_mapping_t;

typedef struct {
  int64_t version;
  file_mapping_t* bottom_addr;
#ifdef __APPLE__
  uintptr_t image_base;
#endif
} file_mapping_header_t;

struct file_mapping {
  file_mapping_header_t header;
  pthread_mutexattr_t gmutex_attr;
  pthread_mutex_t gmutex;
  ginfo_t ginfo_data;
  uint64_t gid;
  size_t capacity;
  void** pconsts;
  char persistent_fileName[1];
};

/*****************************************************************************/
/* Creates a new file mapping. */
/*****************************************************************************/

void sk_create_mapping(char* fileName, size_t icapacity, int show) {
  sk_palloc_log(
      "sk_create_mapping: file=%s size=%zu",
      (fileName != NULL) ? fileName : "(null)",
      icapacity);
  if (fileName != NULL && access(fileName, F_OK) == 0) {
    fprintf(stderr, "ERROR: File %s already exists!\n", fileName);
    exit(ERROR_MAPPING_EXISTS);
  }
  file_mapping_t* mapping;
  int prot = PROT_READ | PROT_WRITE;
#ifdef __APPLE__
  void* target = macos_reserve_address(BOTTOM_ADDR, icapacity);
  if (target == NULL) {
    fprintf(stderr, "ERROR: could not reserve persistent heap at %p\n", BOTTOM_ADDR);
    exit(ERROR_MAPPING_FAILED);
  }
#endif
  if (fileName == NULL) {
#ifdef __APPLE__
    mapping = mmap(target, icapacity, prot, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
    sk_palloc_log("sk_create_mapping: anon mmap returned %p", mapping);
#else
    mapping = mmap(NULL, icapacity, prot, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#endif
  } else {
    int fd = open(fileName, O_RDWR | O_CREAT, 0600);
    lseek(fd, icapacity, SEEK_SET);
    (void)write(fd, "", 1);
#ifdef __APPLE__
    sk_palloc_log("sk_create_mapping: macOS mmap size=%zu", icapacity);
    mapping = mmap(target, icapacity, prot, MAP_SHARED | MAP_FIXED, fd, 0);
    sk_palloc_log("sk_create_mapping: mmap returned %p", mapping);
#else
    mapping = mmap(BOTTOM_ADDR, icapacity, prot, MAP_SHARED | MAP_FIXED, fd, 0);
#endif
    close(fd);
  }

  if (mapping == MAP_FAILED) {
#ifdef __APPLE__
    macos_release_address(target, icapacity);
#endif
    perror("ERROR (MAP FAILED)");
    fprintf(
        stderr,
        "[palloc] mmap failed for file=%s size=%zu errno=%d\n",
        (fileName != NULL) ? fileName : "(null)",
        icapacity,
        errno);
    exit(ERROR_MAPPING_FAILED);
  }

  heap_base = (char*)mapping;

  mapping->header.version = SKIP_get_version();
  mapping->header.bottom_addr = mapping;
#ifdef __APPLE__
  mapping->header.image_base = sk_current_image_base();
#endif

  gmutex_attr = &mapping->gmutex_attr;
  gmutex = &mapping->gmutex;
  ginfo = &mapping->ginfo_data;
  gid = &mapping->gid;
  capacity = &mapping->capacity;
  pconsts = &mapping->pconsts;

  size_t fileName_length = (fileName != NULL) ? strlen(fileName) + 1 : 0;
  char* persistent_fileName = mapping->persistent_fileName;

  char* head = persistent_fileName + fileName_length;
  char* end = (char*)mapping + icapacity;

  if (head >= end) {
    fprintf(stderr, "Could not initialize memory\n");
    exit(ERROR_MAPPING_MEMORY);
  }

  if (fileName != NULL) {
    memcpy(persistent_fileName, fileName, fileName_length);
  } else {
    persistent_fileName = "";
  }

  int i;
  for (i = 0; i < FTABLE_SIZE; i++) {
    ginfo->ftable[i] = NULL;
  }

  ginfo->total_palloc_size = 0;

  // The head must be aligned!
  head = (char*)(((uintptr_t)head + (uintptr_t)(15)) & ~((uintptr_t)(15)));

  ginfo->head = head;
  ginfo->end = end;
  ginfo->fileName = (fileName != NULL) ? persistent_fileName : NULL;
  ginfo->context = NULL;
  *gid = 1;
  if (show && icapacity != DEFAULT_CAPACITY) {
    printf("CAPACITY SET TO: %ld\n", icapacity);
  }
  *capacity = icapacity;
  *pconsts = NULL;

  if (ginfo->fileName != NULL) {
    sk_global_lock_init();
  }
}

/*****************************************************************************/
/* Loads an existing mapping. */
/*****************************************************************************/

int sk_load_mapping(char* fileName) {
  int fd = open(fileName, O_RDWR, 0600);

  if (fd == -1) {
    fprintf(stderr, "Error: could not open file (did you run --init?)\n");
    exit(ERROR_FILE_IO);
  }

  file_mapping_header_t header;
  lseek(fd, 0L, SEEK_SET);
  int bytes = read(fd, &header, sizeof(file_mapping_header_t));

  if (bytes != sizeof(file_mapping_header_t)) {
    fprintf(stderr, "Error: could not read header\n");
    exit(ERROR_MAPPING_MEMORY);
  }

  if (header.version != SKIP_get_version()) {
    fprintf(stderr, "Error: wrong file format: %s\n", fileName);
    exit(ERROR_MAPPING_VERSION);
  }

  size_t fsize = lseek(fd, 0, SEEK_END) - 1;
  int prot = PROT_READ | PROT_WRITE;
#ifdef __APPLE__
  uintptr_t current_image_base = sk_current_image_base();
  if (header.image_base != current_image_base) {
    sk_palloc_log(
        "sk_load_mapping: image base mismatch (file=%p current=%p) for %s",
        (void*)header.image_base,
        (void*)current_image_base,
        fileName);
    close(fd);
    return 0;
  }
  void* target = header.bottom_addr;
  sk_palloc_log(
      "sk_load_mapping: reserving %p (%zu bytes) for %s",
      target,
      fsize,
      fileName);
  void* reserved = macos_reserve_address(target, fsize);
  if (reserved == NULL) {
    fprintf(stderr, "ERROR: could not reserve heap at %p (size %zu)\n", target, fsize);
    exit(ERROR_MAPPING_FAILED);
  }
  file_mapping_t* mapping =
      mmap(target, fsize, prot, MAP_SHARED | MAP_FIXED, fd, 0);
  sk_palloc_log("sk_load_mapping: mmap returned %p", mapping);
#else
  void* target = header.bottom_addr;
  file_mapping_t* mapping =
      mmap(target, fsize, prot, MAP_SHARED | MAP_FIXED, fd, 0);
#endif
  close(fd);

  if (mapping == MAP_FAILED) {
#ifdef __APPLE__
    macos_release_address(target, fsize);
#endif
    perror("ERROR (MAP FAILED)");
    exit(ERROR_MAPPING_FAILED);
  }

  heap_base = (char*)mapping;

  gmutex = &mapping->gmutex;
  ginfo = &mapping->ginfo_data;
  gid = &mapping->gid;
  capacity = &mapping->capacity;
  pconsts = &mapping->pconsts;
#ifdef __APPLE__
  sk_palloc_log("sk_load_mapping: completed for %s", fileName);
#else
  (void)fileName;
#endif
  return 1;
}

/*****************************************************************************/
/* Detects pointers that come from the binary. */
/*****************************************************************************/

int obstack_intervals_contains(void*);

int sk_is_in_heap(void* ptr) {
  return ((char*)ginfo <= (char*)ptr && (char*)ptr < ginfo->end);
}

/*****************************************************************************/
/* Free table. */
/*****************************************************************************/

typedef size_t slot_t;

#if !(defined(__has_builtin) && __has_builtin(__builtin_stdc_bit_width))
static inline size_t __builtin_stdc_bit_width(size_t size) {
  return size ? (size_t)(sizeof(size_t) * 8UL - __builtin_clzl(size)) : 0;
}
#endif

slot_t sk_slot_of_size(size_t size) {
  // Must return a value between 0 and FTABLE_SIZE - 1 included
  if (__builtin_expect(size < sizeof(void*), 0)) {
    // sk_size_of_slot requires slot to be at least log2(sizeof(void*))
    size = sizeof(void*);
  }
  return __builtin_stdc_bit_width(size - 1);
}

size_t sk_size_of_slot(slot_t slot) {
  // Must return a multiple of sizeof(void*) and at least sizeof(void*)
  // Hence slot is expected to be at least log2(sizeof(void*))
  return 1 << slot;
}

void sk_add_ftable(void* ptr, slot_t slot) {
  *(void**)ptr = ginfo->ftable[slot];
  ginfo->ftable[slot] = ptr;
}

void* sk_get_ftable(slot_t slot) {
  void** ptr = ginfo->ftable[slot];
  if (ptr == NULL) {
    return ptr;
  }
  ginfo->ftable[slot] = *ptr;
  return ptr;
}

/*****************************************************************************/
/* No file initialization (the memory is not backed by a file). */
/*****************************************************************************/

// Handy structure to allocate all those things at once
typedef struct {
  ginfo_t ginfo_data;
  uint64_t gid;
  void** pconsts;
  size_t capacity_value;
  char* heap_base;
  size_t heap_size;
} no_file_t;

int sk_is_nofile_mode() {
  return (ginfo->fileName == NULL);
}

size_t sk_current_capacity(void) {
  if (capacity == NULL) {
    return 0;
  }
  return *capacity;
}

size_t sk_current_usage(void) {
  if (ginfo == NULL) {
    return 0;
  }
  return ginfo->total_palloc_size;
}

/*****************************************************************************/
/* Memory initialization. */
/*****************************************************************************/

extern SKIP_gc_type_t* epointer_ty;

void sk_init_external_pointers() {
  char* obj = sk_get_external_pointer();
  epointer_ty = get_gc_type(obj);
}

void SKIP_memory_init(int argc, char** argv) {
  int is_create = 0;
  char* fileName = parse_args(argc, argv, &is_create);

  if (is_create || fileName == NULL) {
    size_t capacity = DEFAULT_CAPACITY;
    capacity = parse_capacity(argc, argv);
    sk_create_mapping(fileName, capacity, 1);
  } else {
    sk_load_mapping(fileName);
  }

  sk_init_external_pointers();
}

/*****************************************************************************/
/* Persistent alloc/free primitives. */
/*****************************************************************************/

void SKIP_print_persistent_size() {
  printf("%ld\n", ginfo->total_palloc_size);
}

void* sk_palloc(size_t size) {
  sk_check_has_lock();
  size_t requested = size;
  slot_t slot = sk_slot_of_size(size);
  size = sk_size_of_slot(slot);

  if (sk_palloc_should_debug()) {
    sk_palloc_log(
        "alloc request=%zu aligned=%zu slot=%zu head=%p end=%p total_before=%zu",
        requested, size, (size_t)slot, ginfo->head, ginfo->end,
        ginfo->total_palloc_size);
  }

  ginfo->total_palloc_size += size;
  sk_cell_t* ptr = sk_get_ftable(slot);
  if (ptr != NULL) {
    if (sk_palloc_should_debug()) {
      sk_palloc_log("alloc reuse slot=%zu ptr=%p total_after=%zu",
                    (size_t)slot, ptr, ginfo->total_palloc_size);
    }
    return ptr;
  }
  if (ginfo->head + size >= ginfo->end) {
    sk_palloc_log("allocation failure: request=%zu aligned=%zu head=%p end=%p "
                  "total=%zu capacity=%zu",
                  requested, size, ginfo->head, ginfo->end,
                  ginfo->total_palloc_size,
                  capacity != NULL ? *capacity : 0);
    fprintf(stderr, "Error: out of persistent memory.\n");
    exit(ERROR_OUT_OF_MEMORY);
  }
  void* result = ginfo->head;
  ginfo->head += size;
  if (sk_palloc_should_debug()) {
    sk_palloc_log("alloc new ptr=%p new_head=%p total_after=%zu", result,
                  ginfo->head, ginfo->total_palloc_size);
  }
  return result;
}

void sk_pfree_size(void* chunk, size_t size) {
  sk_check_has_lock();
  slot_t slot = sk_slot_of_size(size);
  size = sk_size_of_slot(slot);
  ginfo->total_palloc_size -= size;
  if (sk_palloc_should_debug()) {
    sk_palloc_log("pfree ptr=%p size=%zu slot=%zu total_after=%zu", chunk, size,
                  (size_t)slot, ginfo->total_palloc_size);
  }
  sk_add_ftable(chunk, slot);
}
