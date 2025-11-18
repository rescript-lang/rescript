#include "runtime.h"

#ifdef SKIP64
#include <unistd.h>
#endif

/*****************************************************************************/
/* Operations on the runtime representation of skip values. */
/*****************************************************************************/

sk_string_t* get_sk_string(char* obj) {
  return container_of(obj, sk_string_t, data);
}

SKIP_gc_type_t* get_gc_type(char* skip_object) {
  void** vtable_ptr = container_of(skip_object, sk_class_inst_t, data)->vtable;
  // the gc_type of each object is stored in slot 1 of the vtable,
  // see createVTableBuilders in vtable.sk
  SKIP_gc_type_t** slot1 = (SKIP_gc_type_t**)(vtable_ptr + 1);
  return *slot1;
}

/*****************************************************************************/
/* Primitives that are not used in embedded mode. */
/*****************************************************************************/

void SKIP_Regex_initialize() {}

void SKIP_print_stack_trace() {
  todo("Not implemented", "SKIP_print_stack_trace");
}
void SKIP_print_last_exception_stack_trace_and_exit(void*) {
  todo("Not implemented", "SKIP_print_last_exception_stack_trace_and_exit");
}
void SKIP_unreachableMethodCall(void* msg, void*) {
  todo("Unreachable method call", msg);
}
void SKIP_unreachableWithExplanation(void* explanation) {
  todo("Unreachable", explanation);
}

void SKIP_Obstack_vectorUnsafeSet(char** arr, char* x) {
  *arr = x;
}

void SKIP_Obstack_collect(char*, char**, SkipInt) {}

void* SKIP_llvm_memcpy(char* dest, char* val, SkipInt len) {
  return memcpy(dest, val, (size_t)len);
}

/*****************************************************************************/
/* Global context synchronization. */
/*****************************************************************************/

void SKIP_context_init(char* obj) {
  sk_global_lock();
  char* context = SKIP_intern_shared(obj);
  sk_context_set_unsafe(context);
  sk_global_unlock();
}

void SKIP_unsafe_context_incr_ref_count(char* obj) {
  sk_incr_ref_count(obj);
}

void SKIP_unsafe_free(char* context) {
  sk_global_lock();
  sk_free_root(context);
  sk_global_unlock();
}

void SKIP_global_lock() {
#ifdef SKIP64
  sk_global_lock();
#endif
}

#ifdef SKIP64
extern int sk_is_locked;
#endif

uint32_t SKIP_global_has_lock() {
#ifdef SKIP64
  return (uint32_t)sk_is_locked;
#endif
#ifdef SKIP32
  return 1;
#endif
}

void SKIP_global_unlock() {
#ifdef SKIP64
  sk_global_unlock();
#endif
}

void* SKIP_context_sync_no_lock(uint64_t txTime, char* old_root, char* delta,
                                char* synchronizer, uint32_t sync,
                                char* lockF) {
  char* root = SKIP_context_get_unsafe();
  if (root == NULL) {
#ifdef SKIP64
    fprintf(stderr, "Internal error: you forgot to initialize the context");
#endif
    SKIP_throw_cruntime(ERROR_CONTEXT_NOT_INITIALIZED);
  }
  if (root == delta || old_root == delta) {
// INVALID use of sync, the root should be different
#ifdef SKIP64
    fprintf(stderr, "Internal error: tried to sync with the same context");
#endif
    SKIP_throw_cruntime(ERROR_SYNC_SAME_CONTEXT);
  }
  char* rtmp = SKIP_resolve_context(txTime, root, delta, synchronizer, lockF);
  char* new_root = SKIP_intern_shared(rtmp);
  sk_commit(new_root, sync);
  sk_free_root(old_root);
  sk_free_root(root);
  sk_free_root(root);
  sk_free_external_pointers();
#ifdef CTX_TABLE
  sk_print_ctx_table();
#endif
  sk_incr_ref_count(new_root);
  return new_root;
}

void* SKIP_context_sync(uint64_t txTime, char* old_root, char* delta,
                        char* synchronizer, uint32_t sync, char* lockF) {
  sk_global_lock();
  char* new_root = SKIP_context_sync_no_lock(txTime, old_root, delta,
                                             synchronizer, sync, lockF);
  sk_global_unlock();
  SKIP_call_after_unlock(synchronizer, delta);
  return new_root;
}

int64_t SKIP_Unsafe_Ptr__toInt(char* ptr) {
  return (int64_t)ptr;
}

void* SKIP_Unsafe_array_ptr(char* arr, SkipInt byte_offset) {
  return arr + byte_offset;
}

int64_t SKIP_Unsafe_array_byte_size(char* arr) {
  SKIP_gc_type_t* ty = get_gc_type(arr);
  size_t len = skip_array_len(arr);

  return ty->m_userByteSize * len;
}

uint8_t SKIP_Unsafe_array_get_byte(uint8_t* arr, SkipInt index) {
  return arr[index];
}

void SKIP_Unsafe_array_set_byte(uint8_t* arr, SkipInt index, uint8_t value) {
  arr[index] = value;
}

/*****************************************************************************/
// Section added for OCaml support
/*****************************************************************************/

// Define delayedCall as a pointer type
typedef struct {
  char* dirName;
  char* key;
  void* result;
  void* reads;
} delayedCall_t;

// Dynamic array for delayed calls
static delayedCall_t* delayedCalls = NULL;
static size_t delayedCallsCount = 0;
static size_t delayedCallsSortedSize = 0;
static size_t delayedCallsCapacity = 0;

#define INITIAL_CAPACITY 32

void sk_free_delayedCalls() {
  if (delayedCalls != NULL) {
    sk_pfree_size(delayedCalls, delayedCallsCapacity * sizeof(delayedCall_t));
  }
}

void sk_reset_delayedCalls() {
  delayedCallsCount = 0;
}

// Ensure capacity for at least `minCapacity` elements
static void sk_ensureCapacity(size_t minCapacity) {

  if (delayedCallsCapacity >= minCapacity) {
    return;
  }
  size_t newCapacity = 0;
  if (delayedCallsCapacity == 0) {
    newCapacity = INITIAL_CAPACITY;
  } else {
    newCapacity = delayedCallsCapacity;
  }

  while (newCapacity < minCapacity) {
    newCapacity = newCapacity * 2;
  }

  sk_global_lock();
  delayedCall_t* newArray = sk_palloc(newCapacity * sizeof(delayedCall_t));
  if (delayedCalls != NULL) {
    memcpy(newArray, delayedCalls, delayedCallsCount * sizeof(delayedCall_t));
    sk_free_delayedCalls();
  }
  sk_global_unlock();

  delayedCalls = newArray;
  delayedCallsCapacity = newCapacity;
}

// Add an element to delayedCalls
void sk_addDelayedCall(delayedCall_t call) {
  sk_ensureCapacity(delayedCallsCount + 1);
  delayedCalls[delayedCallsCount] = call;
  delayedCallsCount++;
}

size_t sk_getDelayedCallsCount() {
  return delayedCallsCount;
}

delayedCall_t* sk_getDelayedCall(size_t idx) {
  return &delayedCalls[idx];
}

static int strcmp(const char *s1, const char *s2) {
  while (*s1 && (*s1 == *s2)) {
    s1++;
    s2++;
  }
  return *(unsigned char *)s1 - *(unsigned char *)s2;
}

size_t sk_put_first(char* dirName) {
  size_t insertPos = 0;
  for (size_t i = 0; i < delayedCallsCount; i++) {
    if (strcmp(delayedCalls[i].dirName, dirName) == 0) {
      if (i != insertPos) {
        // Swap current with the one at insertPos
        delayedCall_t temp = delayedCalls[i];
        delayedCalls[i] = delayedCalls[insertPos];
        delayedCalls[insertPos] = temp;
      }
      insertPos++;
    }
  }
  return insertPos;  // Number of matches moved to the beginning
}

delayedCall_t* sk_getDelayedCalls() {
  return delayedCalls;
}

extern int64_t SKIP_ocamlArrayFileSize(void*);

void SKIP_saveDelayedCall(char* dirName, char* key) {
  delayedCall_t call;

  call.dirName = dirName;
  call.key = key;
  call.result = NULL;

  sk_addDelayedCall(call);
}

void sk_savedDelayedCallsSortedSize(size_t size) {
  delayedCallsSortedSize = size;
}

static long binarySearchDelayedCalls(
  size_t size,
  const char* key
) {
  size_t left = 0;
  size_t right = size;

  while (left < right) {
    size_t mid = left + (right - left) / 2;
    int cmp = strcmp(delayedCalls[mid].key, key);
    if (cmp == 0) {
      return (long)mid;
    } else if (cmp < 0) {
      left = mid + 1;
    } else {
      right = mid;
    }
  }

  return -1;  // Not found
}

SkipInt SKIP_searchDelayedCall(char*, char* key) {
  return binarySearchDelayedCalls(delayedCallsSortedSize, key);
}

void* SKIP_getDelayedCall(char* dirName, char* key) {
  long index = binarySearchDelayedCalls(delayedCallsSortedSize, key);
  if (index == -1) {
    fprintf(stderr, "Internal error: delayed call not found\n");
    fprintf(stderr, "For key: %s %s\n", dirName, key);
    fprintf(stderr, "Sorted size: %ld\n", delayedCallsSortedSize);
    SKIP_throw(NULL);
  }
  if (strcmp(delayedCalls[index].dirName, dirName) != 0) {
    fprintf(stderr, "Internal error: delayed call does not match\n");
    fprintf(stderr, "Found %s %s\n", delayedCalls[index].dirName, dirName);
    SKIP_throw(NULL);
  }
  return delayedCalls[index].result;
}

void* SKIP_getDelayedReads(char* dirName, char* key) {
  long index = binarySearchDelayedCalls(delayedCallsSortedSize, key);
  if (index == -1) {
    fprintf(stderr, "Internal error: delayed read not found\n");
    SKIP_throw(NULL);
  }
  if (strcmp(delayedCalls[index].dirName, dirName) != 0) {
    fprintf(stderr, "Internal error: delayed read does not match\n");
    SKIP_throw(NULL);
  }
  return delayedCalls[index].reads;
}

void* SKIP_ocamlUnsafeCastFiles(void* obj) {
  return obj;
}

static SkipInt save_reads_mode = 0;

SkipInt sk_is_save_reads_mode() {
  return save_reads_mode;
}

void sk_save_reads_mode_on() {
  save_reads_mode = 1;
}

void sk_save_reads_mode_off() {
  save_reads_mode = 0;
}

static SkipInt is_skip_init = 0;

void sk_skip_set_init_mode() {
  is_skip_init = 1;
}

SkipInt SKIP_isSkipInit() {
  return is_skip_init;
}
