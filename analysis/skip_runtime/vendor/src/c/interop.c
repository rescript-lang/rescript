// interop.c
#include "interop.h"
#include "map.h"
#include "pack.h"
#include "memory.h"

#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/backtrace.h>
#include <caml/mlvalues.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/time.h>

typedef struct {
  char* dirName;
  char* key;
  void* result;
  void* reads;
} delayedCall_t;

extern char* sk_string_create(const char*, uint32_t);
extern void SKIP_createInputOCamlCollection(void*, char*, void*, void*);
extern void* SKIP_ocamlCreateStringArray(int64_t);
extern int64_t SKIP_ocamlSizeStringArray(void*);
extern void SKIP_ocamlSetStringArray(void*, int64_t, void*);
extern char* SKIP_ocamlCreateIntArray(int64_t);
extern void SKIP_ocamlSetIntArray(char*, int64_t, int64_t);
extern char* SKIP_ocamlCreateContext();
extern char* SKIP_ocamlCreateDirName(char*);
extern void SKIP_ocamlCreateInputDir(char*, char*, char*, char*);
extern void SKIP_ocamlWrite(char*, char*, char*);
extern char* SKIP_ocamlUnsafeGetArray(char*, char*, char*);
extern char* SKIP_ocamlGetArray(char*, char*, char*);
extern int64_t SKIP_ocamlArrayFileSize(char*);
extern char* SKIP_ocamlCreateCollect();
extern void SKIP_ocamlCollect(char*, char*, char*, char*);
extern int64_t SKIP_ocamlDelayedMap(char*, char*, char*);
extern int64_t SKIP_ocamlMap(char*, char*, char*, char*);
extern size_t sk_getDelayedCallsCount();
extern delayedCall_t* sk_getDelayedCall(size_t);
extern void sk_removeDelayedCall(size_t);
extern size_t sk_put_first(char*);
extern void sk_global_lock();
extern void sk_global_unlock();
extern void sk_pfree_size(void*, size_t);
extern void sk_freeDelayedCalls();
extern delayedCall_t* sk_getDelayedCalls();
extern value SKIP_ocamlGetRoot(char*);
extern char* SKIP_ocamlCreateArrayKeyFiles(value);
extern char* SKIP_ocamlCreateArrayFile(value);
extern void SKIP_ocamlSetArrayFile(char*, value, char*);
extern char* SKIP_ocamlCreateKeyFiles(char*, char*);
extern void SKIP_ocamlSetArrayKeyFiles(char*, value, char*);
extern void sk_savedDelayedCallsSortedSize(size_t size);
extern void SKIP_ocamlDebugArrayKeyFiles(char*);
extern void SKIP_ocamlDebugKeyFiles(char*);
extern void SKIP_ocamlDebugFiles(char*);
extern void SKIP_ocamlDebugFile(char*);
extern void sk_decr_ref_count(char*);
extern void sk_incr_ref_count(char*);
extern value SKIP_ocamlArrayKeyFilesSize(char*);
extern char* SKIP_ocamlArrayKeyFilesGet(char*, value);
extern char* SKIP_ocamlKeyFilesGetKey(char*);
extern char* SKIP_ocamlKeyFilesGetFiles(char*);
extern void sk_free_root(char*);
extern char* SKIP_ocamlContextGetReads(char*);
extern void sk_finishDelayedCall(size_t);
extern void caml_print_exception_backtrace(FILE *f);
extern void SKIP_context_init(char*);
extern char* SKIP_ocamlMCloneContext(char*);
extern char* SKIP_ocamlCloneContext(char*);
extern void SKIP_ocamlReplaceContext(char*, char*);
extern char* SKIP_ocamlSync(char*, char*);
extern void SKIP_ocamlDirty(char*, char*, char*);
extern void SKIP_ocamlUpdate(char*, char*);
extern char* SKIP_ocamlGetDummyFile();
extern void sk_free_size(char*, size_t);
extern char* SKIP_context_get();
extern int64_t SKIP_ocamlUnion(char*, char*, char*, char*);
extern void SKIP_ocamlAddDirty(char*, char*, char*, char*);
extern void sk_free_delayedCalls();
extern int64_t SKIP_ocamlExistsInputDir(char*, char*);
extern char* SKIP_intern_shared(char*);
extern void sk_reset_delayedCalls();
extern void SKIP_ocamlFreeRootContext(char*, char*);
extern void SKIP_ocamlClearDirty(char*, char*, char*, char*, char*);

extern int64_t sk_is_save_reads_mode();
extern void sk_save_reads_mode_on();
extern void sk_save_reads_mode_off();
extern value caml_format_exception(value);

extern int64_t is_init;

VoidMap* dirNames = NULL;
char* rootContext = NULL;
char* context = NULL;

void oskip_init_context() {
  if (context != NULL) {
    fprintf(stderr, "SKIP error: initialization called twice\n");
    _exit(2);
  }
  rootContext = SKIP_ocamlCreateContext();
  context = SKIP_ocamlMCloneContext(rootContext);
  dirNames = create_map();
}

// Helper function to convert an OCaml string to a sk_string
static char* create_sk_string_from_ocaml(value ocaml_str) {
  if (Tag_val(ocaml_str) != String_tag) {
    caml_invalid_argument("expected a string");
  }

  mlsize_t len = caml_string_length(ocaml_str);
  const char* str = String_val(ocaml_str);

  return sk_string_create(str, len);
}

static char* create_dirName(value dirName) {
  CAMLparam1(dirName);
/*  char* result = map_lookup(dirNames, (void*)dirName);
  if (result != NULL) {
    return result;
  }
*/

  char* skip_str = create_sk_string_from_ocaml(dirName);
  char* result = SKIP_ocamlCreateDirName(skip_str);
//  map_add(dirNames, (void*)dirName, result);

  CAMLreturnT(char*, result);
}

CAMLprim value oskip_exists_input_dir(value dirName_val) {
  CAMLparam1(dirName_val);

  char* dirName = create_dirName(dirName_val);

  CAMLreturn(Val_int(SKIP_ocamlExistsInputDir(context, dirName)));
}

CAMLprim value oskip_create_input_dir(value dirName_val, value filenames) {
  CAMLparam2(dirName_val, filenames);

  char* dirName = create_dirName(dirName_val);
  mlsize_t len = Wosize_val(filenames);
  void* sk_names = SKIP_ocamlCreateStringArray(len);
  void* sk_array = SKIP_ocamlCreateIntArray(len);

  for (mlsize_t i = 0; i < len; ++i) {
    value filename = Field(filenames, i);
    const char* c_filename = String_val(filename);
    char* sk_filename = create_sk_string_from_ocaml(filename);
    SKIP_ocamlSetStringArray(sk_names, i, sk_filename);

    struct stat st;
    if (stat(c_filename, &st) == 0) {
      SKIP_ocamlSetIntArray(sk_array, i, (int64_t)st.st_mtime);
    } else {
      SKIP_ocamlSetIntArray(sk_array, i, 0);
    }
  }

  SKIP_ocamlCreateInputDir(context, dirName, sk_names, sk_array);

  CAMLreturn(Val_unit);
}

CAMLprim value oskip_write(value dirName_val, value key_val) {
  CAMLparam2(dirName_val, key_val);

  char* dirName = create_dirName(dirName_val);
  char* key = create_sk_string_from_ocaml(key_val);

  SKIP_ocamlWrite(
    context,
    dirName,
    key
  );

  CAMLreturn(Val_unit);
}

value create_ocaml_array(void** data, int64_t count) {
  CAMLparam0();
  CAMLlocal1(array);

  array = caml_alloc(count, 0);
  for (int64_t i = 0; i < count; ++i) {
    value root = SKIP_ocamlGetRoot(data[i]);
    if (!Is_long(root) && !oskip_in_skip_heap((char*)root)) {
      fprintf(stderr, "Internal error: ocaml value not in Skip heap\n");
      _exit(2);
    }
    Store_field(array, i, root);
  }

  CAMLreturn(array);
}

CAMLprim value oskip_unsafe_get_array(value dirName_val, value key) {
  CAMLparam2(dirName_val, key);
  CAMLlocal2(array, val);

  char* dirName = create_dirName(dirName_val);
  char* skkey = create_sk_string_from_ocaml(key);
  char* skarray = SKIP_ocamlUnsafeGetArray(context, dirName, skkey);
  int64_t count = SKIP_ocamlArrayFileSize(skarray);
  array = create_ocaml_array((void**)skarray, count);

  CAMLreturn(array);
}

CAMLprim value oskip_get_array(value dirName_val, value key) {
  CAMLparam2(dirName_val, key);
  CAMLlocal2(array, val);

  char* dirName = create_dirName(dirName_val);
  char* skkey = create_sk_string_from_ocaml(key);
  char* skarray = SKIP_ocamlGetArray(context, dirName, skkey);
  int64_t count = SKIP_ocamlArrayFileSize(skarray);
  array = create_ocaml_array((void**)skarray, count);

  CAMLreturn(array);
}


static size_t* delayedCallsIndex = NULL;
static size_t preparedSize = 0;

typedef struct {
  size_t index;
  void* result;
  void* reads;
} locallyPrepared_t;
static __thread locallyPrepared_t* locallyPrepared = NULL;
static __thread size_t locallyPreparedSize;

CAMLprim value oskip_at_exit(value dummy) {
  CAMLparam1(dummy);
//  sk_freeDelayedCalls();
  CAMLreturn(Val_unit);
}

value oskip_setup_local(value param) {
  CAMLparam1(param);

  locallyPrepared = malloc(sizeof(locallyPrepared_t) * preparedSize);
  locallyPreparedSize = 0;

  CAMLreturn(Val_unit);
}

CAMLprim value oskip_prepare_map(
  value parentDirName_val,
  value targetDirName_val
) {
  CAMLparam2(parentDirName_val, targetDirName_val);
  char* parentDirName = create_dirName(parentDirName_val);
  char* targetDirName = create_dirName(targetDirName_val);
  sk_save_reads_mode_off();
  oskip_init_interning_data();

  int64_t existed = SKIP_ocamlDelayedMap(context, parentDirName, targetDirName);

  if (existed) {
    const char* targetDirNameStr = String_val(targetDirName_val);
    size_t targetDirNameStrSize = strlen(targetDirNameStr);
    char* sk_str = sk_string_create(targetDirNameStr, targetDirNameStrSize);
    SKIP_ocamlUpdate(context, sk_str);
  }

  preparedSize = sk_getDelayedCallsCount();

  sk_global_lock();
  if(delayedCallsIndex == NULL) {
    delayedCallsIndex = mmap(
      NULL,
      sizeof(size_t),
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_ANONYMOUS,
      -1,
      0
    );
  }
  *delayedCallsIndex = 0;
  sk_global_unlock();

  if (preparedSize == 0) CAMLreturn(Val_int(0));

  long cpu_count = sysconf(_SC_NPROCESSORS_ONLN);
  if (cpu_count < 1) cpu_count = 1;

  int num_processes = (int)
    ((preparedSize < (size_t)cpu_count) ? preparedSize : cpu_count);

  CAMLreturn(Val_int(num_processes));
}

/*****************************************************************************/
/* We remember all the places where we prepacked, to finalize the packing
 * later.
 */
/*****************************************************************************/
typedef struct {
    char* skip_array;
    size_t index;
    char* prepared;
} PackingStackElement;

// Global packing stack
static __thread PackingStackElement* packingStack = NULL;
static __thread size_t packingStack_size = 0;
static __thread size_t packingStack_capacity = 0;

void pushPacking(char* skip_array, size_t index, char* prepared) {
  if (packingStack_size == packingStack_capacity) {
    // Grow the packing stack
    size_t new_capacity =
      packingStack_capacity == 0 ? 8 : packingStack_capacity * 2;

    PackingStackElement* new_stack =
      malloc(new_capacity * sizeof(PackingStackElement));

    if (!new_stack) {
      fprintf(stderr, "Failed to allocate memory for packingStack\n");
      _exit(1);
    }
    for (size_t i = 0; i < packingStack_size; i++) {
      new_stack[i] = packingStack[i];
    }
    free(packingStack);
    packingStack = new_stack;
    packingStack_capacity = new_capacity;
  }

  packingStack[packingStack_size++] =
    (PackingStackElement){ skip_array, index, prepared };
}

void finalizePacking() {
  size_t i;
  for (i = 0; i < packingStack_size; i++) {
    PackingStackElement elt = packingStack[i];
    char* file = sk_ocaml_finalize_packing(elt.prepared);
    SKIP_ocamlSetArrayFile(elt.skip_array, elt.index, file);
  }
  packingStack_size = 0;
}

/*****************************************************************************/

char* convert_ocaml_array_to_skip(value ocaml_array) {
  CAMLparam1(ocaml_array);
  CAMLlocal3(pair, ocaml_key, ocaml_files);

  mlsize_t outer_len = Wosize_val(ocaml_array);
  char* skip_keyfiles_array = SKIP_ocamlCreateArrayKeyFiles(outer_len);

  for (mlsize_t i = 0; i < outer_len; ++i) {
    pair = Field(ocaml_array, i);
    ocaml_key = Field(pair, 0);
    ocaml_files = Field(pair, 1);

    tag_t tag = Tag_val(ocaml_files);

    if (tag == Double_array_tag) {
      fprintf(stderr, "Error: cannot use floats as collection elements\n");
      fprintf(stderr, "You must wrap them\n");
      _exit(2);
    }

    char* skip_key = create_sk_string_from_ocaml(ocaml_key);

    mlsize_t file_count = Wosize_val(ocaml_files);
    char* skip_file_array = SKIP_ocamlCreateArrayFile(file_count);
    for (mlsize_t j = 0; j < file_count; ++j) {
      char* skip_file = sk_ocaml_prepare_packing(Field(ocaml_files, j));
      pushPacking(skip_file_array, j, skip_file);
      char* dummy_file = SKIP_ocamlGetDummyFile();
      SKIP_ocamlSetArrayFile(skip_file_array, j, dummy_file);
    }

    char* skip_keyfiles = SKIP_ocamlCreateKeyFiles(skip_key, skip_file_array);

    SKIP_ocamlSetArrayKeyFiles(skip_keyfiles_array, i, skip_keyfiles);
  }


  CAMLreturnT(char*, skip_keyfiles_array);
}

CAMLprim value oskip_process_element(value closure) {
  CAMLparam1(closure);
  CAMLlocal2(key, result);

  sk_global_lock();
  size_t index = *delayedCallsIndex;
  *delayedCallsIndex = index + 1;
  sk_global_unlock();

  if(index >= preparedSize) {
    protect_memory_rw();

    finalizePacking();
    delayedCall_t* delayedCalls = sk_getDelayedCalls();

    size_t i;
    for(i = 0; i < locallyPreparedSize; i++) {
      delayedCall_t* dc = &delayedCalls[locallyPrepared[i].index];
      dc->result = oskip_intern(locallyPrepared[i].result);
      dc->reads = oskip_intern(locallyPrepared[i].reads);
    }

    CAMLreturn(Val_int(0));
  }

  char* savedContext = context;
  context = SKIP_ocamlMCloneContext(context);
  delayedCall_t* dc = sk_getDelayedCall(index);
  key = caml_copy_string(dc->key);
  result = caml_callback_exn(closure, key);

  if (Is_exception_result(result)) {
    fprintf(stderr, "Uncaught OCaml exception in a map!\n");
    /* Print the exception value */
    value exn = Extract_exception(result);
    value exn_str = caml_format_exception(exn);
    fprintf(stderr, "Exception: %s\n", String_val(exn_str));
    caml_print_exception_backtrace(stderr);
    _exit(2);
  }

  char* skip_result = convert_ocaml_array_to_skip(result);
  locallyPrepared[locallyPreparedSize].index = index;
  locallyPrepared[locallyPreparedSize].result = skip_result;
  locallyPrepared[locallyPreparedSize].reads =
    SKIP_ocamlContextGetReads(context);
  locallyPreparedSize++;

  context = savedContext;

  CAMLreturn(Val_int(1));
}

static int compare_by_key(const void* a, const void* b) {
  const delayedCall_t* da = (const delayedCall_t*)a;
  const delayedCall_t* db = (const delayedCall_t*)b;
  return strcmp(da->key, db->key);
}

void sort_delayed_calls_by_key(delayedCall_t* array, size_t size) {
  qsort(array, size, sizeof(delayedCall_t), compare_by_key);
}

long long current_time_ms() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (long long)tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

CAMLprim value oskip_map(
  value parentDirName_val,
  value preparedDirName_val,
  value targetDirName_val,
  value dedupDirName_val
) {
  CAMLparam4(parentDirName_val, preparedDirName_val, targetDirName_val, dedupDirName_val);

  char* parentDirName = create_dirName(parentDirName_val);
  char* preparedDirName = create_dirName(preparedDirName_val);
  char* targetDirName = create_dirName(targetDirName_val);
  char* dedupDirName = create_dirName(dedupDirName_val);
  size_t i;

  delayedCall_t* delayedCalls = sk_getDelayedCalls();
  sort_delayed_calls_by_key(delayedCalls, preparedSize);
  sk_savedDelayedCallsSortedSize(preparedSize);

  if (is_init) {
    for (i = 0; i < preparedSize; i++) {
      SKIP_ocamlAddDirty(context, parentDirName, preparedDirName, delayedCalls[i].key);
    }
  }

  sk_save_reads_mode_on();
  const char* preparedDirNameStr = String_val(preparedDirName_val);
  size_t preparedDirNameStrSize = strlen(preparedDirNameStr);
  char* sk_str = sk_string_create(preparedDirNameStr, preparedDirNameStrSize);
  SKIP_ocamlUpdate(context, sk_str);

  int64_t existed = SKIP_ocamlMap(context, preparedDirName, targetDirName, dedupDirName);

  if (existed) {
    const char* targetDirNameStr = String_val(targetDirName_val);
    size_t targetDirNameStrSize = strlen(targetDirNameStr);
    char* target = sk_string_create(targetDirNameStr, targetDirNameStrSize);
    SKIP_ocamlUpdate(context, target);

    const char* dedupDirNameStr = String_val(dedupDirName_val);
    size_t dedupDirNameStrSize = strlen(dedupDirNameStr);
    char* dedup = sk_string_create(dedupDirNameStr, dedupDirNameStrSize);
    SKIP_ocamlUpdate(context, dedup);
  }

  sk_reset_delayedCalls();
  rootContext = SKIP_ocamlSync(rootContext, context);
  oskip_fresh_obstack();
  context = SKIP_ocamlMCloneContext(rootContext);
  oskip_free_all_interned();

  CAMLreturn(Val_unit);
}

CAMLprim value oskip_exit(value dumb) {
  CAMLparam1(dumb);

  SKIP_ocamlFreeRootContext(rootContext, context);

  sk_global_lock();
  sk_free_delayedCalls();
  sk_global_unlock();

  CAMLreturn(Val_unit);
}

CAMLprim value oskip_union(value col1_val, value col2_val, value col3_val) {
  CAMLparam3(col1_val, col2_val, col3_val);

  char* col1 = create_dirName(col1_val);
  char* col2 = create_dirName(col2_val);
  char* col3 = create_dirName(col3_val);
  
  int64_t existed = SKIP_ocamlUnion(context, col1, col2, col3);

  if (existed) {
    const char* unionDirNameStr = String_val(col3_val);
    size_t unionDirNameStrSize = strlen(unionDirNameStr);
    char* union_str = sk_string_create(unionDirNameStr, unionDirNameStrSize);
    SKIP_ocamlUpdate(context, union_str);
  }
  
  CAMLreturn(Val_unit);
}

extern int64_t SKIP_ocamlSetFile(char*, char*, char*, int64_t);
extern void SKIP_ocamlRemoveFile(char*, char*, char*);
extern char** SKIP_ocamlGetFiles(char*, char*);
extern int64_t SKIP_ocamlArrayStringSize(char**);

CAMLprim value oskip_set_file(value dirName_val, value filename) {
  CAMLparam2(dirName_val, filename);

  char* dirName = create_dirName(dirName_val);

  struct stat st;
  if (stat(String_val(filename), &st) != 0) {
    CAMLreturn(Val_int(1));
  }

  char* sk_filename = create_sk_string_from_ocaml(filename);
  SKIP_ocamlSetFile(context, dirName, sk_filename, st.st_mtime);

  CAMLreturn(Val_unit);
}

CAMLprim value oskip_remove_file(value dirName_val, value filename) {
  CAMLparam2(dirName_val, filename);

  char* dirName = create_dirName(dirName_val);

  SKIP_ocamlRemoveFile(context, dirName, create_sk_string_from_ocaml(filename));

  CAMLreturn(Val_unit);
}

static value c_strings_to_ocaml_array(char** c_strings, int size) {
  CAMLparam0();
  CAMLlocal2(arr, str);

  arr = caml_alloc(size, 0);

  for (int i = 0; i < size; i++) {
    str = caml_copy_string(c_strings[i]);
    Store_field(arr, i, str);
  }

  CAMLreturn(arr);
}

CAMLprim value oskip_get_files(value dirName_val) {
  CAMLparam1(dirName_val);

  char* dirName = create_dirName(dirName_val);
  char** files = SKIP_ocamlGetFiles(context, dirName);
  size_t size = SKIP_ocamlArrayStringSize(files);

  CAMLreturn(c_strings_to_ocaml_array(files, size));
}
