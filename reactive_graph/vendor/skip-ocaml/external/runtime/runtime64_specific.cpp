#ifndef SKIP64
#error "This file requires -DSKIP64"
#endif

extern "C" {
#include "runtime.h"
}
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#ifdef __APPLE__
#include <sys/syslimits.h>
#else
#include <linux/limits.h>
#endif
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <exception>
#include <iostream>
#include <map>
#include <random>
#include <string>
#include <vector>

extern "C" {
#include "xoroshiro128plus.h"
}

typedef struct {
  char* head;
  char* page;
  char* end;
} sk_saved_obstack_t;

extern "C" {
void* sk_get_exception_type(void* skExn);
void* sk_get_exception_message(void* skExn);
sk_saved_obstack_t* SKIP_new_Obstack();
void SKIP_destroy_Obstack(sk_saved_obstack_t* saved);
}

#ifndef RELEASE
#include <backtrace.h>

namespace {

struct backtrace_data {
  bool full_trace;
  bool before_main;
  bool before_throw;
};

static int print_callback(void* data, uintptr_t /* pc */, const char* filename,
                          int lineno, const char* function) {
  auto ctx = static_cast<backtrace_data*>(data);

  if (ctx->full_trace || (ctx->before_main && !ctx->before_throw)) {
    fprintf(stderr, "File %s, line %d\n\t%s()\n",
            filename == NULL ? "???" : filename, lineno,
            function == NULL ? "???" : function);
  }

  if (function != NULL && 0 == strcmp(function, "SKIP_throw")) {
    ctx->before_throw = false;
  } else if (function != NULL && 0 == strcmp(function, "skip_main")) {
    ctx->before_main = false;
  }

  return 0;
}

static void error_callback(void* /* data */, const char* msg, int errnum) {
  fprintf(stderr, "libbacktrace: %s", msg);
  if (errnum > 0) fprintf(stderr, ": %s", strerror(errnum));
  fputc('\n', stderr);
}
}  // namespace

#endif  // RELEASE

namespace skip {
struct SkipException : std::exception {
  explicit SkipException(void* exc) : m_skipException(exc) {}

  virtual const char* what() const noexcept override {
    char* str = (char*)sk_get_exception_message(m_skipException);
    sk_string_check_c_safe(str);
    return str;
  }

  const char* name() const noexcept {
    char* str = (char*)sk_get_exception_type(m_skipException);
    sk_string_check_c_safe(str);
    return str;
  }

  void* m_skipException;
};

#ifndef RELEASE

void printStackTrace() {
  struct backtrace_state* state =
      backtrace_create_state(nullptr, 0, error_callback, nullptr);
  if (state == nullptr) {
    fprintf(stderr, "libbacktrace: Failed to initialize backtrace\n");
    return;
  }

  backtrace_data data{false, true, true};
  backtrace_full(state, 3, print_callback, error_callback, &data);
}

#endif
}  // namespace skip

#ifndef RELEASE
namespace {
void terminate() {
  // TODO: Only print backtrace in debug mode.
  try {
    std::exception_ptr eptr = std::current_exception();
    if (eptr) {
      std::rethrow_exception(eptr);
    }
  } catch (skip::SkipException& e) {
    std::cerr << "*** Uncaught exception of type " << e.name() << ": "
              << e.what() << std::endl;
  } catch (std::exception& e) {
    std::cerr << "*** Uncaught exception: " << e.what() << std::endl;
  }
  std::cerr << "*** Stack trace:" << std::endl;
  skip::printStackTrace();
}
}  // namespace
#else
namespace {
void terminate() {
  try {
    std::exception_ptr eptr = std::current_exception();
    if (eptr) {
      std::rethrow_exception(eptr);
    }
  } catch (skip::SkipException& e) {
    std::cerr << "*** Uncaught exception of type " << e.name() << ": "
              << e.what() << std::endl;
  } catch (std::exception& e) {
    std::cerr << "*** Uncaught exception: " << e.what() << std::endl;
  }
}
}  // namespace
#endif

extern "C" {

void SKIP_call0(void*);
void SKIP_initializeSkip();
void skip_main();
void SKIP_throw_EndOfFile();

void sk_string_check_c_safe(char* str) {
  size_t size = SKIP_String_byteSize(str);
  size_t len = strlen(str);
  if (len < size) {
    fprintf(stderr,
            "String contains embedded nul character. addr: %p size: %zu "
            "length: %zu bytes: ",
            str, size, len);
    fwrite(str, size, 1, stderr);
    fprintf(stderr, "\n");
    abort();
  } else if (len > size) {
    fprintf(stderr,
            "String not nul-terminated. addr: %p size: %zu length: %zu bytes: ",
            str, size, len);
    fwrite(str, size, 1, stderr);
    fprintf(stderr, "\n");
    abort();
  }
}

void SKIP_print_char(uint32_t x) {
  printf("%c", x);
}

void SKIP_throw(void* exc) {
  throw skip::SkipException(exc);
}

thread_local std::string lineBuffer;

int32_t SKIP_read_line_fill(void) {
  std::getline(std::cin, lineBuffer);
  if (std::cin.fail()) {
    return -1;
  }
  return lineBuffer.size();
}

int32_t SKIP_read_to_end_fill() {
  std::istreambuf_iterator<char> begin(std::cin), end;
  lineBuffer = std::string(begin, end);
  return lineBuffer.size();
}

uint32_t SKIP_read_line_get(uint32_t i) {
  return lineBuffer[i];
}

thread_local void* exn;

void* SKIP_getExn() {
  return exn;
}

void SKIP_saveExn(void* e) {
  exn = e;
}

void SKIP_replaceExn(void* e) {
  exn = e;
}

static int argc = 0;
static char** argv = NULL;
extern char** environ;

int64_t SKIP_getArgc() {
  return argc;
}

char* SKIP_getArgN(int64_t n) {
  return sk_string_create(argv[n], strlen(argv[n]));
}

int64_t SKIP_get_envc() {
  char** p = nullptr;
  for (p = environ; *p != nullptr; ++p)
    ;
  return (int)(p - environ);
}

char* SKIP_get_envN(int64_t n) {
  return sk_string_create(environ[n], strlen(environ[n]));
}

char* sk_create_none_string_option();
char* sk_create_string_option(char* str);

char* SKIP_getenv(char* name) {
  sk_string_check_c_safe(name);
  char* value = getenv(name);
  if (value == nullptr) {
    return sk_create_none_string_option();
  }
  return sk_create_string_option(sk_string_create(value, strlen(value)));
}

void SKIP_setenv(char* name, char* value) {
  sk_string_check_c_safe(name);
  sk_string_check_c_safe(value);
  int rv = setenv(name, value, 1);
  if (rv != 0) {
    perror("setenv");
    exit(EXIT_FAILURE);
  }
}

void SKIP_unsetenv(char* name) {
  sk_string_check_c_safe(name);
  int rv = unsetenv(name);
  if (rv != 0) {
    perror("unsetenv");
    exit(EXIT_FAILURE);
  }
}

void SKIP_memory_init(int pargc, char** pargv);
void sk_persist_consts();

void sk_init(int pargc, char** pargv) {
  sk_saved_obstack_t* saved;
  argc = pargc;
  argv = pargv;
  SKIP_memory_init(pargc, pargv);
  saved = SKIP_new_Obstack();
  SKIP_initializeSkip();
  sk_persist_consts();
  SKIP_destroy_Obstack(saved);
}

#if defined(SKIP_LIBRARY)
__attribute__((constructor)) static void lib_init() {
  argc = 1;
  char* argv0 = strdup("library");
  argv = (char**)malloc(2 * sizeof(char*));
  argv[0] = argv0;
  argv[1] = NULL;
  sk_init(argc, argv);
}
#elif !defined(SKIP_NO_MAIN)
int main(int pargc, char** pargv) {
  std::set_terminate(terminate);
  // TODO: Make memory initialization read state.db path from the environment
  // rather than command line arguments, and let the above constructor handle
  // it.
  sk_init(pargc, pargv);
  skip_main();
}
#endif  // SKIP_LIBRARY / SKIP_NO_MAIN

static void print(FILE* descr, char* str) {
  size_t size = SKIP_String_byteSize((char*)str);
  fwrite(str, size, 1, descr);
}

void SKIP_print_raw(char* str) {
  print(stdout, str);
}

void SKIP_print_error_raw(char* str) {
  print(stderr, str);
}

void SKIP_print_debug_raw(char* str) {
  SKIP_print_error_raw(str);
}

void SKIP_flush_stdout() {
  fflush(stdout);
  fflush(stderr);
}

void print_string(char* str) {
  print(stdout, str);
  printf("\n");
}

void SKIP_print_error(char* str) {
  print(stderr, str);
  fprintf(stderr, "\n");
}

void SKIP_print_debug(char* str) {
  SKIP_print_error(str);
}

char* SKIP_open_file(char* filename) {
  struct stat s;
  sk_string_check_c_safe(filename);

  int fd = open(filename, O_RDONLY);
  if (fd == -1) {
    perror("ERROR (open failed)");
    fprintf(stderr, "Could not open file: %s\n", filename);
    exit(ERROR_FILE_IO);
  }
  int status = fstat(fd, &s);
  if (status == -1) {
    perror("ERROR (fstat failed)");
    fprintf(stderr, "Could not open file: %s\n", filename);
    exit(ERROR_FILE_IO);
  }
  size_t size = s.st_size;

  char* result;
  if (size == 0) {
    result = sk_string_create("", 0);
  } else {
    void* f = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (f == MAP_FAILED) {
      perror("ERROR (MAP FAILED)");
      fprintf(stderr, "Could not open file: %s\n", filename);
      exit(ERROR_FILE_IO);
    }
    result = sk_string_create((char*)f, size);
    munmap(f, size);
  }
  close(fd);
  return result;
}

void SKIP_write_to_file(int64_t fd, char* str) {
  size_t size = SKIP_String_byteSize(str);
  while (size > 0) {
    ssize_t written = write(fd, str, size);
    if (written < 0) {
      int err = errno;
      fprintf(stderr, "Could not write to file. %" PRId64 " (%d)\n", fd, err);
      exit(ERROR_FILE_IO);
    }
    size -= written;
  }
}

void SKIP_FileSystem_appendTextFile(char* filename, char* str_obj) {
  sk_string_check_c_safe(filename);

  int fd = open(filename, O_WRONLY | O_CREAT | O_APPEND, 0775);
  (void)write(fd, str_obj, SKIP_String_byteSize(str_obj));
  close(fd);
}

bool SKIP_check_if_file_exists(char* filename) {
  sk_string_check_c_safe(filename);

  bool res = (access(filename, F_OK) == 0);

  return res;
}

int32_t SKIP_notify(char* filename, int32_t tick) {
  sk_string_check_c_safe(filename);

  int fd = open(filename, O_CREAT | O_WRONLY, 0644);

  if (fd == -1) {
    return -1;
  }

  char buf_data[256];
  char* buf = buf_data;
  snprintf(buf, 256, "%d\n", tick);
  size_t size = strlen(buf);

  while (size > 0) {
    ssize_t written = write(fd, buf, size);
    if (written == -1) {
      close(fd);
      return -1;
    }
    buf += written;
    size -= written;
  }

  if (close(fd) == -1) {
    return -1;
  }

  return 0;
}

void SKIP_random_init() {
  std::random_device rd;
  std::uniform_int_distribution<uint64_t> dist(0, UINT64_MAX);
  uint64_t seed = dist(rd);
  xoroshiro128plus_init(seed);
}

uint64_t SKIP_random_next() {
  return xoroshiro128plus_next();
}

int64_t SKIP_time() {
  time_t res = time(nullptr);
  if (res == -1) {
    perror("time");
    exit(EXIT_FAILURE);
  }
  return (int64_t)res;
}

uint64_t SKIP_time_ms() {
  using namespace std::chrono;

  return duration_cast<milliseconds>(system_clock::now().time_since_epoch())
      .count();
}

char* SKIP_unix_strftime(char* formatp, char* timep) {
  struct tm* tm = (struct tm*)timep;
  char buffer[1024];
  char cformat[1024];
  uint32_t byteSize = SKIP_String_byteSize(formatp);
  if (byteSize >= 1024) {
    fprintf(stderr, "format string too large");
    exit(ERROR_INVALID_STRING);
  }
  cformat[byteSize] = 0;
  memcpy(cformat, formatp, byteSize);
  size_t size = strftime(buffer, 1024, cformat, tm);
  return sk_string_create(buffer, size);
}

char* SKIP_strftime(char* formatp, int64_t timestamp) {
  struct tm tm;
  localtime_r((time_t*)&timestamp, &tm);
  return SKIP_unix_strftime(formatp, (char*)&tm);
}

char* SKIP_getcwd() {
  char path[PATH_MAX];
  char* res = getcwd(path, PATH_MAX);
  if (res == NULL) {
    perror("getcwd");
    exit(EXIT_FAILURE);
  }
  return sk_string_create(path, strlen(path));
}

void SKIP_chdir(char* path) {
  sk_string_check_c_safe(path);
  int rv = chdir(path);
  if (rv != 0) {
    perror("chdir");
    exit(EXIT_FAILURE);
  }
}

int64_t SKIP_numThreads() {
  return 1;
}

void SKIP_string_to_file(char* str, char* file) {
  sk_string_check_c_safe(file);

  FILE* out = fopen(file, "w");
  size_t size = SKIP_String_byteSize(str);
  while (size != 0) {
    size_t written = fwrite(str, 1, size, out);
    size -= written;
  }
  fclose(out);
}

int64_t SKIP_get_mtime(char* path) {
  struct stat st;
  if (stat(path, &st) < 0) {
    return 0;
  }
  return st.st_mtime;
}

bool SKIP_is_directory(char* path) {
  struct stat st;
  if (stat(path, &st) < 0) {
    return 0;
  }
  return st.st_mode & S_IFDIR;
}

int64_t SKIP_system(char* cmd) {
  sk_string_check_c_safe(cmd);
  int64_t res = system(cmd);
  return res;
}

int64_t SKIP_opendir(char* path) {
  sk_string_check_c_safe(path);

  DIR* res = opendir(path);
  if (res == NULL) {
    perror("Error opening dir");
  }

  return (int64_t)res;
}

char* SKIP_readdir(int64_t dir_handle) {
  struct dirent* dp = readdir((DIR*)dir_handle);
  if (dp == NULL) {
    return sk_string_create("", 0);
  }
  size_t len = strlen(dp->d_name);
  return sk_string_create(dp->d_name, len);
}

void SKIP_closedir(int64_t dir_handle) {
  int rv = closedir((DIR*)dir_handle);

  if (rv != 0) {
    perror("closedir");
  }
}

char* SKIP_realpath(char* path) {
  sk_string_check_c_safe(path);

  char res[PATH_MAX];
  char* rv = realpath(path, res);
  if (rv == NULL) {
    perror("realpath");
    // TODO: Ideally, this function would return a ?String instead.
    return sk_string_create("", 0);
  }

  return sk_string_create(res, strlen(res));
}

__attribute__((noreturn)) void SKIP_exit(uint64_t code) {
  exit(code);
}

char* SKIP_call_external_fun(int32_t, char*) {
  fprintf(stderr, "SKIP_call_external_fun not implemented in native mode");
  exit(2);
}

/*****************************************************************************/
/* Primitives used to output js objects directly, doesn't do anything
 * in native mode.
 */
/*****************************************************************************/

void SKIP_last_tick(uint32_t) {
  // Not implemented
}
void SKIP_switch_to(uint32_t) {
  // Not implemented
}
void SKIP_clear_field_names() {
  // Not implemented
}
void SKIP_push_field_name(char*) {
  // Not implemented
}
void SKIP_clear_object() {
  // Not implemented
}
void SKIP_push_object_field_null() {
  // Not implemented
}
void SKIP_push_object_field_int32(int32_t) {
  // Not implemented
}
void SKIP_push_object_field_int64(char*) {
  // Not implemented
}
void SKIP_push_object_field_float(char*) {
  // Not implemented
}
void SKIP_push_object_field_string(char*) {
  // Not implemented
}
void SKIP_push_object_field_json(void*) {
  // Not implemented
}
void SKIP_push_object() {
  // Not implemented
}
void SKIP_js_delete_fun() {
  // Not implemented
}
}


/*****************************************************************************/
// OCAML comparison
/*****************************************************************************/

// --- Minimal OCaml layout ---
typedef uintptr_t value;
typedef uint64_t header_t;
typedef uint8_t tag_t;
typedef uint32_t mlsize_t;

#define Is_long(x)   (((value)(x) & 1) != 0)
#define Wosize_hd(hd) ((mlsize_t)((hd) >> 10))
#define Tag_hd(hd)    ((tag_t)((hd) & 0xFF))
#define Hd_val(v)    (((header_t*)v)[-1])
#define Wosize_val(v) Wosize_hd(Hd_val(v))
#define Tag_val(v)    Tag_hd(Hd_val(v))
#define Field(v, i)   (((value*)(v))[i])

#define String_tag 252
#define Double_tag 253
#define Double_array_tag 254
#define Abstract_tag 251
#define Custom_tag 250

// --- Pair stack ---
typedef struct Pair {
  void* a;
  void* b;
} Pair;

typedef struct PairList {
  Pair* data;
  size_t size;
  size_t capacity;
} PairList;

void init_pair_list(PairList* list) {
  list->size = 0;
  list->capacity = 16;
  list->data = (Pair*)malloc(sizeof(Pair) * list->capacity);
}

void push_pair(PairList* list, void* a, void* b) {
  if (list->size == list->capacity) {
    list->capacity *= 2;
    list->data = (Pair*)realloc(list->data, sizeof(Pair) * list->capacity);
  }
  list->data[list->size++] = (Pair){a, b};
}

int pop_pair(PairList* list, void** a, void** b) {
  if (list->size == 0) return 0;
  Pair p = list->data[--list->size];
  *a = p.a;
  *b = p.b;
  return 1;
}

void free_pair_list(PairList* list) {
  free(list->data);
}

// --- Visited map using unordered_map ---
#include <unordered_map>

struct AddrPair {
  void* a;
  void* b;

  bool operator==(const AddrPair& other) const {
    return a == other.a && b == other.b;
  }
};

namespace std {
template<>
struct hash<AddrPair> {
  std::size_t operator()(const AddrPair& k) const {
    return std::hash<void*>()(k.a) ^ std::hash<void*>()(k.b);
  }
};
}

typedef std::unordered_map<AddrPair, bool> VisitedMap;

int ocaml_structural_equal(value a, value b) {
  if (a == b) {
    return 1;
  }

  if (Is_long(a) || Is_long(b)) return 0;

  PairList stack;
  init_pair_list(&stack);
  VisitedMap visited;

  push_pair(&stack, (void*)a, (void*)b);

  void *pa, *pb;
  while (pop_pair(&stack, &pa, &pb)) {
    value va = (value)pa;
    value vb = (value)pb;

    if (va == vb) continue;
    if (Is_long(va) || Is_long(vb)) {
      free_pair_list(&stack);
      return 0;
    }

    AddrPair key = {pa, pb};
    if (visited.find(key) != visited.end()) continue;
    visited[key] = true;

    tag_t taga = Tag_val(va);
    tag_t tagb = Tag_val(vb);
    if (taga != tagb) {
      free_pair_list(&stack);
      return 0;
    }

    mlsize_t sizea = Wosize_val(va);
    mlsize_t sizeb = Wosize_val(vb);
    if (sizea != sizeb) {
      free_pair_list(&stack);
      return 0;
    }

    if (taga == String_tag) {
      char* sa = (char*)va;
      char* sb = (char*)vb;
      if (memcmp(sa, sb, sizea * sizeof(value)) != 0) {
        free_pair_list(&stack);
        return 0;
      }
    } else if (taga == Double_tag) {
      double* da = (double*)va;
      double* db = (double*)vb;
      if (*da != *db) {
        free_pair_list(&stack);
        return 0;
      }
    } else if (taga == Double_array_tag) {
      double* da = (double*)va;
      double* db = (double*)vb;
      for (mlsize_t i = 0; i < sizea; i++) {
        if (da[i] != db[i]) {
          free_pair_list(&stack);
          return 0;
        }
      }
    } else if (taga != 0 || tagb != 0) {
      free_pair_list(&stack);
      fprintf(stderr, "Internal error: ocaml comparison\n");
      exit(2);
      return 0;  // Cannot compare
    } else {
      for (mlsize_t i = 0; i < sizea; i++) {
        push_pair(&stack, (void*)Field(va, i), (void*)Field(vb, i));
      }
    }
  }

  free_pair_list(&stack);
  return 1;
}

extern "C" {
SkipInt SKIP_ocamlCompareValues(value a, value b) {
  SkipInt result = (SkipInt)!ocaml_structural_equal(a, b);
  return result;
}
}
