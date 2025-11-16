#include <stdbool.h>

#include "runtime.h"
#include "xoroshiro128plus.h"

void js_throw(void*, uint32_t);

void* exn = (void*)0;

void SKIP_throw(void* exc) {
  uint32_t rethow = exn == exc;
  exn = exc;
  js_throw(exc, rethow);
}

void SKIP_saveExn(void* exc) {
  exn = exc;
}

void js_replace_exn(void* old, void* new);

void SKIP_replaceExn(void* exc) {
  js_replace_exn(exn, exc);
  exn = exc;
}

void* SKIP_getExn() {
  return exn;
}

char* end_of_static;
extern unsigned char* bump_pointer;
unsigned char* real_heap_end;
unsigned char* heap_end;
extern SKIP_gc_type_t* epointer_ty;

void reset_heap_end() {
  heap_end = real_heap_end;
}

unsigned char* decr_heap_end(size_t size) {
  heap_end -= size;
  return heap_end;
}

void SKIP_skstore_init(uint32_t size) {
  real_heap_end = bump_pointer + size;
  heap_end = real_heap_end;
  end_of_static = (char*)bump_pointer;
  char* obj = sk_get_external_pointer();
  epointer_ty = get_gc_type(obj);
}

void SKIP_destroy_Obstack(void*);

void SKIP_skstore_end_of_init() {
  if ((char*)bump_pointer != (char*)0) {
    SKIP_destroy_Obstack((char*)0);
  }
}

int sk_is_in_heap(void* ptr) {
  return (char*)ptr >= end_of_static;
}

void sk_staging() {}

void sk_commit(char* new_root, uint32_t /* sync */) {
  sk_context_set_unsafe(new_root);
}

char* SKIP_read_file(char* /* filename_obj */) {
  SKIP_throw_cruntime(ERROR_NOT_IMPLEMENTED);
  return (void*)0;
}

void* context;

void sk_global_lock() {}

void sk_global_unlock() {}

uint32_t SKIP_has_context() {
  return (uint32_t)(context != NULL);
}

char* SKIP_context_get_unsafe() {
  if (context != NULL) {
    sk_incr_ref_count(context);
  }
  return context;
}

char* SKIP_context_get() {
  return SKIP_context_get_unsafe();
}

void sk_context_set(char* obj) {
  context = obj;
}

void sk_context_set_unsafe(char* obj) {
  context = obj;
}

SkipInt SKIP_genSym(SkipInt /* n */) {
  static SkipInt x = 1;
  x++;
  return x;
}

char* sk_new_const(char* cst) {
  return SKIP_intern_shared(cst);
}

void SKIP_throw_EndOfFile();

int64_t SKIP_posix_isatty(int64_t /* fd */) {
  return 0;
}

void* SKIP_exec(char* /* cmd */) {
  // Not implemented
  return NULL;
}

void SKIP_write_to_proc(void* /* proc */, char* /* str */) {
  // Not implemented.
}

void SKIP_wait_for_proc(void* /* proc */) {
  // Not implemented
}

void sk_check_has_lock() {
  // always true in wasm mode.
}

void SKIP_print_persistent_size() {
  // Not implemented
}

uint32_t SKIP_get_persistent_size() {
  return (uint32_t)bump_pointer;
}

void SKIP_time() {
  // Not implemented
}

uint32_t SKIP_js_time_ms_lo();
uint32_t SKIP_js_time_ms_hi();

uint64_t SKIP_time_ms() {
  uint32_t lo = SKIP_js_time_ms_lo();
  uint32_t hi = SKIP_js_time_ms_hi();
  return (((uint64_t)hi) << 32) | ((uint64_t)lo);
}

void SKIP_flush_stdout() {
  // Not implemented
}

int32_t SKIP_notify(char* /* filename_obj */, int32_t /* tick */) {
  // Not implemented
  return 0;
}

uint32_t SKIP_js_get_entropy();

void SKIP_random_init() {
  uint32_t lo = SKIP_js_get_entropy();
  uint32_t hi = SKIP_js_get_entropy();
  uint64_t seed = (((uint64_t)hi) << 32) | ((uint64_t)lo);
  xoroshiro128plus_init(seed);
}

uint64_t SKIP_random_next() {
  return xoroshiro128plus_next();
}

__attribute__((noreturn)) void SKIP_exit(uint64_t code) {
  SKIP_throw_cruntime((int32_t)code);
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

void SKIP_mutex_init(void* /* lock */) {}

void SKIP_mutex_lock(void* /* lock */) {}

void SKIP_mutex_unlock(void* /* lock */) {}

void SKIP_cond_init(void* /* cond */) {}

void SKIP_cond_wait(void* /* x */, void* /* y */) {}

int32_t SKIP_cond_timedwait(void* /* x */, void* /* y */, uint32_t /* secs */) {
  return 0;
}

int32_t SKIP_cond_broadcast(void* /* c */) {
  return 0;
}

int SKIP_stdin_has_data() {
  return 1;
}

void SKIP_unix_die_on_EOF() {}

int32_t SKIP_js_open_flags(bool read, bool write, bool append, bool truncate,
                           bool create, bool create_new);

int64_t SKIP_posix_open_flags(int64_t read, int64_t write, int64_t append,
                              int64_t truncate, int64_t create,
                              int64_t create_new) {
  return SKIP_js_open_flags((bool)read, (bool)write, (bool)append,
                            (bool)truncate, (bool)create, (bool)create_new);
}

int32_t SKIP_js_open(char* path, int32_t oflag, int32_t mode);

int64_t SKIP_posix_open(char* path, int64_t oflag, int64_t mode) {
  return (int64_t)SKIP_js_open(path, (int32_t)oflag, (int32_t)mode);
}

void SKIP_js_close(int32_t fd);

void SKIP_posix_close(int64_t fd) {
  SKIP_js_close((int32_t)fd);
}

uint32_t SKIP_js_write(uint32_t fd, char* buf, uint32_t len);

int64_t SKIP_posix_write(int64_t fd, char* buf, int64_t len) {
  return (int64_t)SKIP_js_write((uint32_t)fd, buf, (uint32_t)len);
}

int32_t SKIP_js_read(uint32_t fd, char* buf, uint32_t len);

int64_t SKIP_posix_read(int64_t fd, char* buf, int64_t len) {
  return (int64_t)SKIP_js_read((uint32_t)fd, buf, (uint32_t)len);
}

int32_t SKIP_js_get_argc();

int64_t SKIP_getArgc() {
  return (int64_t)SKIP_js_get_argc();
}

char* SKIP_js_get_argn(int32_t n);

char* SKIP_getArgN(int64_t n) {
  return SKIP_js_get_argn((int32_t)n);
}

uint32_t SKIP_js_get_envc();

int64_t SKIP_get_envc() {
  return (int64_t)SKIP_js_get_envc();
}

char* SKIP_js_get_envn(uint32_t n);

char* SKIP_get_envN(int64_t n) {
  return SKIP_js_get_envn((uint32_t)n);
}

char* SKIP_js_pipe();

char* SKIP_posix_pipe() {
  return SKIP_js_pipe();
}

uint32_t SKIP_js_fork();

int64_t SKIP_posix_fork() {
  return (int64_t)SKIP_js_fork();
}

void SKIP_js_dup2(uint32_t oldfd, uint32_t newfd);

void SKIP_posix_dup2(int64_t oldfd, int64_t newfd) {
  SKIP_js_dup2((uint32_t)oldfd, (uint32_t)newfd);
}

void SKIP_js_execvp(char* args_obj);

void SKIP_posix_execvp(char* args_obj) {
  SKIP_js_execvp(args_obj);
}
