#include <caml/mlvalues.h>
#include <caml/alloc.h>

//
// Platform-specific includes
//
#if (defined(__MACH__) && defined(__APPLE__))
#include <mach/mach_time.h>
#elif defined(__linux__)
#include <time.h>
#endif

//
// Platform-specific globals
//
#if (defined(__MACH__) && defined(__APPLE__))
static mach_timebase_info_data_t info;
#endif

//
// Exported functions
//
CAMLprim value caml_mach_initialize(value unit) {
#if (defined(__MACH__) && defined(__APPLE__))
  mach_timebase_info(&info);
#endif

  return Val_unit;
}

CAMLprim value caml_mach_absolute_time(value unit) {
  uint64_t result = 0;

#if (defined(__MACH__) && defined(__APPLE__))
  uint64_t now = mach_absolute_time();
  result = (now * info.numer) / info.denom;
#elif defined(__linux__)
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  result = now.tv_sec * 1000000000 + now.tv_nsec;
#endif

  return caml_copy_int64(result);
}
