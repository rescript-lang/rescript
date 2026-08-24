#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/signals.h"

#ifdef _WIN32
#include <sys/utime.h>

CAMLprim value caml_stale_file(value path)
{
  CAMLparam1(path);
  struct _utimbuf times;
  char *os_path = caml_stat_strdup(String_val(path));
  times.modtime = 0;
  caml_enter_blocking_section();
  _utime(os_path, &times);
  caml_leave_blocking_section();
  caml_stat_free(os_path);
  CAMLreturn(Val_unit);
}
#else
#include <sys/time.h>

CAMLprim value caml_stale_file(value path)
{
  CAMLparam1(path);
  struct timeval times[2] = {{0, 0}, {0, 0}};
  char *os_path = caml_stat_strdup_to_os(String_val(path));
  utimes(os_path, times);
  caml_stat_free(os_path);
  CAMLreturn(Val_unit);
}
#endif
