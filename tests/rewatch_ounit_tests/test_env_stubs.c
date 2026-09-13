#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>

CAMLprim value rewatch_test_unsetenv(value name) {
  CAMLparam1(name);
#ifdef _WIN32
  int result = _putenv_s(String_val(name), "");
#else
  int result = unsetenv(String_val(name));
#endif
  if (result != 0) {
    caml_failwith("could not restore the test process environment");
  }
  CAMLreturn(Val_unit);
}
