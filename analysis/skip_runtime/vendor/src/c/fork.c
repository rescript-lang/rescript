#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

// Provided global lock/unlock primitives
extern void sk_global_lock();
extern void sk_global_unlock();

void process_element(size_t index, value closure);

// Internal guard to prevent nested fork_and_process calls
static int in_fork_and_process = 0;

pid_t ocaml_safe_fork() {
  static const value *ocaml_fork_closure = NULL;
  if (ocaml_fork_closure == NULL) {
    ocaml_fork_closure = caml_named_value("Unix.fork");
    if (ocaml_fork_closure == NULL) {
      fprintf(stderr, "Error: could not find OCaml function Unix.fork\n");
      exit(1);
    }
  }

  value pid_val = caml_callback(*ocaml_fork_closure, Val_unit);
  return Int_val(pid_val);
}

void fork_and_process(
  size_t size,
  volatile size_t* persistent_index,
  value closure
) {
  if (size == 0 || !persistent_index) return;

  if (__atomic_test_and_set(&in_fork_and_process, __ATOMIC_SEQ_CST)) {
    fprintf(stderr, "Error: fork_and_process cannot be called recursively.\n");
    return;
  }

  long cpu_count = sysconf(_SC_NPROCESSORS_ONLN);
  if (cpu_count < 1) cpu_count = 1;

  int num_processes = (int)((size < (size_t)cpu_count) ? size : cpu_count);

  for (int i = 0; i < num_processes; ++i) {
    pid_t pid = fork();
    if (pid == 0) {
      // Child process
      while (1) {
        size_t index;

        if (getppid() == 1) {
          break;
        }

        sk_global_lock();
        if (*persistent_index >= size) {
          sk_global_unlock();
          break;
        }
        index = (*persistent_index)++;
        sk_global_unlock();

        process_element(index, closure);
      }
      _exit(0);
    } else if (pid < 0) {
      perror("fork");
      __atomic_clear(&in_fork_and_process, __ATOMIC_SEQ_CST);
      exit(1);
    }
  }

  // Parent waits
  for (int i = 0; i < num_processes; ++i) {
    wait(NULL);
  }

  __atomic_clear(&in_fork_and_process, __ATOMIC_SEQ_CST);
}
