// memory.h
#ifndef FORK_H
#define FORK_H

#include <caml/mlvalues.h>

void fork_and_process(
  size_t size,
  size_t *index,
  value closure
);

#endif
