// interop.h
#ifndef INTEROP_H
#define INTEROP_H

#include <sys/stat.h>
#include <time.h>

#include <caml/mlvalues.h>

value oskip_create_input(
  value context, 
  value name_val, 
  value files_val, 
  value files_size_val
);

void oskip_init_context();

#endif
