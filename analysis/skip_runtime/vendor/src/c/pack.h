// pack.h
#ifndef PACK_H
#define PACK_H

#include <caml/mlvalues.h>

void* sk_ocaml_prepare_packing(value);
void* sk_ocaml_finalize_packing(void*);
void* sk_ocaml_pack(value root);

#endif
