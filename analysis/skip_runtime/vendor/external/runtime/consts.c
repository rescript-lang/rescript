/*****************************************************************************/
/* File dealing with constants initialization.
 *
 * During the initialization phase, an array keeping track of all constants
 * is allocated with malloc. Until we know how many constants there are.
 * We then transfer the array into a persistent array.
 *
 * For all the subsequent process initializations (the ones that start from an
 * existing file), we check if the constant has changed, if it hasn't we keep
 * the data that was in the persistent heap.
 * If not, we update the array with the new interned value (and free the old
 * one).
 */
/*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"

#define MCONSTS_INIT_SIZE 1024

// pconsts = persistent consts (the array is in the persistent heap).
extern void*** pconsts;
size_t pconsts_count = 0;

// mconsts = malloced consts (the array is allocated with malloc).
void** mconsts = NULL;
size_t mconsts_count = 0;
size_t mconsts_size = 0;

int unsafe_new_const_mode = 0;

void SKIP_unsafe_enable_new_const_mode() {
  unsafe_new_const_mode = 1;
}

void SKIP_unsafe_disable_new_const_mode() {
  unsafe_new_const_mode = 0;
}

char* sk_new_const(char* cst) {
  if ((*pconsts) != NULL) {
    void* pcst = (*pconsts)[pconsts_count];
    if (SKIP_isEq(pcst, cst) == 0) {
      pconsts_count++;
      return pcst;
    }
#ifdef SKIP64
    if (!sk_is_nofile_mode()) {
      if (unsafe_new_const_mode) {
        return cst;
      }
      fprintf(stderr, "Cannot have a changing constant in persistent mode\n");
      SKIP_throw_cruntime(ERROR_CHANGING_CONST);
    }
#endif
    sk_global_lock();
    char* icst = SKIP_intern_shared(cst);
    sk_free_root((*pconsts)[pconsts_count]);
    (*pconsts)[pconsts_count] = icst;
    sk_global_unlock();
    pconsts_count++;
    return icst;
  }

  if (mconsts == NULL) {
    mconsts = (void**)sk_malloc(MCONSTS_INIT_SIZE * sizeof(void*));
    mconsts_size = MCONSTS_INIT_SIZE;
  }

  if (mconsts_count >= mconsts_size) {
    size_t new_size = mconsts_size * 2;
    void** new_mconsts = (void**)sk_malloc(new_size * sizeof(void*));
    memcpy(new_mconsts, mconsts, mconsts_size * sizeof(void*));
    sk_free_size(mconsts, mconsts_size * sizeof(void*));
    mconsts = new_mconsts;
    mconsts_size = new_size;
  }

  sk_global_lock();
  char* pcst = SKIP_intern_shared(cst);
  mconsts[mconsts_count] = pcst;
  mconsts_count++;
  sk_global_unlock();

  return pcst;
}

/*****************************************************************************/
/* Called after the consts initialization is over.
 * If it's the first initialization (the persistent heap started from scratch)
 * we copy over the data that was in mconsts into pconsts.
 * Otherwise, we do nothing.
 */
/*****************************************************************************/

void sk_persist_consts() {
  if ((*pconsts) != NULL) return;
  sk_global_lock();
  *pconsts = (void**)sk_palloc(mconsts_count * sizeof(void*));
  memcpy(*pconsts, mconsts, mconsts_count * sizeof(void*));
  sk_free_size(mconsts, mconsts_size * sizeof(void*));
  sk_global_unlock();
}
