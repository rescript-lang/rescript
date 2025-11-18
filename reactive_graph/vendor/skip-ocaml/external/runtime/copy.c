#include "runtime.h"

/*****************************************************************************/
/* Copying primitives. */
/*****************************************************************************/

extern size_t total_palloc_size;

static char* shallow_copy(char* obj, size_t memsize, size_t leftsize,
                          sk_cell_t* large_page) {
  if (large_page != NULL) {
    sk_obstack_attach_page(large_page->key, large_page->next);
    return obj;
  }

  memsize += leftsize;
  size_t alloc_size = memsize;
  char* mem = SKIP_Obstack_alloc(alloc_size);
  memcpy(mem, obj - leftsize, memsize);
  mem = mem + leftsize;
  return mem;
}

static char* SKIP_copy_obj(sk_stack_t* st, char* obj, sk_cell_t* large_page) {
  SKIP_gc_type_t* ty = get_gc_type(obj);

  size_t len = skip_object_len(ty, obj);
  size_t memsize = ty->m_userByteSize * len;
  size_t leftsize = uninterned_metadata_byte_size(ty);
  char* result = shallow_copy(obj, memsize, leftsize, large_page);

  if ((ty->m_refsHintMask & 1) != 0) {
    const size_t refMaskWordBitSize = sizeof(ty->m_refMask[0]) * 8;
    char* rhead = result;
    char* ohead = obj;
    char* end = obj + memsize;

    while (ohead < end) {
      size_t size = ty->m_userByteSize;
      size_t mask_slot = 0;
      while (size > 0) {
        unsigned int i;
        for (i = 0; i < refMaskWordBitSize && size > 0; i++) {
          if (ty->m_refMask[mask_slot] & (1 << i)) {
            void** ptr = (void**)ohead;
            if (*ptr != NULL) {
              void** slot = (void**)rhead;
              sk_stack_push(st, ptr, slot);
            }
          }
          ohead += sizeof(void*);
          rhead += sizeof(void*);
          size -= sizeof(void*);
        }
        mask_slot++;
      }
    }
  }

  return result;
}

static char* SKIP_copy_string(char* obj, sk_cell_t* large_page) {
  size_t memsize = get_sk_string(obj)->size + 1;
  char* result = shallow_copy(obj, memsize, sk_string_header_size, large_page);
  return result;
}

void* SKIP_copy_with_pages(void* obj, size_t nbr_pages, sk_cell_t* pages) {
  if (obj == NULL) {
    return NULL;
  }

  sk_stack_t st_holder;
  sk_stack_t* st = &st_holder;
  sk_stack3_t st3_holder;
  sk_stack3_t* st3 = &st3_holder;

  sk_stack_init(st, STACK_INIT_CAPACITY);
  sk_stack3_init(st3, STACK_INIT_CAPACITY);

  void* result = obj;
  sk_stack_push(st, &obj, &result);
  sk_cell_t* large_page = NULL;

  while (st->head > 0) {
    sk_value_t delayed = sk_stack_pop(st);
    void* toCopy = *delayed.value;

    size_t obstack_idx = sk_get_obstack_idx(toCopy, pages, nbr_pages);

    if (obstack_idx >= nbr_pages) {
      continue;
    }

    large_page = NULL;

    if (sk_is_large_page(pages[obstack_idx].key)) {
      large_page = &pages[obstack_idx];
      pages[obstack_idx].value = (uint64_t)pages[obstack_idx].key;
    }

    void* copied_ptr;

    if (SKIP_is_string(toCopy)) {
      sk_string_t* str = get_sk_string(toCopy);
      // mark already-copied strings by setting size to -1
      if (str->size == (uint32_t)-1) {
        void* copied_ptr = *(void**)toCopy;
        *delayed.slot = copied_ptr;
        continue;
      }
      void* copied_ptr = SKIP_copy_string(toCopy, large_page);
      sk_stack3_push(st3, (void**)toCopy, *(void**)toCopy,
                     (void*)(uintptr_t)(str->size));
      str->size = (uint32_t)-1;
      *(void**)toCopy = copied_ptr;
      *delayed.slot = copied_ptr;
      continue;
    }

    void*** addr_vtable_ptr =
        &(container_of(toCopy, sk_class_inst_t, data)->vtable);
    void** vtable_ptr = *addr_vtable_ptr;
    // mark already-copied objects by replacing their vtable pointer with a
    // forwarding pointer to the copied object, with the lsb set to
    // distinguish it from vtable pointers
    if (((uintptr_t)vtable_ptr & 1) == 0) {
      copied_ptr = SKIP_copy_obj(st, toCopy, large_page);
      sk_stack3_push(st3, addr_vtable_ptr, vtable_ptr, NULL);
      *addr_vtable_ptr = (void*)((uintptr_t)copied_ptr | 1);
    } else {
      copied_ptr = (void*)((uintptr_t)vtable_ptr & ~1);
    }

    *delayed.slot = copied_ptr;
  }

  while (st3->head > 0) {
    sk_value3_t cell = sk_stack3_pop(st3);
    void** toClean = cell.value1;
    *toClean = cell.value2;
    if (cell.value3 != NULL) {
      sk_string_t* str = get_sk_string(cell.value1);
      str->size = (uint32_t)(uintptr_t)cell.value3;
    }
  }

  sk_stack_free(st);
  sk_stack3_free(st3);

  return result;
}
