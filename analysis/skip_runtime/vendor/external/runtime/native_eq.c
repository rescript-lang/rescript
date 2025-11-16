#include "runtime.h"

/*****************************************************************************/
/* Native equality implementation.
 * => May return true when two objects are equal.
 * => Always returns false when two objects are not equal.
 *
 * This function cannot be used to replace equality in general. Because it may
 * return false when two objects are actually equal.
 *
 * When does this happen?
 * Consider the following object:
 * class MyObj(Int16, String)
 *
 * Now let's imagine that the layout is done in such a way that the Int16 is
 * "padded" with 16 bits. These 16 bits may contain random garbage (this is
 * only true in embedded32 mode, in normal and embedded64 mode that space is
 * actually filled with zeros, but still). If we want our algorithm to work,
 * we need to take into account the fact that because of padding, two objects
 * can be considered different, when they are in fact equal.
 *
 * Because this function is used to decide if we need to invalide a cache or
 * not: this ends not being a problem. We might recompute a little bit too
 * much on the rare occasions where this padding problem occurs ... which is
 * ok!
 */
/*****************************************************************************/

SkipInt SKIP_native_eq_obj(sk_stack_t* st, char* obj1, char* obj2) {
  if (obj1 == obj2) return 0;

  SKIP_gc_type_t* ty1 = get_gc_type(obj1);
  SKIP_gc_type_t* ty2 = get_gc_type(obj2);

  if (ty1 != ty2) {
    return 1;
  }

  size_t len1 = skip_object_len(ty1, obj1);
  size_t len2 = skip_object_len(ty1, obj2);

  if (len1 != len2) {
    return 1;
  }

  size_t memsize = ty1->m_userByteSize * len1;

  if ((ty1->m_refsHintMask & 1) == 0) {
    return (SkipInt)memcmp(obj1, obj2, memsize);
  }

  const size_t refMaskWordBitSize = sizeof(ty1->m_refMask[0]) * 8;

  char* ohead1 = obj1;
  char* ohead2 = obj2;
  char* end = obj1 + memsize;

  while (ohead1 < end) {
    size_t size = ty1->m_userByteSize;
    size_t mask_slot = 0;
    while (size > 0) {
      unsigned int i;
      for (i = 0; i < refMaskWordBitSize && size > 0; i++) {
        void* ptr1 = *((void**)ohead1);
        void* ptr2 = *((void**)ohead2);

        if (ptr1 != ptr2) {
          if (ty1->m_refMask[mask_slot] & (1 << i)) {
            sk_stack_push(st, ptr1, ptr2);
          } else {
            return 1;
          }
        }

        ohead1 += sizeof(void*);
        ohead2 += sizeof(void*);
        size -= sizeof(void*);
      }
      mask_slot++;
    }
  }

  return 0;
}

SkipInt SKIP_native_eq_helper(sk_stack_t* st, char* obj1, char* obj2) {
  if (obj1 == obj2) {
    return 0;
  }

  if (obj1 == NULL || obj2 == NULL) {
    return 1;
  }

  uint32_t isString1 = SKIP_is_string(obj1);
  uint32_t isString2 = SKIP_is_string(obj2);

  if (isString1 && isString2) {
    return SKIP_String_cmp((unsigned char*)obj1, (unsigned char*)obj2);
  }

  if (isString1 || isString2) {
    return 1;
  }

  return SKIP_native_eq_obj(st, obj1, obj2);
}

SkipInt SKIP_isEq(char* obj1, char* obj2) {
  sk_stack_t st_holder;
  sk_stack_t* st = &st_holder;
  sk_stack_init(st, STACK_INIT_CAPACITY);
  SkipInt cmp = SKIP_native_eq_helper(st, obj1, obj2);
  if (cmp != 0) {
    sk_stack_free(st);
    return !!cmp;
  }
  while (st->head > 0) {
    sk_value_t delayed = sk_stack_pop(st);
    void* obj1 = delayed.value;
    void* obj2 = delayed.slot;
    SkipInt cmp = SKIP_native_eq_helper(st, obj1, obj2);
    if (cmp != 0) {
      sk_stack_free(st);
      return !!cmp;
    }
  }
  sk_stack_free(st);
  return 0;
}

uint32_t SKIP_unsafe_compare_sets(char* obj1, char* obj2) {
  return (obj1 == obj2);
}
