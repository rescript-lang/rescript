#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/address_class.h>

typedef struct {
  value *data;
  intnat capacity;
  int in_use;
} reactive_block;

static reactive_block *reactive_blocks = NULL;
static intnat reactive_blocks_capacity = 0;
static intnat reactive_live_block_count = 0;
static intnat reactive_live_block_capacity_slots = 0;

static void reactive_abort_oom(void) {
  fputs("reactive allocator: out of memory\n", stderr);
  abort();
}

static void *reactive_xrealloc(void *ptr, size_t size) {
  void *next = realloc(ptr, size);
  if (next == NULL && size != 0) {
    reactive_abort_oom();
  }
  return next;
}

static void reactive_ensure_block_registry(void) {
  intnat old_capacity;
  intnat new_capacity;

  if (reactive_blocks == NULL) {
    reactive_blocks_capacity = 16;
    reactive_blocks =
        reactive_xrealloc(NULL, (size_t)reactive_blocks_capacity * sizeof(*reactive_blocks));
    memset(reactive_blocks, 0,
           (size_t)reactive_blocks_capacity * sizeof(*reactive_blocks));
    return;
  }

  for (old_capacity = 0; old_capacity < reactive_blocks_capacity; old_capacity++) {
    if (!reactive_blocks[old_capacity].in_use) {
      return;
    }
  }

  old_capacity = reactive_blocks_capacity;
  new_capacity = old_capacity * 2;
  reactive_blocks =
      reactive_xrealloc(reactive_blocks, (size_t)new_capacity * sizeof(*reactive_blocks));
  memset(reactive_blocks + old_capacity, 0,
         (size_t)(new_capacity - old_capacity) * sizeof(*reactive_blocks));
  reactive_blocks_capacity = new_capacity;
}

static intnat reactive_alloc_block_slot(void) {
  intnat index;

  reactive_ensure_block_registry();
  for (index = 0; index < reactive_blocks_capacity; index++) {
    if (!reactive_blocks[index].in_use) {
      reactive_blocks[index].in_use = 1;
      reactive_blocks[index].data = NULL;
      reactive_blocks[index].capacity = 0;
      return index;
    }
  }

  abort();
}

static reactive_block *reactive_block_of_handle(value handle) {
  return &reactive_blocks[Int_val(handle)];
}

static void reactive_resize_block(reactive_block *block, intnat capacity) {
  intnat old_capacity;
  intnat target_capacity = capacity > 0 ? capacity : 1;

  old_capacity = block->capacity;
  block->data =
      reactive_xrealloc(block->data, (size_t)target_capacity * sizeof(value));
  if (target_capacity > old_capacity) {
    memset(block->data + old_capacity, 0,
           (size_t)(target_capacity - old_capacity) * sizeof(value));
  }
  block->capacity = target_capacity;
  reactive_live_block_capacity_slots += target_capacity - old_capacity;
}

value caml_reactive_allocator_create(value capacity) {
  intnat slot = reactive_alloc_block_slot();
  reactive_block *block = &reactive_blocks[slot];

  reactive_live_block_count += 1;
  reactive_resize_block(block, Int_val(capacity));
  return Val_int(slot);
}

value caml_reactive_allocator_destroy(value handle) {
  reactive_block *block = reactive_block_of_handle(handle);

  free(block->data);
  reactive_live_block_capacity_slots -= block->capacity;
  block->data = NULL;
  block->capacity = 0;
  block->in_use = 0;
  reactive_live_block_count -= 1;
  return Val_unit;
}

value caml_reactive_allocator_capacity(value handle) {
  reactive_block *block = reactive_block_of_handle(handle);
  return Val_int(block->capacity);
}

value caml_reactive_allocator_slot_size_bytes(value unit) {
  (void)unit;
  return Val_int(sizeof(value));
}

value caml_reactive_allocator_live_block_count(value unit) {
  (void)unit;
  return Val_int(reactive_live_block_count);
}

value caml_reactive_allocator_live_block_capacity_slots(value unit) {
  (void)unit;
  return Val_int(reactive_live_block_capacity_slots);
}

value caml_reactive_allocator_reset(value unit) {
  intnat index;
  (void)unit;

  for (index = 0; index < reactive_blocks_capacity; index++) {
    reactive_block *block = &reactive_blocks[index];
    if (block->in_use) {
      free(block->data);
      block->data = NULL;
      block->capacity = 0;
      block->in_use = 0;
    }
  }

  reactive_live_block_count = 0;
  reactive_live_block_capacity_slots = 0;
  return Val_unit;
}

value caml_reactive_allocator_resize(value handle, value capacity) {
  reactive_block *block = reactive_block_of_handle(handle);

  reactive_resize_block(block, Int_val(capacity));
  return Val_unit;
}

value caml_reactive_allocator_get(value handle, value index) {
  reactive_block *block = reactive_block_of_handle(handle);
  return block->data[Int_val(index)];
}

value caml_reactive_allocator_set(value handle, value index, value data) {
  reactive_block *block = reactive_block_of_handle(handle);

  block->data[Int_val(index)] = data;
  return Val_unit;
}

value caml_reactive_allocator_blit(value src_handle, value src_pos, value dst_handle,
                                   value dst_pos, value len) {
  reactive_block *src = reactive_block_of_handle(src_handle);
  reactive_block *dst = reactive_block_of_handle(dst_handle);

  memmove(dst->data + Int_val(dst_pos), src->data + Int_val(src_pos),
          (size_t)Int_val(len) * sizeof(value));
  return Val_unit;
}

value caml_reactive_value_is_young(value data) {
  return Val_bool(Is_block(data) && Is_young(data));
}
