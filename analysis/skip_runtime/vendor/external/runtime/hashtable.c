#include "runtime.h"

void sk_htbl_init(sk_htbl_t* table, size_t bitcapacity) {
  size_t capacity = 1 << bitcapacity;
  sk_cell_t* data = sk_malloc(sizeof(sk_cell_t) * capacity);
  size_t i;

  // Sets the unused keys to zero.
  for (i = 0; i < capacity; i++) {
    data[i].key = 0;
  }

  table->size = 0;
  table->rsize = 0;
  table->bitcapacity = bitcapacity;
  table->data = data;
}

void sk_htbl_free(sk_htbl_t* table) {
  size_t capacity = 1 << table->bitcapacity;
  sk_free_size(table->data, sizeof(sk_cell_t) * capacity);
}

void sk_htbl_resize(sk_htbl_t* table) {
  size_t table_size = 1 << table->bitcapacity;
  size_t bitcapacity = table->bitcapacity;

  if (table->rsize > table_size / 2) {
    bitcapacity++;
  }

  sk_htbl_t new_table;
  sk_htbl_init(&new_table, bitcapacity);

  size_t i;

  for (i = 0; i < 1 << table->bitcapacity; i++) {
    if (table->data[i].key != 0 && table->data[i].value != TOMB) {
      sk_htbl_add(&new_table, table->data[i].key, table->data[i].value);
    }
  }

  sk_free_size(table->data, sizeof(sk_cell_t) * table_size);
  *table = new_table;
}

void sk_htbl_add(sk_htbl_t* table, void* key, uint64_t value) {
  size_t capacity = 1 << table->bitcapacity;

  if (table->size >= capacity / 2) {
    sk_htbl_resize(table);
  }

  capacity = 1 << table->bitcapacity;

  uintptr_t ikey = (uintptr_t)key;
  ikey = ikey & (capacity - 1);
  uintptr_t n = 1;

  while (table->data[ikey].key != 0 &&
         !(table->data[ikey].key == key && table->data[ikey].value == TOMB)) {
    ikey = (ikey + n * n) & (capacity - 1);
    n++;
  }

  table->size++;
  table->rsize++;
  table->data[ikey].key = key;
  table->data[ikey].value = value;
}

sk_cell_t* sk_htbl_find(sk_htbl_t* table, void* key) {
  size_t capacity = 1 << table->bitcapacity;
  uintptr_t ikey = (uintptr_t)key;
  ikey = ikey & (capacity - 1);
  uintptr_t n = 1;

  while (table->data[ikey].key != 0) {
    if (table->data[ikey].key == key) {
      if (table->data[ikey].value == TOMB) {
        return NULL;
      }
      return &table->data[ikey];
    }
    ikey = (ikey + n * n) & (capacity - 1);
    n++;
  }

  return NULL;
}

int sk_htbl_mem(sk_htbl_t* table, void* key) {
  sk_cell_t* cell = sk_htbl_find(table, key);

  if (cell == NULL) {
    return 0;
  }

  return (cell->value != TOMB);
}

void sk_htbl_remove(sk_htbl_t* table, void* key) {
  sk_cell_t* cell = sk_htbl_find(table, key);

  if (cell == NULL) {
    return;
  }

  table->rsize--;
  cell->value = TOMB;
}

int sk_test_table() {
  sk_htbl_t table_slot;
  sk_htbl_t* table = &table_slot;

  sk_htbl_init(table, 20);

  size_t i = 0;
  for (i = 1; i < 1000000; i++) {
    sk_htbl_add(table, (void*)i, i);
  }

  for (i = 1; i < 10000; i++) {
    sk_cell_t* cell = sk_htbl_find(table, (void*)i);
    if (cell == NULL) {
      return 2;
    }
    if ((uintptr_t)cell->value != i) {
      return 3;
    }
  }

  return 0;
}
