#ifndef VIRTUAL_PAGE_H
#define VIRTUAL_PAGE_H

#include "map.h"  // Required for VoidMap

#ifdef __cplusplus
extern "C" {
#endif

// Write a value to a virtual page based on a pointer.
// If the page does not exist in the map, it will be created and zero-initialized.
// The pointer must be 8-byte aligned.
// On error (e.g., unaligned pointer or allocation failure), the program aborts.
void write_to_virtual_page(VoidMap* map, void* ptr, void* value);

void* lookup_in_virtual_page(VoidMap* map, void* ptr);


#ifdef __cplusplus
}
#endif

#endif // VIRTUAL_PAGE_H
