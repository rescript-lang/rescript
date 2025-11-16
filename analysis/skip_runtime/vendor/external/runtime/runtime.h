/*****************************************************************************/
/* We are really trying to keep it to a bare minimum when it comes to C/C++
 * dependencies. We need the runtime to compile to webassembly, and each
 * additional dependency makes it more challenging.
 */
/*****************************************************************************/
#define SK_FTABLE_SIZE 64

#ifndef SKIP_RUNTIME
#define SKIP_RUNTIME 1

#include <stddef.h>
#include <stdint.h>

#ifndef PAGE_SIZE
#define PAGE_SIZE (1024 * 1024 * 8)
#endif

#define STACK_INIT_CAPACITY (1024)

typedef uint64_t SkipInt;

#ifdef SKIP32
#ifndef WASM_HEAP_SIZE
#define WASM_HEAP_SIZE 1073741824
#endif

#define PERSISTENT_PAGE_BIT_SIZE 20
#define PERSISTENT_PAGE_SIZE (1 << PERSISTENT_PAGE_BIT_SIZE)
#define PERSISTENT_TABLE_SIZE (WASM_HEAP_SIZE / PERSISTENT_PAGE_SIZE)
#endif

#ifdef SKIP64
#include <stdio.h>
#endif

#ifdef CTX_TABLE
#define CTX_TABLE_CAPACITY 256
void sk_clean_ctx_table();
void sk_print_ctx_table();
#endif

/*****************************************************************************/
/* Handy macros. */
/*****************************************************************************/

#define ptr_is_a_type_member_array(ptr, type, member) \
  (1 ? (ptr) : &((type*)0)->member[0])
#define container_of(ptr, type, member)                           \
  ((type*)((char*)ptr_is_a_type_member_array(ptr, type, member) - \
           offsetof(type, member)))

/*****************************************************************************/
/* The error code thrown by the runtime. */
/*****************************************************************************/

#define ERROR_TODO 1
#define ERROR_EXEC 4
#define ERROR_CHANGING_CONST 22
#define ERROR_INVALID_EXTERNAL_POINTER 23
#define ERROR_OUT_OF_MEMORY 24
#define ERROR_INTERNAL_LOCK 25
#define ERROR_LOCAL_CONTEXT_NULL 26
#define ERROR_CONTEXT_NOT_INITIALIZED 27
#define ERROR_SYNC_SAME_CONTEXT 28
#define ERROR_NOT_IMPLEMENTED 29
#define ERROR_FLOAT_TOO_LARGE 30
#define ERROR_INVALID_STRING 31
#define ERROR_MAPPING_EXISTS 40
#define ERROR_MAPPING_FAILED 41
#define ERROR_MAPPING_MEMORY 42
#define ERROR_MAPPING_VERSION 43
#define ERROR_LOCKING 44
#define ERROR_FILE_IO 45
#define ERROR_MEMORY_CHECK 88
#define ERROR_CONTEXT_CHECK 102
#define ERROR_ARG_PARSE 103

size_t sk_current_capacity(void);
char* SKIP_get_heap_base(void);

/*****************************************************************************/
/* Types used for the Obstack pages. */
/*****************************************************************************/

#ifdef SKIP32
typedef struct sk_size_info sk_size_info_t;
#endif
typedef struct sk_obstack sk_obstack_t;

/*****************************************************************************/
/* Types used for the hashtable. */
/*****************************************************************************/

#define TOMB ((uint64_t)-1)

typedef struct {
  sk_obstack_t* key;
  uint64_t value;
  sk_obstack_t* next;
} sk_cell_t;

typedef struct {
  size_t size;
  size_t rsize;
  size_t bitcapacity;
  sk_cell_t* data;
} sk_htbl_t;

void sk_htbl_init(sk_htbl_t* table, size_t bitcapacity);
void sk_htbl_free(sk_htbl_t* table);
void sk_htbl_add(sk_htbl_t* table, void* key, uint64_t value);
sk_cell_t* sk_htbl_find(sk_htbl_t* table, void* key);
int sk_htbl_mem(sk_htbl_t* table, void* key);
void sk_htbl_remove(sk_htbl_t* table, void* key);
SkipInt SKIP_String_cmp(unsigned char* str1, unsigned char* str2);
size_t sk_get_nbr_pages(sk_obstack_t* from_page, sk_obstack_t* to_page);
sk_cell_t* sk_get_pages(sk_obstack_t* from_page, size_t size);
size_t sk_get_obstack_idx(char* ptr, sk_cell_t* pages, size_t size);

/*****************************************************************************/
/* Stack types. */
/*****************************************************************************/

typedef struct {
  void** value;
  void** slot;
} sk_value_t;

typedef struct {
  size_t head;
  size_t capacity;
  sk_value_t* values;
} sk_stack_t;

void sk_stack_init(sk_stack_t* st, size_t capacity);
void sk_stack_free(sk_stack_t* st);
void sk_stack_push(sk_stack_t* st, void** value, void** slot);
sk_value_t sk_stack_pop(sk_stack_t* st);

/*****************************************************************************/
/* Stack3 types. */
/*****************************************************************************/

typedef struct {
  void* value1;
  void* value2;
  void* value3;
} sk_value3_t;

typedef struct {
  size_t head;
  size_t capacity;
  sk_value3_t* values;
} sk_stack3_t;

void sk_stack3_init(sk_stack3_t* st, size_t capacity);
void sk_stack3_free(sk_stack3_t* st);
void sk_stack3_push(sk_stack3_t* st, void* value1, void* value2, void* value3);
sk_value3_t sk_stack3_pop(sk_stack3_t* st);

/*****************************************************************************/
/* The type information exposed by the Skip compiler for each object. */
/*****************************************************************************/

#define kSkipGcKindClass 0
#define kSkipGcKindArray 1

// vtable entries created by createGCType in vtable.sk
typedef struct {
  uint8_t m_refsHintMask;
  uint8_t m_kind;  // either kSkipGcKindClass or kSkipGcKindArray
  uint8_t m_unused_tilesPerMask;
  uint8_t m_hasName;
  uint16_t m_uninternedMetadataByteSize;
  uint16_t m_unused_internedMetadataByteSize;
  SkipInt m_userByteSize;
  SkipInt m_unused_padding;
  SkipInt m_refMask[0];
  // a 0-terminated name follows if m_hasName is true
} SKIP_gc_type_t;

/* The uninterned_metadata_byte_size is the size preceding the pointer to a
   non-string GC value. It is:
   - 1 word for kSkipGcKindClass, its vtable pointer (see sk_class_inst_t)
   - 2 words for kSkipGcKindArray, its vtable pointer preceded by its size on
       32 bits, itself preceded by an unused padding of 32 bits on 64-bits arch
       (see sk_array_t).
*/
#define uninterned_metadata_byte_size(ty)       \
  (ty_is_array(ty) ? offsetof(sk_array_t, data) \
                   : offsetof(sk_class_inst_t, data))
#define uninterned_metadata_word_size(ty) \
  (uninterned_metadata_byte_size(ty) / sizeof(void*))

#define ty_is_array(ty) ((ty)->m_kind == kSkipGcKindArray)

#define ty_is_array(ty) ((ty)->m_kind == kSkipGcKindArray)

SKIP_gc_type_t* get_gc_type(char* skip_object);

/*****************************************************************************/
/* SKIP Class instance representation: */
/*****************************************************************************/

typedef struct {
  void** vtable;
  char data[0];
} sk_class_inst_t;

/*****************************************************************************/
/* SKIP Array representation: */
/*****************************************************************************/

typedef struct {
#ifdef SKIP64
  uint32_t unused_padding;
#endif
  uint32_t length;
  void** vtable;
  char data[0];
} sk_array_t;

#define skip_array_len(obj) (container_of(obj, sk_array_t, data)->length)

/*****************************************************************************/
/* SKIP objects: arrays and class instances. */
/*****************************************************************************/

#define skip_object_len(ty, obj) (ty_is_array(ty) ? skip_array_len(obj) : 1)

/*****************************************************************************/
/* SKIP String representation. */
/*****************************************************************************/

/*
 * Each skip object is preceded in memory by either a string header, if the
 * object is a string, or a vtable pointer otherwise. String headers are laid
 * out so that the least significant byte of the hash of a string is at the
 * same offset from the object pointer as the least significant byte of the
 * vtable pointer of a non-string object.  Vtable entries are guaranteed to
 * be 8-aligned, so their least significant 3 bits are clear. String hashes
 * are tagged so that bit 1 is always set. Therefore, strings can be
 * distinguished from other objects by reading what might be a vtable pointer
 * and testing bit 1.
 *
 * In 64-bit mode, reading the vtable pointer of obj reads 8 bytes from
 * obj-8, while in 32-bit mode it reads 4 bytes from obj-4. Labeling the
 * bytes 0x01 through 0x08, suppose memory is laid out as follows:
 *
 *                   SKIP64                 SKIP32
 * obj-8 -> ┌──────┐
 *          │ 0x01 │ ⎫       ⎫              ⎫
 * obj-7 -> ├──────┤ ⎪       ⎪              ⎪
 *          │ 0x02 │ ⎪       ⎪              ⎪
 * obj-6 -> ├──────┤ ⎬ hash  ⎪              ⎬ size
 *          │ 0x03 │ ⎪       ⎪              ⎪
 * obj-5 -> ├──────┤ ⎪       ⎪              ⎪
 *          │ 0x04 │ ⎭       ⎪              ⎭
 * obj-4 -> ├──────┤         ⎬ vtable ptr
 *          │ 0x05 │ ⎫       ⎪              ⎫       ⎫
 * obj-3 -> ├──────┤ ⎪       ⎪              ⎪       ⎪
 *          │ 0x06 │ ⎪       ⎪              ⎪       ⎪
 * obj-2 -> ├──────┤ ⎬ size  ⎪              ⎬ hash  ⎬ vtable ptr
 *          │ 0x07 │ ⎪       ⎪              ⎪       ⎪
 * obj-1 -> ├──────┤ ⎪       ⎪              ⎪       ⎪
 *          │ 0x08 │ ⎭       ⎭              ⎭       ⎭
 * obj   -> ├──────┤
 *          │      │
 *
 * On little-endian machines, the vtable pointer in 64-bit mode will be
 * 0x0807060504030201 and in 32-bit mode, 0x08070605. So the low byte in
 * 64-bit mode is that labeled 0x01 while in 32-bit mode the low byte is
 * 0x05. Therefore in 64-bit mode the string hash is stored in [obj-8,obj-5],
 * with the size in [obj-4,obj-1], while in 32-bit mode the order is
 * reversed: the hash in [obj-4,obj-1] and the size in [obj-8,obj-5].
 *
 * The compiler generates code that depends on this representation,
 * see AsmOutput::sk_string_header.
 *
 * Resolving this reliance on a little-endian architecture would
 * require treating the string header as a single 64-bit value in
 * 64-bit mode, but not in 32 bit mode. Additionally, the code emitted
 * by the compiler for pattern matching on string constants reads the
 * 64-bit string header with a single load, which in 32-bit mode,
 * would need to be changed to 2 32-bit loads, combining the results
 * using sk_string_header.
 */

typedef struct {
#ifdef SKIP64
  uint32_t hash;
  uint32_t size;
#else
  uint32_t size;
  uint32_t hash;
#endif
  char data[0];
} sk_string_t;

#define sk_string_header_size (offsetof(sk_string_t, data))
sk_string_t* get_sk_string(char* obj);

/*****************************************************************************/
/* SKIP linked list. */
/*****************************************************************************/

typedef struct {
  void* head;
  void* tail;
} sk_list_t;

/*****************************************************************************/
/* Function signatures. */
/*****************************************************************************/

#ifndef __cplusplus
int(memcmp)(const void* ptr1, const void* ptr2, size_t num);
void*(memcpy)(void* restrict dst, const void* restrict src, size_t n);
void*(memset)(void* b, int c, size_t len);
#endif

char* SKIP_Obstack_alloc(size_t size);
uint32_t SKIP_String_byteSize(char* str);
char* SKIP_context_get();
void* SKIP_copy_with_pages(void* obj, size_t nbr_pages, sk_cell_t* pages);
uint32_t SKIP_getArraySize(char*);
char* SKIP_get_free_slot(uint32_t);
void* SKIP_intern(void* obj);
void* SKIP_intern_shared(void* obj);
void SKIP_invalid_utf8();
SkipInt SKIP_isEq(char* obj1, char* obj2);
uint32_t SKIP_is_string(char* obj);
void SKIP_print_char(uint32_t);
int32_t SKIP_read_line_fill();
int32_t SKIP_read_to_end_fill();
uint32_t SKIP_read_line_get(uint32_t);
char* SKIP_resolve_context(uint64_t, char* context, char* obj,
                           char* synchronizer, char* lockedF);
void SKIP_call_after_unlock(char*, char*);

void SKIP_throw(void*);
__attribute__((noreturn)) void SKIP_throw_cruntime(int32_t);

void sk_commit(char*, uint32_t);
char* SKIP_context_get_unsafe();
void sk_context_set(char* obj);
void sk_context_set_unsafe(char* obj);
uintptr_t sk_decr_ref_count(void*);
void sk_free_size(void*, size_t);
void sk_free_root(char* obj);
#ifdef SKIP32
void sk_add_ftable(void*, sk_size_info_t);
void* sk_get_ftable(sk_size_info_t);
#else
void sk_add_ftable(void* ptr, size_t size);
void* sk_get_ftable(size_t size);
#endif
int sk_has_global_lock();
void sk_global_lock();
void sk_global_unlock();
void sk_incr_ref_count(void*);
int sk_is_const(void*);
int sk_is_large_page(sk_obstack_t* page);
int sk_is_in_heap(void*);
void* sk_malloc(size_t size);
char* sk_new_const(char* cst);
void sk_obstack_attach_page(sk_obstack_t* lpage, sk_obstack_t* next);
size_t sk_page_size(sk_obstack_t* page);
void* sk_palloc(size_t size);
void sk_persist_consts();
void sk_pfree_size(void*, size_t);
size_t sk_pow2_size(size_t);
void sk_print_int(SkipInt);
void sk_staging();
char* sk_string_create(const char* buffer, uint32_t size);
void sk_string_check_c_safe(char* str);
void throw_Invalid_utf8();
void todo(char* err, char* msg);
char* sk_get_external_pointer();
char* sk_get_external_pointer_destructor(char* obj);
uint32_t sk_get_external_pointer_value(char* obj);
uint32_t sk_get_magic_number(char* obj);
void sk_call_external_pointer_destructor(char*, uint32_t);
int sk_is_nofile_mode();
void sk_check_has_lock();
void sk_free_obj(sk_stack_t* st, char* obj);
void sk_free_external_pointers();
uintptr_t sk_get_ref_count(void* obj);
void SKIP_throwInvalidSynchronization();
void SKIP_call_finalize(char*, char*);
void SKIP_exit(SkipInt);
void sk_heap_sort(sk_cell_t* arr, size_t n);
int SKIP_get_version();

#endif
