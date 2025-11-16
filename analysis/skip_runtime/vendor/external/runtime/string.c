#include "runtime.h"

#ifdef SKIP64
#include <stdlib.h>
#include <string.h>
#endif

/*****************************************************************************/
/* String implementation */
/*****************************************************************************/

uint32_t SKIP_is_string(char* obj) {
  void** vtable_ptr = container_of(obj, sk_class_inst_t, data)->vtable;
  return (uintptr_t)vtable_ptr & 0x2;
}

void sk_string_set_hash(char* obj) {
  sk_string_t* str = get_sk_string(obj);
  SkipInt acc = 0;

  for (uint32_t i = 0; i < str->size; i++) {
    acc = acc * 31 + str->data[i];
  }

  // This tag is used by SKIP_is_string to recognize strings.
  acc |= 0x2;
  str->hash = (uint32_t)acc;
}

// The size of the represented string, in bytes, excludes nul terminator.
uint32_t SKIP_String_byteSize(char* obj) {
  sk_string_t* str = get_sk_string(obj);
  return str->size;
}

SkipInt SKIP_String_hash(char* obj) {
  sk_string_t* str = get_sk_string(obj);
  return (SkipInt)str->hash;
}

// Allocates and nul-terminates a string (with no data nor hash).
char* sk_string_alloc(uint32_t size) {
  sk_string_t* obj =
      (sk_string_t*)SKIP_Obstack_alloc(sk_string_header_size + size + 1);
  obj->size = size;
  char* str = &obj->data[0];
  str[size] = '\0';
  return str;
}

char* sk_string_create(const char* buffer, uint32_t size) {
  char* result = sk_string_alloc(size);
  memcpy(result, buffer, size);
  sk_string_set_hash(result);
  return result;
}

char* SKIP_getBuildVersion() {
  return sk_string_create("1", 1);
}

char* SKIP_String__fromUtf8(char* /* class */, char* array) {
  uint32_t size = SKIP_getArraySize(array);
  return sk_string_create(array, size);
}

uint8_t SKIP_String_getByte(unsigned char* bytes, SkipInt idx) {
  return (uint8_t)bytes[idx];
}

/* Must match the layout of StringIterator in the standard library. */
typedef struct {
  char* string_data;
  SkipInt i;
} sk_StringIterator;

char* SKIP_String_StringIterator__substring(sk_StringIterator* argStart,
                                            sk_StringIterator* argEnd) {
  SkipInt start = argStart->i;
  SkipInt end = argEnd->i;
  SkipInt size = end - start;
  char* buffer = argStart->string_data + start;
  char* result = sk_string_create(buffer, size);
  return result;
}

unsigned char* SKIP_String__fromChars(const unsigned char* /* dumb */,
                                      unsigned char* src_) {
  uint32_t* src = (uint32_t*)src_;
  uint32_t size = SKIP_getArraySize((char*)src_);
  SkipInt i;
  size_t result_size = 0;
  for (i = 0; i < size; i++) {
    uint32_t code = src[i];
    if (code < 0) {
      SKIP_invalid_utf8();
    } else if (code < 0x80) {
      result_size++;
    } else if (code < 0x800) {
      result_size += 2;
    } else if (code < 0x10000) {
      result_size += 3;
    } else if (code < 0x110000) {
      result_size += 4;
    } else {
      SKIP_invalid_utf8();
    }
  }
  unsigned char* result = (unsigned char*)sk_string_alloc(result_size);
  int j = 0;
  for (i = 0; i < size; i++) {
    uint32_t code = src[i];
    if (code < 0) {
      SKIP_invalid_utf8();
    } else if (code < 0x80) {
      result[j] = (unsigned char)code;
      j++;
    } else if (code < 0x800) {
      result[j] = (unsigned char)(192 + code / 64);
      j++;
      result[j] = (unsigned char)(128 + code % 64);
      j++;
    } else if (code < 0x10000) {
      result[j] = (unsigned char)(224 + code / 4096);
      j++;
      result[j] = (unsigned char)(128 + code / 64 % 64);
      j++;
      result[j] = (unsigned char)(128 + code % 64);
      j++;
    } else if (code < 0x110000) {
      result[j] = (unsigned char)(240 + code / 262144);
      j++;
      result[j] = (unsigned char)(128 + code / 4096 % 64);
      j++;
      result[j] = (unsigned char)(128 + code / 64 % 64);
      j++;
      result[j] = (unsigned char)(128 + code % 64);
      j++;
    } else {
      SKIP_invalid_utf8();
    }
  }
  sk_string_set_hash((char*)result);
  return result;
}

char* SKIP_String_concat2(char* str1, char* str2) {
  uint32_t size1 = SKIP_String_byteSize(str1);
  uint32_t size2 = SKIP_String_byteSize(str2);
  char* result = sk_string_alloc(size1 + size2);
  memcpy(result, (const char*)str1, size1);
  memcpy(result + size1, (const char*)str2, size2);
  sk_string_set_hash(result);
  return result;
}

char* SKIP_String_concatN(char** arr) {
  uint32_t arr_size = SKIP_getArraySize((char*)arr);
  SkipInt byte_size = 0;
  unsigned int i;

  for (i = 0; i < arr_size; i++) {
    byte_size += SKIP_String_byteSize(arr[i]);
  }

  char* result = sk_string_alloc(byte_size);
  char* buffer = result;

  for (i = 0; i < arr_size; i++) {
    SkipInt str_size = SKIP_String_byteSize(arr[i]);
    memcpy(buffer, (const char*)arr[i], str_size);
    buffer += str_size;
  }

  sk_string_set_hash(result);
  return result;
}

SkipInt SKIP_String_cmp(unsigned char* str1, unsigned char* str2) {
  SkipInt size1 = SKIP_String_byteSize((char*)str1);
  SkipInt size2 = SKIP_String_byteSize((char*)str2);
  unsigned char* end1 = str1 + size1;
  unsigned char* end2 = str2 + size2;
  while (1) {
    if (str1 == end1 && str2 == end2) {
      return 0;
    }
    if (str1 == end1) {
      return (SkipInt)-1;
    }
    if (str2 == end2) {
      return 1;
    }
    unsigned char c1 = *str1;
    unsigned char c2 = *str2;
    SkipInt diff = c1 - c2;
    if (diff != 0) return diff;
    str1++;
    str2++;
  }
}

/* 8 bytes with all bits set, which is not valid utf8, but is larger than
   any valid utf8 when compared bytewise */
char* SKIP_largest_string() {
  static unsigned char buffer[] = {255, 255, 255, 255, 255, 255, 255, 255};
  return sk_string_create((char*)buffer, 8);
}

/*****************************************************************************/
/* Unsafe char access */
/*****************************************************************************/

void* SKIP_String_unsafeSlice(unsigned char* str, SkipInt n1, SkipInt n2) {
  size_t size = n2 - n1;
  char* result = sk_string_alloc(size);
  memcpy(result, str + n1, size);
  sk_string_set_hash(result);
  return result;
}

extern char* SKIP_floatToString(double origf);

char* SKIP_Float_toString(double origf) {
  return SKIP_floatToString(origf);
}

#ifdef SKIP32
#define isdigit(c) (c >= '0' && c <= '9')

double atof(const char* s) {
  double a = 0.0;
  int e = 0;
  int c;
  while ((c = *s++) != '\0' && isdigit(c)) {
    a = a * 10.0 + (c - '0');
  }
  if (c == '.') {
    while ((c = *s++) != '\0' && isdigit(c)) {
      a = a * 10.0 + (c - '0');
      e = e - 1;
    }
  }
  if (c == 'e' || c == 'E') {
    int sign = 1;
    int i = 0;
    c = *s++;
    if (c == '+')
      c = *s++;
    else if (c == '-') {
      c = *s++;
      sign = -1;
    }
    while (isdigit(c)) {
      i = i * 10 + (c - '0');
      c = *s++;
    }
    e += i * sign;
  }
  while (e > 0) {
    a *= 10.0;
    e--;
  }
  while (e < 0) {
    a *= 0.1;
    e++;
  }
  return a;
}

#endif

double SKIP_String__toFloat_raw(char* str) {
  size_t size = SKIP_String_byteSize((char*)str);
  if (size >= 255) {
    SKIP_throw_cruntime(ERROR_FLOAT_TOO_LARGE);
  }
  char cstr[256];
  memcpy(cstr, str, size);
  cstr[size] = 0;

  return atof(cstr);
}

void* SKIP_Unsafe_string_ptr(char* str, int64_t offset) {
  return str + offset;
}

/*****************************************************************************/
/* Multibyte utf8 string used for testing purposes. */
/*****************************************************************************/

static unsigned int test_utf8_data[] = {
    126, 240, 157, 152, 136, 225, 184, 134, 240, 157, 150, 162, 240,
    157, 149, 175, 217, 164, 225, 184, 158, 212, 141, 208, 157, 199,
    143, 240, 157, 153, 133, 198, 152, 212, 184, 226, 178, 152, 240,
    157, 153, 137, 224, 167, 166, 206, 161, 240, 157, 151, 164, 201,
    140, 240, 157, 147, 162, 200, 154, 208, 166, 240, 157, 146, 177};

static unsigned char string_utf8_buffer[sizeof(test_utf8_data) / sizeof(int)];

char* SKIP_utf8_test_string() {
  unsigned int i;
  for (i = 0; i < sizeof(string_utf8_buffer); i++) {
    string_utf8_buffer[i] = (unsigned char)test_utf8_data[i];
  }
  return sk_string_create((char*)string_utf8_buffer,
                          sizeof(string_utf8_buffer));
}

char* SKIP_invalid_utf8_test_string() {
  int i;
  for (i = 0; i < 2; i++) {
    string_utf8_buffer[i] = (unsigned char)test_utf8_data[i];
  }
  return sk_string_create((char*)string_utf8_buffer, 2);
}
