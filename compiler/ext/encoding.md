





```c
CAMLprim value caml_ml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}
```

Like all heap blocks, strings contain a header defining the size of
the string in machine words.  The actual block contents are:
- the characters of the string
- padding bytes to align the block on a word boundary.  
  The padding is one of
    00
    00 01
    00 00 02
    00 00 00 03
  on a 32-bit machine, and up to 00 00 .... 07 on a 64-bit machine.

Thus, the string is always zero-terminated, and its length can be
computed as follows:

    number_of_words_in_block * sizeof(word) - last_byte_of_block - 1

The null-termination comes handy when passing a string to C, but is
not relied upon to compute the length (in Caml), allowing the string
to contain nulls.

so, suppose 

"" -> `8 - 7 - 1 `
"a" -> `8 - 6 - 1`
"0123456" -> `8 - 0 - 1`
"01234567" ->  `2 * 8 - 7 - 1`