/* https://tools.ietf.org/html/rfc3629#section-10 */
/* let bom = 0xFEFF */

let repl = 0xFFFD

/* let min = 0x0000 */
let max = 0x10FFFF

let surrogateMin = 0xD800
let surrogateMax = 0xDFFF

/*
 * Char. number range  |        UTF-8 octet sequence
 *       (hexadecimal)    |              (binary)
 *    --------------------+---------------------------------------------
 *    0000 0000-0000 007F | 0xxxxxxx
 *    0000 0080-0000 07FF | 110xxxxx 10xxxxxx
 *    0000 0800-0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
 *    0001 0000-0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
 */
let h2 = 0b1100_0000
let h3 = 0b1110_0000
let h4 = 0b1111_0000

let cont_mask = 0b0011_1111

type category = {
  low: int,
  high: int,
  size: int,
}

let locb = 0b1000_0000
let hicb = 0b1011_1111

let categoryTable = [
  /* 0 */ {low: -1, high: -1, size: 1} /* invalid */,
  /* 1 */ {low: 1, high: -1, size: 1} /* ascii */,
  /* 2 */ {low: locb, high: hicb, size: 2},
  /* 3 */ {low: 0xA0, high: hicb, size: 3},
  /* 4 */ {low: locb, high: hicb, size: 3},
  /* 5 */ {low: locb, high: 0x9F, size: 3},
  /* 6 */ {low: 0x90, high: hicb, size: 4},
  /* 7 */ {low: locb, high: hicb, size: 4},
  /* 8 */ {low: locb, high: 0x8F, size: 4},
]

let categories = [
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  /* surrogate range U+D800 - U+DFFFF = 55296 - 917503 */
  0,
  0,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  2,
  3,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  4,
  5,
  4,
  4,
  6,
  7,
  7,
  7,
  8,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
]

let decodeCodePoint = (i, s, len) =>
  if len < 1 {
    (repl, 1)
  } else {
    let first = int_of_char(String.unsafe_get(s, i))
    if first < 128 {
      (first, 1)
    } else {
      let index = Array.unsafe_get(categories, first)
      if index == 0 {
        (repl, 1)
      } else {
        let cat = Array.unsafe_get(categoryTable, index)
        if len < i + cat.size {
          (repl, 1)
        } else if cat.size === 2 {
          let c1 = int_of_char(String.unsafe_get(s, i + 1))
          if c1 < cat.low || cat.high < c1 {
            (repl, 1)
          } else {
            let i1 = land(c1, 0b00111111)
            let i0 = lsl(land(first, 0b00011111), 6)
            let uc = lor(i0, i1)
            (uc, 2)
          }
        } else if cat.size === 3 {
          let c1 = int_of_char(String.unsafe_get(s, i + 1))
          let c2 = int_of_char(String.unsafe_get(s, i + 2))
          if c1 < cat.low || (cat.high < c1 || (c2 < locb || hicb < c2)) {
            (repl, 1)
          } else {
            let i0 = lsl(land(first, 0b00001111), 12)
            let i1 = lsl(land(c1, 0b00111111), 6)
            let i2 = land(c2, 0b00111111)
            let uc = lor(lor(i0, i1), i2)
            (uc, 3)
          }
        } else {
          let c1 = int_of_char(String.unsafe_get(s, i + 1))
          let c2 = int_of_char(String.unsafe_get(s, i + 2))
          let c3 = int_of_char(String.unsafe_get(s, i + 3))
          if (
            c1 < cat.low ||
              (cat.high < c1 ||
              (c2 < locb || (hicb < c2 || (c3 < locb || hicb < c3))))
          ) {
            (repl, 1)
          } else {
            let i1 = lsl(land(c1, 0x3f), 12)
            let i2 = lsl(land(c2, 0x3f), 6)
            let i3 = land(c3, 0x3f)
            let i0 = lsl(land(first, 0x07), 18)
            let uc = lor(lor(lor(i0, i3), i2), i1)
            (uc, 4)
          }
        }
      }
    }
  }

let encodeCodePoint = c =>
  if c <= 127 {
    let bytes = (@doesNotRaise Bytes.create)(1)
    Bytes.unsafe_set(bytes, 0, Char.unsafe_chr(c))
    Bytes.unsafe_to_string(bytes)
  } else if c <= 2047 {
    let bytes = (@doesNotRaise Bytes.create)(2)
    Bytes.unsafe_set(bytes, 0, Char.unsafe_chr(lor(h2, lsr(c, 6))))
    Bytes.unsafe_set(bytes, 1, Char.unsafe_chr(lor(0b1000_0000, land(c, cont_mask))))
    Bytes.unsafe_to_string(bytes)
  } else if c <= 65535 {
    let bytes = (@doesNotRaise Bytes.create)(3)
    Bytes.unsafe_set(bytes, 0, Char.unsafe_chr(lor(h3, lsr(c, 12))))
    Bytes.unsafe_set(bytes, 1, Char.unsafe_chr(lor(0b1000_0000, land(lsr(c, 6), cont_mask))))
    Bytes.unsafe_set(bytes, 2, Char.unsafe_chr(lor(0b1000_0000, land(c, cont_mask))))
    Bytes.unsafe_to_string(bytes)
  } else {
    /* if c <= max then */
    let bytes = (@doesNotRaise Bytes.create)(4)
    Bytes.unsafe_set(bytes, 0, Char.unsafe_chr(lor(h4, lsr(c, 18))))
    Bytes.unsafe_set(bytes, 1, Char.unsafe_chr(lor(0b1000_0000, land(lsr(c, 12), cont_mask))))
    Bytes.unsafe_set(bytes, 2, Char.unsafe_chr(lor(0b1000_0000, land(lsr(c, 6), cont_mask))))
    Bytes.unsafe_set(bytes, 3, Char.unsafe_chr(lor(0b1000_0000, land(c, cont_mask))))
    Bytes.unsafe_to_string(bytes)
  }

let isValidCodePoint = c => (0 <= c && c < surrogateMin) || (surrogateMax < c && c <= max)

