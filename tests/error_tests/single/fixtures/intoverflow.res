let v1: int = 2_147_483_647 // max int
let v2: int = 2_147_483_648 // overflow
let v3: int = 2_147_483_649 // overflow
let v4: int = -2_147_483_647
let v5: int = -2_147_483_648 // min int
let v6: int = -2_147_483_649 // underflow

// hex
let v7: int = 0xFFFF_FFFF // -1
let v8: int = -0xFFFF_FFFF // 1
let v9: int = 0x1_0000_0000 // overflow
let v10: int = -0x1_0000_0000 // underflow

// Wrapped aliases are redundant after int32 normalization.
let wrappedAlias = value =>
  switch value {
  | 4294967295 => 1
  | -1 => 2
  | _ => 0
  }

let nondecimalAlias = value =>
  switch value {
  | -0xFFFF_FFFF => 1
  | 1 => 2
  | _ => 0
  }
