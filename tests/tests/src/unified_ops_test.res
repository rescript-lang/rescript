let int = 1 + 2
let float = 1. + 2.
let string = "1" + "2"
let bigint = 1n + 2n

let unknown = (a, b) => a + b

let lhsint = (a: int, b) => a + b
let lhsfloat = (a: float, b) => a + b
let lhsbigint = (a: bigint, b) => a + b
let lhsstring = (a: string, b) => a + b

let rhsint = (a, b: int) => a + b
let rhsfloat = (a, b: float) => a + b
let rhsbigint = (a, b: bigint) => a + b
let rhsstring = (a, b: string) => a + b

let case1 = a => 1 + a
let case2 = (a, b) => a + "test" + b

let even = n => n % 2 == 0
let odd = n => n % 2 == 1

let pow1 = 2 ** 2
let pow2 = 2. ** 2.
let pow3 = 2n ** 2n

let pow_overflow = 2147483647 ** 2

let bnot_int = a => ~a
let bnot_bigint = (a: bigint) => ~a

let bxor_int = (a, b) => a ^ b
let bxor_bigint = (a: bigint, b) => a ^ b

let band_int = (a, b) => a & b
let band_bigint = (a: bigint, b) => a & b

let intShiftLeft = 1 << 2
let intShiftRight = 8 >> 2
let intShiftRightUnsigned = -2 >>> 1

let bigintShiftLeft = 1n << 2n
let bigintShiftRight = 8n >> 2n
