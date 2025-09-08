let x = true
let y = false

let txt = "a string"

let txtWithEscapedChar = "foo\nbar"

let number = 1

let template = `amazing
  multine
  template
     string
`


let complexNumber = 1.6
let x = 0b0000_0001

let int32 = 42l
let int64 = 42L

let x = -44.20e99
let x = -44.20_34e99
let x = -44.20e+9
let x = -44.20e-9
let x = -44.20e-99_99
let x = -444_444.20e99
let x = 44.20e99
let x = 44.20_34e99
let x = 44.20e+9
let x = 44.20e-9
let x = 44.20e-99_99
let x = 44e99
let x = 44_44e99

let x = 0x0
let x = 0X0

let x = 0xAA
let x = 0XA_A
let x = 0xAA.ff
let x = 0xAA.ff_ff
let x = 0xAA.ff_ffp10
let x = 0xAA.ff_ffp+10
let x = 0xAA.ff_ffp-10
let x = 0xAA.ff_ffp100_00
let x = 0xAA.ff_ffp+100_00
let x = 0xAA.ff_ffp-100_00

let x = 'a' // char
let x = '\\'
let x = '\''
let x = '\n'
let x = '\t'
let x = '\b'
let x = '\r'
let x = ' '
let x = '\xAA'

let () = {
  getResult()
  -10
}

let x = "foo\0bar"
let x = "foo\x0Abar"
let x = "\\abc"
let x = "\'bar"
let x = "\nbar"
let x = "\tbar"
let x = "\bbar"
let x = "\rbar"
let x = "\ bar"
let x = "\u00A9"
let x = "\u00a9"
let x = "\u2665"
let a = "\u{000000000061}"
let x = "\u{00A9}"
let x = "\u{00a9}"
let x = "\oAAA" // same as "oAAA" since \o is not a valid escape
let x = "\m" // same as "m" since \m is not a valid escape
let smile = "\u{1F600}"
let smile = "\u{1f600}"

// represent the same thing
let u = "日本語" 
let u = "\u65e5\u672c\u8a9e"
let u = "\u{000065e5}\u{0000672c}\u{00008a9e}"
let u = "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e"
