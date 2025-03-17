type localeMatcher = [#lookup | #"best fit"]

type calendar = [
  | #buddhist
  | #chinese
  | #coptic
  | #dangi
  | #ethioaa
  | #ethiopic
  | #gregory
  | #hebrew
  | #indian
  | #islamic
  | #"islamic-umalqura"
  | #"islamic-tbla"
  | #"islamic-civil"
  | #"islamic-rgsa"
  | #iso8601
  | #japanese
  | #persian
  | #roc
]

type collation = [
  | #compat // (Arabic)
  | #dict // (Sinhala)
  | #emoji // (root)
  | #eor // (root)
  | #phonebk // (German)
  | #phonetic // (Lingala)
  | #pinyin // (Chinese)
  | #stroke // (Chinese)
  | #trad
  | #unihan // (Chinese, Japanese, and Korean; not available in Chrome or Edge)
  | #zhuyin
] // (Chinese)

type numberingSystem = [
  | #adlm
  | #ahom
  | #arab
  | #arabext
  | #bali
  | #beng
  | #bhks
  | #brah
  | #cakm
  | #cham
  | #deva
  | #diak
  | #fullwide
  | #gong
  | #gonm
  | #gujr
  | #guru
  | #hanidec
  | #hmng
  | #hmnp
  | #java
  | #kali
  | #kawi
  | #khmr
  | #knda
  | #lana
  | #lanatham
  | #laoo
  | #latn
  | #lepc
  | #limb
  | #mathbold
  | #mathdbl
  | #mathmono
  | #mathsanb
  | #mathsans
  | #mlym
  | #modi
  | #mong
  | #mroo
  | #mtei
  | #mymr
  | #mymrshan
  | #mymrtlng
  | #nagm
  | #newa
  | #nkoo
  | #olck
  | #orya
  | #osma
  | #rohg
  | #saur
  | #segment
  | #shrd
  | #sind
  | #sinh
  | #sora
  | #sund
  | #takr
  | #talu
  | #tamldec
  | #telu
  | #thai
  | #tibt
  | #tirh
  | #tnsa
  | #vaii
  | #wara
  | #wcho
]

type oneTo21 = [
  | #1
  | #2
  | #3
  | #4
  | #5
  | #6
  | #7
  | #8
  | #9
  | #10
  | #11
  | #12
  | #13
  | #14
  | #15
  | #16
  | #17
  | #18
  | #19
  | #20
  | #21
]

type zeroTo20 = [
  | #0
  | #1
  | #2
  | #3
  | #4
  | #5
  | #6
  | #7
  | #8
  | #9
  | #10
  | #11
  | #12
  | #13
  | #14
  | #15
  | #16
  | #17
  | #18
  | #19
  | #20
]
