@deriving(jsConverter)
type t = [
  | #variant0
  | #variant1
  | #variant2
  | #variant3
  | #variant4
  | #variant5
  | #variant6
  | #variant7
  | #variant8
  | #variant9
  | #variant10
  | #variant11
  | #variant12
  | #variant13
  | #variant14
  | #variant15
  | #variant16
  | #variant17
  | #variant18
  | #variant19
  | #variant20
  | #variant21
  | #variant22
  | #variant23
  | #variant24
  | #variant25
  | #variant26
  | #variant27
  | #variant28
  | #variant29
  | #variant30
  | #variant31
  | #variant32
  | #variant33
  | #variant34
  | #variant35
  | #variant36
  | #variant37
  | #variant38
  | #variant39
  | #variant40
  | #variant41
  | #variant42
  | #variant43
  | #variant44
  | #variant45
  | #variant46
  | #variant47
  | #variant48
  | #variant49
  | #variant50
  | #variant51
  | #variant52
  | #variant53
  | #variant54
  | #variant55
  | #variant56
  | #variant57
  | #variant58
  | #variant59
  | #variant60
  | #variant61
  | #variant62
  | #variant63
  | #variant64
  | #variant65
  | #variant66
  | #variant67
  | #variant68
  | #variant69
  | #variant70
  | #variant71
  | #variant72
  | #variant73
  | #variant74
  | #variant75
  | #variant76
  | #variant77
  | #variant78
  | #variant79
  | #variant80
  | #variant81
  | #variant82
  | #variant83
  | #variant84
  | #variant85
  | #variant86
  | #variant87
  | #variant88
  | #variant89
  | #variant90
  | #variant91
  | #variant92
  | #variant93
  | #variant94
  | #variant95
  | #variant96
  | #variant97
  | #variant98
  | #variant99
  | #variant100
  | #variant101
  | #variant102
  | #variant103
  | #variant104
  | #variant105
  | #variant106
  | #variant107
  | #variant108
  | #variant109
  | #variant110
  | #variant111
  | #variant112
  | #variant113
  | #variant114
  | #variant115
  | #variant116
  | #variant117
  | #variant118
  | #variant119
  | #variant120
  | #variant121
  | #variant122
  | #variant123
  | #variant124
  | #variant125
  | #variant126
  | #variant127
  | #variant128
  | #variant129
  | #variant130
  | #variant131
  | #variant132
  | #variant133
  | #variant134
  | #variant135
  | #variant136
  | #variant137
  | #variant138
  | #variant139
  | #variant140
  | #variant141
  | #variant142
  | #variant143
  | #variant144
  | #variant145
  | #variant146
  | #variant147
  | #variant148
  | #variant149
  | #variant150
  | #variant151
  | #variant152
  | #variant153
  | #variant154
  | #variant155
  | #variant156
  | #variant157
  | #variant158
  | #variant159
  | #variant160
  | #variant161
  | #variant162
  | #variant163
  | #variant164
  | #variant165
  | #variant166
  | #variant167
  | #variant168
  | #variant169
  | #variant170
  | #variant171
  | #variant172
  | #variant173
  | #variant174
  | #variant175
  | #variant176
  | #variant177
  | #variant178
  | #variant179
  | #variant180
  | #variant181
  | #variant182
  | #variant183
  | #variant184
  | #variant185
  | #variant186
  | #variant187
  | #variant188
  | #variant189
  | #variant190
  | #variant191
  | #variant192
  | #variant193
  | #variant194
  | #variant195
  | #variant196
  | #variant197
  | #variant198
  | #variant199
  | #variant200
  | #variant201
  | #variant202
  | #variant203
  | #variant204
  | #variant205
  | #variant206
  | #variant207
  | #variant208
  | #variant209
  | #variant210
  | #variant211
  | #variant212
  | #variant213
  | #variant214
  | #variant215
  | #variant216
  | #variant217
  | #variant218
  | #variant219
  | #variant220
  | #variant221
  | #variant222
  | #variant223
  | #variant224
  | #variant225
  | #variant226
  | #variant227
  | #variant228
  | #variant229
  | #variant230
  | #variant231
  | #variant232
  | #variant233
  | #variant234
  | #variant235
  | #variant236
  | #variant237
  | #variant238
  | #variant239
  | #variant240
  | #variant241
  | #variant242
  | #variant243
  | #variant244
  | #variant245
  | #variant246
  | #variant247
  | #variant248
  | #variant249
  | #variant250
  | #variant251
  | #variant252
  | #variant253
  | #variant254
  | #variant255
  | #variant256
  | #variant257
  | #variant258
  | #variant259
  | #variant260
  | #variant261
  | #variant262
  | #variant263
  | #variant264
  | #variant265
  | #variant266
  | #variant267
  | #variant268
  | #variant269
  | #variant270
  | #variant271
  | #variant272
  | #variant273
  | #variant274
  | #variant275
  | #variant276
  | #variant277
  | #variant278
  | #variant279
  | #variant280
  | #variant281
  | #variant282
  | #variant283
  | #variant284
  | #variant285
  | #variant286
  | #variant287
  | #variant288
  | #variant289
  | #variant290
  | #variant291
  | #variant292
  | #variant293
  | #variant294
  | #variant295
  | #variant296
  | #variant297
  | #variant298
  | #variant299
]
let eq = (x: option<t>, y: option<t>) =>
  switch x {
  | Some(x) =>
    switch y {
    | None => false
    | Some(y) => x == y
    }
  | None => y == None
  }

assert(tToJs(#variant0) == "variant0")
assert(tToJs(#variant1) == "variant1")
assert(tToJs(#variant2) == "variant2")
assert(tToJs(#variant3) == "variant3")
assert(tToJs(#variant4) == "variant4")
assert(tToJs(#variant5) == "variant5")
assert(tToJs(#variant6) == "variant6")
assert(tToJs(#variant7) == "variant7")
assert(tToJs(#variant8) == "variant8")
assert(tToJs(#variant9) == "variant9")
assert(tToJs(#variant10) == "variant10")
assert(tToJs(#variant11) == "variant11")
assert(tToJs(#variant12) == "variant12")
assert(tToJs(#variant13) == "variant13")
assert(tToJs(#variant14) == "variant14")
assert(tToJs(#variant15) == "variant15")
assert(tToJs(#variant16) == "variant16")
assert(tToJs(#variant17) == "variant17")
assert(tToJs(#variant18) == "variant18")
assert(tToJs(#variant19) == "variant19")
assert(tToJs(#variant20) == "variant20")
assert(tToJs(#variant21) == "variant21")
assert(tToJs(#variant22) == "variant22")
assert(tToJs(#variant23) == "variant23")
assert(tToJs(#variant24) == "variant24")
assert(tToJs(#variant25) == "variant25")
assert(tToJs(#variant26) == "variant26")
assert(tToJs(#variant27) == "variant27")
assert(tToJs(#variant28) == "variant28")
assert(tToJs(#variant29) == "variant29")
assert(tToJs(#variant30) == "variant30")
assert(tToJs(#variant31) == "variant31")
assert(tToJs(#variant32) == "variant32")
assert(tToJs(#variant33) == "variant33")
assert(tToJs(#variant34) == "variant34")
assert(tToJs(#variant35) == "variant35")
assert(tToJs(#variant36) == "variant36")
assert(tToJs(#variant37) == "variant37")
assert(tToJs(#variant38) == "variant38")
assert(tToJs(#variant39) == "variant39")
assert(tToJs(#variant40) == "variant40")
assert(tToJs(#variant41) == "variant41")
assert(tToJs(#variant42) == "variant42")
assert(tToJs(#variant43) == "variant43")
assert(tToJs(#variant44) == "variant44")
assert(tToJs(#variant45) == "variant45")
assert(tToJs(#variant46) == "variant46")
assert(tToJs(#variant47) == "variant47")
assert(tToJs(#variant48) == "variant48")
assert(tToJs(#variant49) == "variant49")
assert(tToJs(#variant50) == "variant50")
assert(tToJs(#variant51) == "variant51")
assert(tToJs(#variant52) == "variant52")
assert(tToJs(#variant53) == "variant53")
assert(tToJs(#variant54) == "variant54")
assert(tToJs(#variant55) == "variant55")
assert(tToJs(#variant56) == "variant56")
assert(tToJs(#variant57) == "variant57")
assert(tToJs(#variant58) == "variant58")
assert(tToJs(#variant59) == "variant59")
assert(tToJs(#variant60) == "variant60")
assert(tToJs(#variant61) == "variant61")
assert(tToJs(#variant62) == "variant62")
assert(tToJs(#variant63) == "variant63")
assert(tToJs(#variant64) == "variant64")
assert(tToJs(#variant65) == "variant65")
assert(tToJs(#variant66) == "variant66")
assert(tToJs(#variant67) == "variant67")
assert(tToJs(#variant68) == "variant68")
assert(tToJs(#variant69) == "variant69")
assert(tToJs(#variant70) == "variant70")
assert(tToJs(#variant71) == "variant71")
assert(tToJs(#variant72) == "variant72")
assert(tToJs(#variant73) == "variant73")
assert(tToJs(#variant74) == "variant74")
assert(tToJs(#variant75) == "variant75")
assert(tToJs(#variant76) == "variant76")
assert(tToJs(#variant77) == "variant77")
assert(tToJs(#variant78) == "variant78")
assert(tToJs(#variant79) == "variant79")
assert(tToJs(#variant80) == "variant80")
assert(tToJs(#variant81) == "variant81")
assert(tToJs(#variant82) == "variant82")
assert(tToJs(#variant83) == "variant83")
assert(tToJs(#variant84) == "variant84")
assert(tToJs(#variant85) == "variant85")
assert(tToJs(#variant86) == "variant86")
assert(tToJs(#variant87) == "variant87")
assert(tToJs(#variant88) == "variant88")
assert(tToJs(#variant89) == "variant89")
assert(tToJs(#variant90) == "variant90")
assert(tToJs(#variant91) == "variant91")
assert(tToJs(#variant92) == "variant92")
assert(tToJs(#variant93) == "variant93")
assert(tToJs(#variant94) == "variant94")
assert(tToJs(#variant95) == "variant95")
assert(tToJs(#variant96) == "variant96")
assert(tToJs(#variant97) == "variant97")
assert(tToJs(#variant98) == "variant98")
assert(tToJs(#variant99) == "variant99")
assert(tToJs(#variant100) == "variant100")
assert(tToJs(#variant101) == "variant101")
assert(tToJs(#variant102) == "variant102")
assert(tToJs(#variant103) == "variant103")
assert(tToJs(#variant104) == "variant104")
assert(tToJs(#variant105) == "variant105")
assert(tToJs(#variant106) == "variant106")
assert(tToJs(#variant107) == "variant107")
assert(tToJs(#variant108) == "variant108")
assert(tToJs(#variant109) == "variant109")
assert(tToJs(#variant110) == "variant110")
assert(tToJs(#variant111) == "variant111")
assert(tToJs(#variant112) == "variant112")
assert(tToJs(#variant113) == "variant113")
assert(tToJs(#variant114) == "variant114")
assert(tToJs(#variant115) == "variant115")
assert(tToJs(#variant116) == "variant116")
assert(tToJs(#variant117) == "variant117")
assert(tToJs(#variant118) == "variant118")
assert(tToJs(#variant119) == "variant119")
assert(tToJs(#variant120) == "variant120")
assert(tToJs(#variant121) == "variant121")
assert(tToJs(#variant122) == "variant122")
assert(tToJs(#variant123) == "variant123")
assert(tToJs(#variant124) == "variant124")
assert(tToJs(#variant125) == "variant125")
assert(tToJs(#variant126) == "variant126")
assert(tToJs(#variant127) == "variant127")
assert(tToJs(#variant128) == "variant128")
assert(tToJs(#variant129) == "variant129")
assert(tToJs(#variant130) == "variant130")
assert(tToJs(#variant131) == "variant131")
assert(tToJs(#variant132) == "variant132")
assert(tToJs(#variant133) == "variant133")
assert(tToJs(#variant134) == "variant134")
assert(tToJs(#variant135) == "variant135")
assert(tToJs(#variant136) == "variant136")
assert(tToJs(#variant137) == "variant137")
assert(tToJs(#variant138) == "variant138")
assert(tToJs(#variant139) == "variant139")
assert(tToJs(#variant140) == "variant140")
assert(tToJs(#variant141) == "variant141")
assert(tToJs(#variant142) == "variant142")
assert(tToJs(#variant143) == "variant143")
assert(tToJs(#variant144) == "variant144")
assert(tToJs(#variant145) == "variant145")
assert(tToJs(#variant146) == "variant146")
assert(tToJs(#variant147) == "variant147")
assert(tToJs(#variant148) == "variant148")
assert(tToJs(#variant149) == "variant149")
assert(tToJs(#variant150) == "variant150")
assert(tToJs(#variant151) == "variant151")
assert(tToJs(#variant152) == "variant152")
assert(tToJs(#variant153) == "variant153")
assert(tToJs(#variant154) == "variant154")
assert(tToJs(#variant155) == "variant155")
assert(tToJs(#variant156) == "variant156")
assert(tToJs(#variant157) == "variant157")
assert(tToJs(#variant158) == "variant158")
assert(tToJs(#variant159) == "variant159")
assert(tToJs(#variant160) == "variant160")
assert(tToJs(#variant161) == "variant161")
assert(tToJs(#variant162) == "variant162")
assert(tToJs(#variant163) == "variant163")
assert(tToJs(#variant164) == "variant164")
assert(tToJs(#variant165) == "variant165")
assert(tToJs(#variant166) == "variant166")
assert(tToJs(#variant167) == "variant167")
assert(tToJs(#variant168) == "variant168")
assert(tToJs(#variant169) == "variant169")
assert(tToJs(#variant170) == "variant170")
assert(tToJs(#variant171) == "variant171")
assert(tToJs(#variant172) == "variant172")
assert(tToJs(#variant173) == "variant173")
assert(tToJs(#variant174) == "variant174")
assert(tToJs(#variant175) == "variant175")
assert(tToJs(#variant176) == "variant176")
assert(tToJs(#variant177) == "variant177")
assert(tToJs(#variant178) == "variant178")
assert(tToJs(#variant179) == "variant179")
assert(tToJs(#variant180) == "variant180")
assert(tToJs(#variant181) == "variant181")
assert(tToJs(#variant182) == "variant182")
assert(tToJs(#variant183) == "variant183")
assert(tToJs(#variant184) == "variant184")
assert(tToJs(#variant185) == "variant185")
assert(tToJs(#variant186) == "variant186")
assert(tToJs(#variant187) == "variant187")
assert(tToJs(#variant188) == "variant188")
assert(tToJs(#variant189) == "variant189")
assert(tToJs(#variant190) == "variant190")
assert(tToJs(#variant191) == "variant191")
assert(tToJs(#variant192) == "variant192")
assert(tToJs(#variant193) == "variant193")
assert(tToJs(#variant194) == "variant194")
assert(tToJs(#variant195) == "variant195")
assert(tToJs(#variant196) == "variant196")
assert(tToJs(#variant197) == "variant197")
assert(tToJs(#variant198) == "variant198")
assert(tToJs(#variant199) == "variant199")
assert(tToJs(#variant200) == "variant200")
assert(tToJs(#variant201) == "variant201")
assert(tToJs(#variant202) == "variant202")
assert(tToJs(#variant203) == "variant203")
assert(tToJs(#variant204) == "variant204")
assert(tToJs(#variant205) == "variant205")
assert(tToJs(#variant206) == "variant206")
assert(tToJs(#variant207) == "variant207")
assert(tToJs(#variant208) == "variant208")
assert(tToJs(#variant209) == "variant209")
assert(tToJs(#variant210) == "variant210")
assert(tToJs(#variant211) == "variant211")
assert(tToJs(#variant212) == "variant212")
assert(tToJs(#variant213) == "variant213")
assert(tToJs(#variant214) == "variant214")
assert(tToJs(#variant215) == "variant215")
assert(tToJs(#variant216) == "variant216")
assert(tToJs(#variant217) == "variant217")
assert(tToJs(#variant218) == "variant218")
assert(tToJs(#variant219) == "variant219")
assert(tToJs(#variant220) == "variant220")
assert(tToJs(#variant221) == "variant221")
assert(tToJs(#variant222) == "variant222")
assert(tToJs(#variant223) == "variant223")
assert(tToJs(#variant224) == "variant224")
assert(tToJs(#variant225) == "variant225")
assert(tToJs(#variant226) == "variant226")
assert(tToJs(#variant227) == "variant227")
assert(tToJs(#variant228) == "variant228")
assert(tToJs(#variant229) == "variant229")
assert(tToJs(#variant230) == "variant230")
assert(tToJs(#variant231) == "variant231")
assert(tToJs(#variant232) == "variant232")
assert(tToJs(#variant233) == "variant233")
assert(tToJs(#variant234) == "variant234")
assert(tToJs(#variant235) == "variant235")
assert(tToJs(#variant236) == "variant236")
assert(tToJs(#variant237) == "variant237")
assert(tToJs(#variant238) == "variant238")
assert(tToJs(#variant239) == "variant239")
assert(tToJs(#variant240) == "variant240")
assert(tToJs(#variant241) == "variant241")
assert(tToJs(#variant242) == "variant242")
assert(tToJs(#variant243) == "variant243")
assert(tToJs(#variant244) == "variant244")
assert(tToJs(#variant245) == "variant245")
assert(tToJs(#variant246) == "variant246")
assert(tToJs(#variant247) == "variant247")
assert(tToJs(#variant248) == "variant248")
assert(tToJs(#variant249) == "variant249")
assert(tToJs(#variant250) == "variant250")
assert(tToJs(#variant251) == "variant251")
assert(tToJs(#variant252) == "variant252")
assert(tToJs(#variant253) == "variant253")
assert(tToJs(#variant254) == "variant254")
assert(tToJs(#variant255) == "variant255")
assert(tToJs(#variant256) == "variant256")
assert(tToJs(#variant257) == "variant257")
assert(tToJs(#variant258) == "variant258")
assert(tToJs(#variant259) == "variant259")
assert(tToJs(#variant260) == "variant260")
assert(tToJs(#variant261) == "variant261")
assert(tToJs(#variant262) == "variant262")
assert(tToJs(#variant263) == "variant263")
assert(tToJs(#variant264) == "variant264")
assert(tToJs(#variant265) == "variant265")
assert(tToJs(#variant266) == "variant266")
assert(tToJs(#variant267) == "variant267")
assert(tToJs(#variant268) == "variant268")
assert(tToJs(#variant269) == "variant269")
assert(tToJs(#variant270) == "variant270")
assert(tToJs(#variant271) == "variant271")
assert(tToJs(#variant272) == "variant272")
assert(tToJs(#variant273) == "variant273")
assert(tToJs(#variant274) == "variant274")
assert(tToJs(#variant275) == "variant275")
assert(tToJs(#variant276) == "variant276")
assert(tToJs(#variant277) == "variant277")
assert(tToJs(#variant278) == "variant278")
assert(tToJs(#variant279) == "variant279")
assert(tToJs(#variant280) == "variant280")
assert(tToJs(#variant281) == "variant281")
assert(tToJs(#variant282) == "variant282")
assert(tToJs(#variant283) == "variant283")
assert(tToJs(#variant284) == "variant284")
assert(tToJs(#variant285) == "variant285")
assert(tToJs(#variant286) == "variant286")
assert(tToJs(#variant287) == "variant287")
assert(tToJs(#variant288) == "variant288")
assert(tToJs(#variant289) == "variant289")
assert(tToJs(#variant290) == "variant290")
assert(tToJs(#variant291) == "variant291")
assert(tToJs(#variant292) == "variant292")
assert(tToJs(#variant293) == "variant293")
assert(tToJs(#variant294) == "variant294")
assert(tToJs(#variant295) == "variant295")
assert(tToJs(#variant296) == "variant296")
assert(tToJs(#variant297) == "variant297")
assert(tToJs(#variant298) == "variant298")
assert(tToJs(#variant299) == "variant299")
assert(eq(tFromJs("variant0"), Some(#variant0)))
assert(eq(tFromJs("variant1"), Some(#variant1)))
assert(eq(tFromJs("variant2"), Some(#variant2)))
assert(eq(tFromJs("variant3"), Some(#variant3)))
assert(eq(tFromJs("variant4"), Some(#variant4)))
assert(eq(tFromJs("variant5"), Some(#variant5)))
assert(eq(tFromJs("variant6"), Some(#variant6)))
assert(eq(tFromJs("variant7"), Some(#variant7)))
assert(eq(tFromJs("variant8"), Some(#variant8)))
assert(eq(tFromJs("variant9"), Some(#variant9)))
assert(eq(tFromJs("variant10"), Some(#variant10)))
assert(eq(tFromJs("variant11"), Some(#variant11)))
assert(eq(tFromJs("variant12"), Some(#variant12)))
assert(eq(tFromJs("variant13"), Some(#variant13)))
assert(eq(tFromJs("variant14"), Some(#variant14)))
assert(eq(tFromJs("variant15"), Some(#variant15)))
assert(eq(tFromJs("variant16"), Some(#variant16)))
assert(eq(tFromJs("variant17"), Some(#variant17)))
assert(eq(tFromJs("variant18"), Some(#variant18)))
assert(eq(tFromJs("variant19"), Some(#variant19)))
assert(eq(tFromJs("variant20"), Some(#variant20)))
assert(eq(tFromJs("variant21"), Some(#variant21)))
assert(eq(tFromJs("variant22"), Some(#variant22)))
assert(eq(tFromJs("variant23"), Some(#variant23)))
assert(eq(tFromJs("variant24"), Some(#variant24)))
assert(eq(tFromJs("variant25"), Some(#variant25)))
assert(eq(tFromJs("variant26"), Some(#variant26)))
assert(eq(tFromJs("variant27"), Some(#variant27)))
assert(eq(tFromJs("variant28"), Some(#variant28)))
assert(eq(tFromJs("variant29"), Some(#variant29)))
assert(eq(tFromJs("variant30"), Some(#variant30)))
assert(eq(tFromJs("variant31"), Some(#variant31)))
assert(eq(tFromJs("variant32"), Some(#variant32)))
assert(eq(tFromJs("variant33"), Some(#variant33)))
assert(eq(tFromJs("variant34"), Some(#variant34)))
assert(eq(tFromJs("variant35"), Some(#variant35)))
assert(eq(tFromJs("variant36"), Some(#variant36)))
assert(eq(tFromJs("variant37"), Some(#variant37)))
assert(eq(tFromJs("variant38"), Some(#variant38)))
assert(eq(tFromJs("variant39"), Some(#variant39)))
assert(eq(tFromJs("variant40"), Some(#variant40)))
assert(eq(tFromJs("variant41"), Some(#variant41)))
assert(eq(tFromJs("variant42"), Some(#variant42)))
assert(eq(tFromJs("variant43"), Some(#variant43)))
assert(eq(tFromJs("variant44"), Some(#variant44)))
assert(eq(tFromJs("variant45"), Some(#variant45)))
assert(eq(tFromJs("variant46"), Some(#variant46)))
assert(eq(tFromJs("variant47"), Some(#variant47)))
assert(eq(tFromJs("variant48"), Some(#variant48)))
assert(eq(tFromJs("variant49"), Some(#variant49)))
assert(eq(tFromJs("variant50"), Some(#variant50)))
assert(eq(tFromJs("variant51"), Some(#variant51)))
assert(eq(tFromJs("variant52"), Some(#variant52)))
assert(eq(tFromJs("variant53"), Some(#variant53)))
assert(eq(tFromJs("variant54"), Some(#variant54)))
assert(eq(tFromJs("variant55"), Some(#variant55)))
assert(eq(tFromJs("variant56"), Some(#variant56)))
assert(eq(tFromJs("variant57"), Some(#variant57)))
assert(eq(tFromJs("variant58"), Some(#variant58)))
assert(eq(tFromJs("variant59"), Some(#variant59)))
assert(eq(tFromJs("variant60"), Some(#variant60)))
assert(eq(tFromJs("variant61"), Some(#variant61)))
assert(eq(tFromJs("variant62"), Some(#variant62)))
assert(eq(tFromJs("variant63"), Some(#variant63)))
assert(eq(tFromJs("variant64"), Some(#variant64)))
assert(eq(tFromJs("variant65"), Some(#variant65)))
assert(eq(tFromJs("variant66"), Some(#variant66)))
assert(eq(tFromJs("variant67"), Some(#variant67)))
assert(eq(tFromJs("variant68"), Some(#variant68)))
assert(eq(tFromJs("variant69"), Some(#variant69)))
assert(eq(tFromJs("variant70"), Some(#variant70)))
assert(eq(tFromJs("variant71"), Some(#variant71)))
assert(eq(tFromJs("variant72"), Some(#variant72)))
assert(eq(tFromJs("variant73"), Some(#variant73)))
assert(eq(tFromJs("variant74"), Some(#variant74)))
assert(eq(tFromJs("variant75"), Some(#variant75)))
assert(eq(tFromJs("variant76"), Some(#variant76)))
assert(eq(tFromJs("variant77"), Some(#variant77)))
assert(eq(tFromJs("variant78"), Some(#variant78)))
assert(eq(tFromJs("variant79"), Some(#variant79)))
assert(eq(tFromJs("variant80"), Some(#variant80)))
assert(eq(tFromJs("variant81"), Some(#variant81)))
assert(eq(tFromJs("variant82"), Some(#variant82)))
assert(eq(tFromJs("variant83"), Some(#variant83)))
assert(eq(tFromJs("variant84"), Some(#variant84)))
assert(eq(tFromJs("variant85"), Some(#variant85)))
assert(eq(tFromJs("variant86"), Some(#variant86)))
assert(eq(tFromJs("variant87"), Some(#variant87)))
assert(eq(tFromJs("variant88"), Some(#variant88)))
assert(eq(tFromJs("variant89"), Some(#variant89)))
assert(eq(tFromJs("variant90"), Some(#variant90)))
assert(eq(tFromJs("variant91"), Some(#variant91)))
assert(eq(tFromJs("variant92"), Some(#variant92)))
assert(eq(tFromJs("variant93"), Some(#variant93)))
assert(eq(tFromJs("variant94"), Some(#variant94)))
assert(eq(tFromJs("variant95"), Some(#variant95)))
assert(eq(tFromJs("variant96"), Some(#variant96)))
assert(eq(tFromJs("variant97"), Some(#variant97)))
assert(eq(tFromJs("variant98"), Some(#variant98)))
assert(eq(tFromJs("variant99"), Some(#variant99)))
assert(eq(tFromJs("variant100"), Some(#variant100)))
assert(eq(tFromJs("variant101"), Some(#variant101)))
assert(eq(tFromJs("variant102"), Some(#variant102)))
assert(eq(tFromJs("variant103"), Some(#variant103)))
assert(eq(tFromJs("variant104"), Some(#variant104)))
assert(eq(tFromJs("variant105"), Some(#variant105)))
assert(eq(tFromJs("variant106"), Some(#variant106)))
assert(eq(tFromJs("variant107"), Some(#variant107)))
assert(eq(tFromJs("variant108"), Some(#variant108)))
assert(eq(tFromJs("variant109"), Some(#variant109)))
assert(eq(tFromJs("variant110"), Some(#variant110)))
assert(eq(tFromJs("variant111"), Some(#variant111)))
assert(eq(tFromJs("variant112"), Some(#variant112)))
assert(eq(tFromJs("variant113"), Some(#variant113)))
assert(eq(tFromJs("variant114"), Some(#variant114)))
assert(eq(tFromJs("variant115"), Some(#variant115)))
assert(eq(tFromJs("variant116"), Some(#variant116)))
assert(eq(tFromJs("variant117"), Some(#variant117)))
assert(eq(tFromJs("variant118"), Some(#variant118)))
assert(eq(tFromJs("variant119"), Some(#variant119)))
assert(eq(tFromJs("variant120"), Some(#variant120)))
assert(eq(tFromJs("variant121"), Some(#variant121)))
assert(eq(tFromJs("variant122"), Some(#variant122)))
assert(eq(tFromJs("variant123"), Some(#variant123)))
assert(eq(tFromJs("variant124"), Some(#variant124)))
assert(eq(tFromJs("variant125"), Some(#variant125)))
assert(eq(tFromJs("variant126"), Some(#variant126)))
assert(eq(tFromJs("variant127"), Some(#variant127)))
assert(eq(tFromJs("variant128"), Some(#variant128)))
assert(eq(tFromJs("variant129"), Some(#variant129)))
assert(eq(tFromJs("variant130"), Some(#variant130)))
assert(eq(tFromJs("variant131"), Some(#variant131)))
assert(eq(tFromJs("variant132"), Some(#variant132)))
assert(eq(tFromJs("variant133"), Some(#variant133)))
assert(eq(tFromJs("variant134"), Some(#variant134)))
assert(eq(tFromJs("variant135"), Some(#variant135)))
assert(eq(tFromJs("variant136"), Some(#variant136)))
assert(eq(tFromJs("variant137"), Some(#variant137)))
assert(eq(tFromJs("variant138"), Some(#variant138)))
assert(eq(tFromJs("variant139"), Some(#variant139)))
assert(eq(tFromJs("variant140"), Some(#variant140)))
assert(eq(tFromJs("variant141"), Some(#variant141)))
assert(eq(tFromJs("variant142"), Some(#variant142)))
assert(eq(tFromJs("variant143"), Some(#variant143)))
assert(eq(tFromJs("variant144"), Some(#variant144)))
assert(eq(tFromJs("variant145"), Some(#variant145)))
assert(eq(tFromJs("variant146"), Some(#variant146)))
assert(eq(tFromJs("variant147"), Some(#variant147)))
assert(eq(tFromJs("variant148"), Some(#variant148)))
assert(eq(tFromJs("variant149"), Some(#variant149)))
assert(eq(tFromJs("variant150"), Some(#variant150)))
assert(eq(tFromJs("variant151"), Some(#variant151)))
assert(eq(tFromJs("variant152"), Some(#variant152)))
assert(eq(tFromJs("variant153"), Some(#variant153)))
assert(eq(tFromJs("variant154"), Some(#variant154)))
assert(eq(tFromJs("variant155"), Some(#variant155)))
assert(eq(tFromJs("variant156"), Some(#variant156)))
assert(eq(tFromJs("variant157"), Some(#variant157)))
assert(eq(tFromJs("variant158"), Some(#variant158)))
assert(eq(tFromJs("variant159"), Some(#variant159)))
assert(eq(tFromJs("variant160"), Some(#variant160)))
assert(eq(tFromJs("variant161"), Some(#variant161)))
assert(eq(tFromJs("variant162"), Some(#variant162)))
assert(eq(tFromJs("variant163"), Some(#variant163)))
assert(eq(tFromJs("variant164"), Some(#variant164)))
assert(eq(tFromJs("variant165"), Some(#variant165)))
assert(eq(tFromJs("variant166"), Some(#variant166)))
assert(eq(tFromJs("variant167"), Some(#variant167)))
assert(eq(tFromJs("variant168"), Some(#variant168)))
assert(eq(tFromJs("variant169"), Some(#variant169)))
assert(eq(tFromJs("variant170"), Some(#variant170)))
assert(eq(tFromJs("variant171"), Some(#variant171)))
assert(eq(tFromJs("variant172"), Some(#variant172)))
assert(eq(tFromJs("variant173"), Some(#variant173)))
assert(eq(tFromJs("variant174"), Some(#variant174)))
assert(eq(tFromJs("variant175"), Some(#variant175)))
assert(eq(tFromJs("variant176"), Some(#variant176)))
assert(eq(tFromJs("variant177"), Some(#variant177)))
assert(eq(tFromJs("variant178"), Some(#variant178)))
assert(eq(tFromJs("variant179"), Some(#variant179)))
assert(eq(tFromJs("variant180"), Some(#variant180)))
assert(eq(tFromJs("variant181"), Some(#variant181)))
assert(eq(tFromJs("variant182"), Some(#variant182)))
assert(eq(tFromJs("variant183"), Some(#variant183)))
assert(eq(tFromJs("variant184"), Some(#variant184)))
assert(eq(tFromJs("variant185"), Some(#variant185)))
assert(eq(tFromJs("variant186"), Some(#variant186)))
assert(eq(tFromJs("variant187"), Some(#variant187)))
assert(eq(tFromJs("variant188"), Some(#variant188)))
assert(eq(tFromJs("variant189"), Some(#variant189)))
assert(eq(tFromJs("variant190"), Some(#variant190)))
assert(eq(tFromJs("variant191"), Some(#variant191)))
assert(eq(tFromJs("variant192"), Some(#variant192)))
assert(eq(tFromJs("variant193"), Some(#variant193)))
assert(eq(tFromJs("variant194"), Some(#variant194)))
assert(eq(tFromJs("variant195"), Some(#variant195)))
assert(eq(tFromJs("variant196"), Some(#variant196)))
assert(eq(tFromJs("variant197"), Some(#variant197)))
assert(eq(tFromJs("variant198"), Some(#variant198)))
assert(eq(tFromJs("variant199"), Some(#variant199)))
assert(eq(tFromJs("variant200"), Some(#variant200)))
assert(eq(tFromJs("variant201"), Some(#variant201)))
assert(eq(tFromJs("variant202"), Some(#variant202)))
assert(eq(tFromJs("variant203"), Some(#variant203)))
assert(eq(tFromJs("variant204"), Some(#variant204)))
assert(eq(tFromJs("variant205"), Some(#variant205)))
assert(eq(tFromJs("variant206"), Some(#variant206)))
assert(eq(tFromJs("variant207"), Some(#variant207)))
assert(eq(tFromJs("variant208"), Some(#variant208)))
assert(eq(tFromJs("variant209"), Some(#variant209)))
assert(eq(tFromJs("variant210"), Some(#variant210)))
assert(eq(tFromJs("variant211"), Some(#variant211)))
assert(eq(tFromJs("variant212"), Some(#variant212)))
assert(eq(tFromJs("variant213"), Some(#variant213)))
assert(eq(tFromJs("variant214"), Some(#variant214)))
assert(eq(tFromJs("variant215"), Some(#variant215)))
assert(eq(tFromJs("variant216"), Some(#variant216)))
assert(eq(tFromJs("variant217"), Some(#variant217)))
assert(eq(tFromJs("variant218"), Some(#variant218)))
assert(eq(tFromJs("variant219"), Some(#variant219)))
assert(eq(tFromJs("variant220"), Some(#variant220)))
assert(eq(tFromJs("variant221"), Some(#variant221)))
assert(eq(tFromJs("variant222"), Some(#variant222)))
assert(eq(tFromJs("variant223"), Some(#variant223)))
assert(eq(tFromJs("variant224"), Some(#variant224)))
assert(eq(tFromJs("variant225"), Some(#variant225)))
assert(eq(tFromJs("variant226"), Some(#variant226)))
assert(eq(tFromJs("variant227"), Some(#variant227)))
assert(eq(tFromJs("variant228"), Some(#variant228)))
assert(eq(tFromJs("variant229"), Some(#variant229)))
assert(eq(tFromJs("variant230"), Some(#variant230)))
assert(eq(tFromJs("variant231"), Some(#variant231)))
assert(eq(tFromJs("variant232"), Some(#variant232)))
assert(eq(tFromJs("variant233"), Some(#variant233)))
assert(eq(tFromJs("variant234"), Some(#variant234)))
assert(eq(tFromJs("variant235"), Some(#variant235)))
assert(eq(tFromJs("variant236"), Some(#variant236)))
assert(eq(tFromJs("variant237"), Some(#variant237)))
assert(eq(tFromJs("variant238"), Some(#variant238)))
assert(eq(tFromJs("variant239"), Some(#variant239)))
assert(eq(tFromJs("variant240"), Some(#variant240)))
assert(eq(tFromJs("variant241"), Some(#variant241)))
assert(eq(tFromJs("variant242"), Some(#variant242)))
assert(eq(tFromJs("variant243"), Some(#variant243)))
assert(eq(tFromJs("variant244"), Some(#variant244)))
assert(eq(tFromJs("variant245"), Some(#variant245)))
assert(eq(tFromJs("variant246"), Some(#variant246)))
assert(eq(tFromJs("variant247"), Some(#variant247)))
assert(eq(tFromJs("variant248"), Some(#variant248)))
assert(eq(tFromJs("variant249"), Some(#variant249)))
assert(eq(tFromJs("variant250"), Some(#variant250)))
assert(eq(tFromJs("variant251"), Some(#variant251)))
assert(eq(tFromJs("variant252"), Some(#variant252)))
assert(eq(tFromJs("variant253"), Some(#variant253)))
assert(eq(tFromJs("variant254"), Some(#variant254)))
assert(eq(tFromJs("variant255"), Some(#variant255)))
assert(eq(tFromJs("variant256"), Some(#variant256)))
assert(eq(tFromJs("variant257"), Some(#variant257)))
assert(eq(tFromJs("variant258"), Some(#variant258)))
assert(eq(tFromJs("variant259"), Some(#variant259)))
assert(eq(tFromJs("variant260"), Some(#variant260)))
assert(eq(tFromJs("variant261"), Some(#variant261)))
assert(eq(tFromJs("variant262"), Some(#variant262)))
assert(eq(tFromJs("variant263"), Some(#variant263)))
assert(eq(tFromJs("variant264"), Some(#variant264)))
assert(eq(tFromJs("variant265"), Some(#variant265)))
assert(eq(tFromJs("variant266"), Some(#variant266)))
assert(eq(tFromJs("variant267"), Some(#variant267)))
assert(eq(tFromJs("variant268"), Some(#variant268)))
assert(eq(tFromJs("variant269"), Some(#variant269)))
assert(eq(tFromJs("variant270"), Some(#variant270)))
assert(eq(tFromJs("variant271"), Some(#variant271)))
assert(eq(tFromJs("variant272"), Some(#variant272)))
assert(eq(tFromJs("variant273"), Some(#variant273)))
assert(eq(tFromJs("variant274"), Some(#variant274)))
assert(eq(tFromJs("variant275"), Some(#variant275)))
assert(eq(tFromJs("variant276"), Some(#variant276)))
assert(eq(tFromJs("variant277"), Some(#variant277)))
assert(eq(tFromJs("variant278"), Some(#variant278)))
assert(eq(tFromJs("variant279"), Some(#variant279)))
assert(eq(tFromJs("variant280"), Some(#variant280)))
assert(eq(tFromJs("variant281"), Some(#variant281)))
assert(eq(tFromJs("variant282"), Some(#variant282)))
assert(eq(tFromJs("variant283"), Some(#variant283)))
assert(eq(tFromJs("variant284"), Some(#variant284)))
assert(eq(tFromJs("variant285"), Some(#variant285)))
assert(eq(tFromJs("variant286"), Some(#variant286)))
assert(eq(tFromJs("variant287"), Some(#variant287)))
assert(eq(tFromJs("variant288"), Some(#variant288)))
assert(eq(tFromJs("variant289"), Some(#variant289)))
assert(eq(tFromJs("variant290"), Some(#variant290)))
assert(eq(tFromJs("variant291"), Some(#variant291)))
assert(eq(tFromJs("variant292"), Some(#variant292)))
assert(eq(tFromJs("variant293"), Some(#variant293)))
assert(eq(tFromJs("variant294"), Some(#variant294)))
assert(eq(tFromJs("variant295"), Some(#variant295)))
assert(eq(tFromJs("variant296"), Some(#variant296)))
assert(eq(tFromJs("variant297"), Some(#variant297)))
assert(eq(tFromJs("variant298"), Some(#variant298)))
assert(eq(tFromJs("variant299"), Some(#variant299)))
assert(eq(tFromJs("xx"), None))
