/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Luc Maranget, projet Gallium, INRIA Rocquencourt */
/*  */
/* Copyright 2014 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the Q Public License version 1.0. */
/*  */
/* ********************************************************************* */

/* Empty string oddities */

let rec tst01 = s =>
  switch s {
  | "" => 0
  | _ => 1
  }

let () = {
  assert(tst01("") == 0)
  assert(tst01("\x00\x00\x00\x03") == 1)
  assert(tst01("\x00\x00\x00\x00\x00\x00\x00\x07") == 1)
  ()
}

/* A few when clauses */

let tst02 = s => {
  let len = String.length(s)
  switch s {
  | "" if len < 0 => assert(false)
  | "" => 1
  | _ if len == 0 => assert(false)
  | "A" => 2
  | _ => 3
  }
}

let () = {
  assert(tst02("") == 1)
  assert(tst02("A") == 2)
  assert(tst02("B") == 3)
  assert(tst02("\x00\x00\x00\x00\x00\x00\x00\x07") == 3)
  assert(tst02("\x00\x00\x00\x03") == 3)
  ()
}

/* Keword reckognition */

let s00 = "get_const"
let t00 = "set_congt"
let s01 = "get_var"
let t01 = "gat_ver"
let s02 = "get_env"
let t02 = "get_env"
let s03 = "get_meth"
let t03 = "met_geth"
let s04 = "set_var"
let t04 = "sev_tar"
let s05 = "app_const"
let t05 = "ppa_const"
let s06 = "app_var"
let t06 = "app_var"
let s07 = "app_env"
let t07 = "epp_anv"
let s08 = "app_meth"
let t08 = "atp_meph"
let s09 = "app_const_const"
let t09 = "app_const_const"
let s10 = "app_const_var"
let t10 = "atp_consp_var"
let s11 = "app_const_env"
let t11 = "app_constne_v"
let s12 = "app_const_meth"
let t12 = "spp_conat_meth"
let s13 = "app_var_const"
let t13 = "app_va_rconst"
let s14 = "app_env_const"
let t14 = "app_env_const"
let s15 = "app_meth_const"
let t15 = "app_teth_consm"
let s16 = "meth_app_const"
let t16 = "math_epp_const"
let s17 = "meth_app_var"
let t17 = "meth_app_var"
let s18 = "meth_app_env"
let t18 = "eeth_app_mnv"
let s19 = "meth_app_meth"
let t19 = "meth_apt_meph"
let s20 = "send_const"
let t20 = "tend_conss"
let s21 = "send_var"
let t21 = "serd_van"
let s22 = "send_env"
let t22 = "sen_denv"
let s23 = "send_meth"
let t23 = "tend_mesh"

let tst03 = s =>
  switch s {
  | "get_const" => 0
  | "get_var" => 1
  | "get_env" => 2
  | "get_meth" => 3
  | "set_var" => 4
  | "app_const" => 5
  | "app_var" => 6
  | "app_env" => 7
  | "app_meth" => 8
  | "app_const_const" => 9
  | "app_const_var" => 10
  | "app_const_env" => 11
  | "app_const_meth" => 12
  | "app_var_const" => 13
  | "app_env_const" => 14
  | "app_meth_const" => 15
  | "meth_app_const" => 16
  | "meth_app_var" => 17
  | "meth_app_env" => 18
  | "meth_app_meth" => 19
  | "send_const" => 20
  | "send_var" => 21
  | "send_env" => 22
  | "send_meth" => 23
  | _ => -1
  }

let () = {
  assert(tst03(s00) == 0)
  assert(tst03(t00) == -1)
  assert(tst03(s01) == 1)
  assert(tst03(t01) == -1)
  assert(tst03(s02) == 2)
  assert(tst03(t02) == 2)
  assert(tst03(s03) == 3)
  assert(tst03(t03) == -1)
  assert(tst03(s04) == 4)
  assert(tst03(t04) == -1)
  assert(tst03(s05) == 5)
  assert(tst03(t05) == -1)
  assert(tst03(s06) == 6)
  assert(tst03(t06) == 6)
  assert(tst03(s07) == 7)
  assert(tst03(t07) == -1)
  assert(tst03(s08) == 8)
  assert(tst03(t08) == -1)
  assert(tst03(s09) == 9)
  assert(tst03(t09) == 9)
  assert(tst03(s10) == 10)
  assert(tst03(t10) == -1)
  assert(tst03(s11) == 11)
  assert(tst03(t11) == -1)
  assert(tst03(s12) == 12)
  assert(tst03(t12) == -1)
  assert(tst03(s13) == 13)
  assert(tst03(t13) == -1)
  assert(tst03(s14) == 14)
  assert(tst03(t14) == 14)
  assert(tst03(s15) == 15)
  assert(tst03(t15) == -1)
  assert(tst03(s16) == 16)
  assert(tst03(t16) == -1)
  assert(tst03(s17) == 17)
  assert(tst03(t17) == 17)
  assert(tst03(s18) == 18)
  assert(tst03(t18) == -1)
  assert(tst03(s19) == 19)
  assert(tst03(t19) == -1)
  assert(tst03(s20) == 20)
  assert(tst03(t20) == -1)
  assert(tst03(s21) == 21)
  assert(tst03(t21) == -1)
  assert(tst03(s22) == 22)
  assert(tst03(t22) == -1)
  assert(tst03(s23) == 23)
  assert(tst03(t23) == -1)
  ()
}

/* Activate the test first column first heuristics */

let s00 = "AAAAAAAA"
let s01 = "AAAAAAAAAAAAAAAA"
let s02 = "AAAAAAAAAAAAAAAAAAAAAAAA"
let s03 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s04 = "BBBBBBBB"
let s05 = "BBBBBBBBBBBBBBBB"
let s06 = "BBBBBBBBBBBBBBBBBBBBBBBB"
let s07 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
let s08 = "CCCCCCCC"
let s09 = "CCCCCCCCCCCCCCCC"
let s10 = "CCCCCCCCCCCCCCCCCCCCCCCC"
let s11 = "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"

let tst04 = s =>
  switch s {
  | "AAAAAAAA" => 0
  | "AAAAAAAAAAAAAAAA" => 1
  | "AAAAAAAAAAAAAAAAAAAAAAAA" => 2
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" => 3
  | "BBBBBBBB" => 4
  | "BBBBBBBBBBBBBBBB" => 5
  | "BBBBBBBBBBBBBBBBBBBBBBBB" => 6
  | "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" => 7
  | "CCCCCCCC" => 8
  | "CCCCCCCCCCCCCCCC" => 9
  | "CCCCCCCCCCCCCCCCCCCCCCCC" => 10
  | "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC" => 11
  | _ => -1
  }

let () = {
  assert(tst04(s00) == 0)
  assert(tst04(s01) == 1)
  assert(tst04(s02) == 2)
  assert(tst04(s03) == 3)
  assert(tst04(s04) == 4)
  assert(tst04(s05) == 5)
  assert(tst04(s06) == 6)
  assert(tst04(s07) == 7)
  assert(tst04(s08) == 8)
  assert(tst04(s09) == 9)
  assert(tst04(s10) == 10)
  assert(tst04(s11) == 11)
  assert(tst04("") == -1)
  assert(tst04("DDD") == -1)
  assert(tst04("DDDDDDD") == -1)
  assert(tst04("AAADDDD") == -1)
  assert(tst04("AAAAAAADDDDDDDD") == -1)
  assert(tst04("AAAAAAADDDD") == -1)
  assert(tst04("AAAAAAAAAAAAAAADDDD") == -1)
  ()
}

/* Similar */

let s00 = "AAA"
let s01 = "AAAA"
let s02 = "AAAAA"
let s03 = "AAAAAA"
let s04 = "AAAAAAA"
let s05 = "AAAAAAAAAAAA"
let s06 = "AAAAAAAAAAAAAAAA"
let s07 = "AAAAAAAAAAAAAAAAAAAA"
let s08 = "BBB"
let s09 = "BBBB"
let s10 = "BBBBB"
let s11 = "BBBBBB"
let s12 = "BBBBBBB"

let tst05 = s =>
  switch s {
  | "AAA" => 0
  | "AAAA" => 1
  | "AAAAA" => 2
  | "AAAAAA" => 3
  | "AAAAAAA" => 4
  | "AAAAAAAAAAAA" => 5
  | "AAAAAAAAAAAAAAAA" => 6
  | "AAAAAAAAAAAAAAAAAAAA" => 7
  | "BBB" => 8
  | "BBBB" => 9
  | "BBBBB" => 10
  | "BBBBBB" => 11
  | "BBBBBBB" => 12
  | _ => -1
  }

let () = {
  assert(tst05(s00) == 0)
  assert(tst05(s01) == 1)
  assert(tst05(s02) == 2)
  assert(tst05(s03) == 3)
  assert(tst05(s04) == 4)
  assert(tst05(s05) == 5)
  assert(tst05(s06) == 6)
  assert(tst05(s07) == 7)
  assert(tst05(s08) == 8)
  assert(tst05(s09) == 9)
  assert(tst05(s10) == 10)
  assert(tst05(s11) == 11)
  assert(tst05(s12) == 12)
  assert(tst05("") == -1)
  assert(tst05("AAD") == -1)
  assert(tst05("AAAD") == -1)
  assert(tst05("AAAAAAD") == -1)
  assert(tst05("AAAAAAAD") == -1)
  assert(tst05("BBD") == -1)
  assert(tst05("BBBD") == -1)
  assert(tst05("BBBBBBD") == -1)
  assert(tst05("BBBBBBBD") == -1)
  ()
}

/* Big test */

let s00 = "and"
let t00 = "nad"
let s01 = "as"
let t01 = "sa"
let s02 = "assert"
let t02 = "asesrt"
let s03 = "begin"
let t03 = "negib"
let s04 = "class"
let t04 = "lcass"
let s05 = "constraint"
let t05 = "constiarnt"
let s06 = "do"
let t06 = "od"
let s07 = "done"
let t07 = "eond"
let s08 = "downto"
let t08 = "dowtno"
let s09 = "else"
let t09 = "lese"
let s10 = "end"
let t10 = "edn"
let s11 = "exception"
let t11 = "exception"
let s12 = "external"
let t12 = "external"
let s13 = "false"
let t13 = "fslae"
let s14 = "for"
let t14 = "ofr"
let s15 = "fun"
let t15 = "fnu"
let s16 = "function"
let t16 = "function"
let s17 = "functor"
let t17 = "ounctfr"
let s18 = "if"
let t18 = "fi"
let s19 = "in"
let t19 = "in"
let s20 = "include"
let t20 = "inculde"
let s21 = "inherit"
let t21 = "iehnrit"
let s22 = "initializer"
let t22 = "enitializir"
let s23 = "lazy"
let t23 = "zaly"
let s24 = "let"
let t24 = "elt"
let s25 = "match"
let t25 = "match"
let s26 = "method"
let t26 = "methdo"
let s27 = "module"
let t27 = "modelu"
let s28 = "mutable"
let t28 = "butamle"
let s29 = "new"
let t29 = "wen"
let s30 = "object"
let t30 = "objcet"
let s31 = "of"
let t31 = "of"
let s32 = "open"
let t32 = "epon"
let s33 = "or"
let t33 = "ro"
let s34 = "private"
let t34 = "privaet"
let s35 = "rec"
let t35 = "rec"
let s36 = "sig"
let t36 = "gis"
let s37 = "struct"
let t37 = "scrutt"
let s38 = "then"
let t38 = "hten"
let s39 = "to"
let t39 = "to"
let s40 = "true"
let t40 = "teur"
let s41 = "try"
let t41 = "try"
let s42 = "type"
let t42 = "pyte"
let s43 = "val"
let t43 = "val"
let s44 = "virtual"
let t44 = "vritual"
let s45 = "when"
let t45 = "whne"
let s46 = "while"
let t46 = "wlihe"
let s47 = "with"
let t47 = "iwth"
let s48 = "mod"
let t48 = "mod"
let s49 = "land"
let t49 = "alnd"
let s50 = "lor"
let t50 = "rol"
let s51 = "lxor"
let t51 = "lxor"
let s52 = "lsl"
let t52 = "lsl"
let s53 = "lsr"
let t53 = "lsr"
let s54 = "asr"
let t54 = "sar"
let s55 = "A"
let t55 = "A"
let s56 = "AA"
let t56 = "AA"
let s57 = "AAA"
let t57 = "AAA"
let s58 = "AAAA"
let t58 = "AAAA"
let s59 = "AAAAA"
let t59 = "AAAAA"
let s60 = "AAAAAA"
let t60 = "AAAAAA"
let s61 = "AAAAAAA"
let t61 = "AAAAAAA"
let s62 = "AAAAAAAA"
let t62 = "AAAAAAAA"
let s63 = "AAAAAAAAA"
let t63 = "AAAAAAAAA"
let s64 = "AAAAAAAAAA"
let t64 = "AAAAAAAAAA"
let s65 = "AAAAAAAAAAA"
let t65 = "AAAAAAAAAAA"
let s66 = "AAAAAAAAAAAA"
let t66 = "AAAAAAAAAAAA"
let s67 = "AAAAAAAAAAAAA"
let t67 = "AAAAAAAAAAAAA"
let s68 = "AAAAAAAAAAAAAA"
let t68 = "AAAAAAAAAAAAAA"
let s69 = "AAAAAAAAAAAAAAA"
let t69 = "AAAAAAAAAAAAAAA"
let s70 = "AAAAAAAAAAAAAAAA"
let t70 = "AAAAAAAAAAAAAAAA"
let s71 = "AAAAAAAAAAAAAAAAA"
let t71 = "AAAAAAAAAAAAAAAAA"
let s72 = "AAAAAAAAAAAAAAAAAA"
let t72 = "AAAAAAAAAAAAAAAAAA"
let s73 = "AAAAAAAAAAAAAAAAAAA"
let t73 = "AAAAAAAAAAAAAAAAAAA"
let s74 = "AAAAAAAAAAAAAAAAAAAA"
let t74 = "AAAAAAAAAAAAAAAAAAAA"
let s75 = "AAAAAAAAAAAAAAAAAAAAA"
let t75 = "AAAAAAAAAAAAAAAAAAAAA"
let s76 = "AAAAAAAAAAAAAAAAAAAAAA"
let t76 = "AAAAAAAAAAAAAAAAAAAAAA"
let s77 = "AAAAAAAAAAAAAAAAAAAAAAA"
let t77 = "AAAAAAAAAAAAAAAAAAAAAAA"
let s78 = "AAAAAAAAAAAAAAAAAAAAAAAA"
let t78 = "AAAAAAAAAAAAAAAAAAAAAAAA"
let s79 = "AAAAAAAAAAAAAAAAAAAAAAAAA"
let t79 = "AAAAAAAAAAAAAAAAAAAAAAAAA"
let s80 = "AAAAAAAAAAAAAAAAAAAAAAAAAA"
let t80 = "AAAAAAAAAAAAAAAAAAAAAAAAAA"
let s81 = "AAAAAAAAAAAAAAAAAAAAAAAAAAA"
let t81 = "AAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s82 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let t82 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s83 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let t83 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s84 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let t84 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s85 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let t85 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s86 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let t86 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s87 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let t87 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s88 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let t88 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
let s89 = "BBBBBBBBBBBBBBB"
let t89 = "BBBBBBBBBBBBBBB"
let s90 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
let t90 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
let s91 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
let t91 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"

let tst06 = s =>
  switch s {
  | "and" => 0
  | "as" => 1
  | "assert" => 2
  | "begin" => 3
  | "class" => 4
  | "constraint" => 5
  | "do" => 6
  | "done" => 7
  | "downto" => 8
  | "else" => 9
  | "end" => 10
  | "exception" => 11
  | "external" => 12
  | "false" => 13
  | "for" => 14
  | "fun" => 15
  | "function" => 16
  | "functor" => 17
  | "if" => 18
  | "in" => 19
  | "include" => 20
  | "inherit" => 21
  | "initializer" => 22
  | "lazy" => 23
  | "let" => 24
  | "match" => 25
  | "method" => 26
  | "module" => 27
  | "mutable" => 28
  | "new" => 29
  | "object" => 30
  | "of" => 31
  | "open" => 32
  | "or" => 33
  | "private" => 34
  | "rec" => 35
  | "sig" => 36
  | "struct" => 37
  | "then" => 38
  | "to" => 39
  | "true" => 40
  | "try" => 41
  | "type" => 42
  | "val" => 43
  | "virtual" => 44
  | "when" => 45
  | "while" => 46
  | "with" => 47
  | "mod" => 48
  | "land" => 49
  | "lor" => 50
  | "lxor" => 51
  | "lsl" => 52
  | "lsr" => 53
  | "asr" => 54
  | "A" => 55
  | "AA" => 56
  | "AAA" => 57
  | "AAAA" => 58
  | "AAAAA" => 59
  | "AAAAAA" => 60
  | "AAAAAAA" => 61
  | "AAAAAAAA" => 62
  | "AAAAAAAAA" => 63
  | "AAAAAAAAAA" => 64
  | "AAAAAAAAAAA" => 65
  | "AAAAAAAAAAAA" => 66
  | "AAAAAAAAAAAAA" => 67
  | "AAAAAAAAAAAAAA" => 68
  | "AAAAAAAAAAAAAAA" => 69
  | "AAAAAAAAAAAAAAAA" => 70
  | "AAAAAAAAAAAAAAAAA" => 71
  | "AAAAAAAAAAAAAAAAAA" => 72
  | "AAAAAAAAAAAAAAAAAAA" => 73
  | "AAAAAAAAAAAAAAAAAAAA" => 74
  | "AAAAAAAAAAAAAAAAAAAAA" => 75
  | "AAAAAAAAAAAAAAAAAAAAAA" => 76
  | "AAAAAAAAAAAAAAAAAAAAAAA" => 77
  | "AAAAAAAAAAAAAAAAAAAAAAAA" => 78
  | "AAAAAAAAAAAAAAAAAAAAAAAAA" => 79
  | "AAAAAAAAAAAAAAAAAAAAAAAAAA" => 80
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAA" => 81
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAAA" => 82
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA" => 83
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" => 84
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" => 85
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" => 86
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" => 87
  | "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" => 88
  | "BBBBBBBBBBBBBBB" => 89
  | "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" => 90
  | "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" => 91
  | _ => -1
  }

let () = {
  assert(tst06(s00) == 0)
  assert(tst06(t00) == -1)
  assert(tst06(s01) == 1)
  assert(tst06(t01) == -1)
  assert(tst06(s02) == 2)
  assert(tst06(t02) == -1)
  assert(tst06(s03) == 3)
  assert(tst06(t03) == -1)
  assert(tst06(s04) == 4)
  assert(tst06(t04) == -1)
  assert(tst06(s05) == 5)
  assert(tst06(t05) == -1)
  assert(tst06(s06) == 6)
  assert(tst06(t06) == -1)
  assert(tst06(s07) == 7)
  assert(tst06(t07) == -1)
  assert(tst06(s08) == 8)
  assert(tst06(t08) == -1)
  assert(tst06(s09) == 9)
  assert(tst06(t09) == -1)
  assert(tst06(s10) == 10)
  assert(tst06(t10) == -1)
  assert(tst06(s11) == 11)
  assert(tst06(t11) == 11)
  assert(tst06(s12) == 12)
  assert(tst06(t12) == 12)
  assert(tst06(s13) == 13)
  assert(tst06(t13) == -1)
  assert(tst06(s14) == 14)
  assert(tst06(t14) == -1)
  assert(tst06(s15) == 15)
  assert(tst06(t15) == -1)
  assert(tst06(s16) == 16)
  assert(tst06(t16) == 16)
  assert(tst06(s17) == 17)
  assert(tst06(t17) == -1)
  assert(tst06(s18) == 18)
  assert(tst06(t18) == -1)
  assert(tst06(s19) == 19)
  assert(tst06(t19) == 19)
  assert(tst06(s20) == 20)
  assert(tst06(t20) == -1)
  assert(tst06(s21) == 21)
  assert(tst06(t21) == -1)
  assert(tst06(s22) == 22)
  assert(tst06(t22) == -1)
  assert(tst06(s23) == 23)
  assert(tst06(t23) == -1)
  assert(tst06(s24) == 24)
  assert(tst06(t24) == -1)
  assert(tst06(s25) == 25)
  assert(tst06(t25) == 25)
  assert(tst06(s26) == 26)
  assert(tst06(t26) == -1)
  assert(tst06(s27) == 27)
  assert(tst06(t27) == -1)
  assert(tst06(s28) == 28)
  assert(tst06(t28) == -1)
  assert(tst06(s29) == 29)
  assert(tst06(t29) == -1)
  assert(tst06(s30) == 30)
  assert(tst06(t30) == -1)
  assert(tst06(s31) == 31)
  assert(tst06(t31) == 31)
  assert(tst06(s32) == 32)
  assert(tst06(t32) == -1)
  assert(tst06(s33) == 33)
  assert(tst06(t33) == -1)
  assert(tst06(s34) == 34)
  assert(tst06(t34) == -1)
  assert(tst06(s35) == 35)
  assert(tst06(t35) == 35)
  assert(tst06(s36) == 36)
  assert(tst06(t36) == -1)
  assert(tst06(s37) == 37)
  assert(tst06(t37) == -1)
  assert(tst06(s38) == 38)
  assert(tst06(t38) == -1)
  assert(tst06(s39) == 39)
  assert(tst06(t39) == 39)
  assert(tst06(s40) == 40)
  assert(tst06(t40) == -1)
  assert(tst06(s41) == 41)
  assert(tst06(t41) == 41)
  assert(tst06(s42) == 42)
  assert(tst06(t42) == -1)
  assert(tst06(s43) == 43)
  assert(tst06(t43) == 43)
  assert(tst06(s44) == 44)
  assert(tst06(t44) == -1)
  assert(tst06(s45) == 45)
  assert(tst06(t45) == -1)
  assert(tst06(s46) == 46)
  assert(tst06(t46) == -1)
  assert(tst06(s47) == 47)
  assert(tst06(t47) == -1)
  assert(tst06(s48) == 48)
  assert(tst06(t48) == 48)
  assert(tst06(s49) == 49)
  assert(tst06(t49) == -1)
  assert(tst06(s50) == 50)
  assert(tst06(t50) == -1)
  assert(tst06(s51) == 51)
  assert(tst06(t51) == 51)
  assert(tst06(s52) == 52)
  assert(tst06(t52) == 52)
  assert(tst06(s53) == 53)
  assert(tst06(t53) == 53)
  assert(tst06(s54) == 54)
  assert(tst06(t54) == -1)
  assert(tst06(s55) == 55)
  assert(tst06(t55) == 55)
  assert(tst06(s56) == 56)
  assert(tst06(t56) == 56)
  assert(tst06(s57) == 57)
  assert(tst06(t57) == 57)
  assert(tst06(s58) == 58)
  assert(tst06(t58) == 58)
  assert(tst06(s59) == 59)
  assert(tst06(t59) == 59)
  assert(tst06(s60) == 60)
  assert(tst06(t60) == 60)
  assert(tst06(s61) == 61)
  assert(tst06(t61) == 61)
  assert(tst06(s62) == 62)
  assert(tst06(t62) == 62)
  assert(tst06(s63) == 63)
  assert(tst06(t63) == 63)
  assert(tst06(s64) == 64)
  assert(tst06(t64) == 64)
  assert(tst06(s65) == 65)
  assert(tst06(t65) == 65)
  assert(tst06(s66) == 66)
  assert(tst06(t66) == 66)
  assert(tst06(s67) == 67)
  assert(tst06(t67) == 67)
  assert(tst06(s68) == 68)
  assert(tst06(t68) == 68)
  assert(tst06(s69) == 69)
  assert(tst06(t69) == 69)
  assert(tst06(s70) == 70)
  assert(tst06(t70) == 70)
  assert(tst06(s71) == 71)
  assert(tst06(t71) == 71)
  assert(tst06(s72) == 72)
  assert(tst06(t72) == 72)
  assert(tst06(s73) == 73)
  assert(tst06(t73) == 73)
  assert(tst06(s74) == 74)
  assert(tst06(t74) == 74)
  assert(tst06(s75) == 75)
  assert(tst06(t75) == 75)
  assert(tst06(s76) == 76)
  assert(tst06(t76) == 76)
  assert(tst06(s77) == 77)
  assert(tst06(t77) == 77)
  assert(tst06(s78) == 78)
  assert(tst06(t78) == 78)
  assert(tst06(s79) == 79)
  assert(tst06(t79) == 79)
  assert(tst06(s80) == 80)
  assert(tst06(t80) == 80)
  assert(tst06(s81) == 81)
  assert(tst06(t81) == 81)
  assert(tst06(s82) == 82)
  assert(tst06(t82) == 82)
  assert(tst06(s83) == 83)
  assert(tst06(t83) == 83)
  assert(tst06(s84) == 84)
  assert(tst06(t84) == 84)
  assert(tst06(s85) == 85)
  assert(tst06(t85) == 85)
  assert(tst06(s86) == 86)
  assert(tst06(t86) == 86)
  assert(tst06(s87) == 87)
  assert(tst06(t87) == 87)
  assert(tst06(s88) == 88)
  assert(tst06(t88) == 88)
  assert(tst06(s89) == 89)
  assert(tst06(t89) == 89)
  assert(tst06(s90) == 90)
  assert(tst06(t90) == 90)
  assert(tst06(s91) == 91)
  assert(tst06(t91) == 91)
  assert(tst06("") == -1)
  ()
}
