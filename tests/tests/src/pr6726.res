/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Jacques Garrigue, Nagoya University */
/*  */
/* Copyright 2014 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the Q Public License version 1.0. */
/*  */
/* ********************************************************************* */

module ExtUnixAll = {
  external unused: unit => unit = "%identity"
  module BigEndian = {
    let get_uint8 = (str, off) => 33
  }
}

module ExtUnix = {
  module All = ExtUnixAll
}

module Test = {
  open ExtUnix.All
  let test_endian_string = x => {
    module B = BigEndian
    B.get_uint8(x, 0)
  }
  let v = test_endian_string(1)
}
