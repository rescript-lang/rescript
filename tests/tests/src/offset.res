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

module M = {
  type t = string

  let x = 0
  let x = 1

  module Set = Belt.Set.String
}

include M
