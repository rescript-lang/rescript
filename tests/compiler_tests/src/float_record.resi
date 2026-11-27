/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Pierre Weis, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 2007 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the Q Public License version 1.0. */
/*  */
/* ********************************************************************* */

type t = private float

let make: float => t
let from: t => float

type s = {f: t}
