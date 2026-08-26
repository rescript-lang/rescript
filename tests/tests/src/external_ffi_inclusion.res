/* See external_ffi_inclusion.resi for what this pins. */

@obj external make_same: (~x: int=?, unit) => _ = ""

@obj external make_widened: (~x: 'a=?, unit) => _ = ""

type s = int

@obj external make_alias_impl: (~x: s=?, unit) => _ = ""

@val external parse_int: string => int = "parseInt"

type s2 = int

@obj external make_alias_intf: (~x: int=?, unit) => _ = ""
