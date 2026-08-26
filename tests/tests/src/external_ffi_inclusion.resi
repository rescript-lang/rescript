/* Pins the signature-inclusion behavior for externals ahead of the
   structured-FFI representation work: these declarations are compatible
   with the implementation's and must keep compiling.

   The rejecting directions are pinned in
   tests/build_tests/super_errors_multi/fixtures/Cross_external_obj_optional_flag
   and Cross_external_spec_mismatch. */

/* identical specs: byte-equality fast path */
@obj external make_same: (~x: int=?, unit) => _ = ""

/* impl declares ~x: 'a (for_sure_no_nested_option = false); this interface
 declares ~x: int (true). Widening false -> true must be accepted. */
@obj external make_widened: (~x: int=?, unit) => _ = ""

type s = int

/* impl declares ~x: s (an alias: flag false); interface the bare builtin
 (flag true). Same widening direction through a type alias. */
@obj external make_alias_impl: (~x: int=?, unit) => _ = ""

/* identical Ffi_bs specs */
@val external parse_int: string => int = "parseInt"
