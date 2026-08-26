/* Pins the signature-inclusion behavior for externals: these declarations
   are compatible
   with the implementation's and must keep compiling.

   The rejecting direction (differing specs) is pinned in
   tests/build_tests/super_errors_multi/fixtures/Cross_external_spec_mismatch. */

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

type s2 = int

/* impl declares ~x: int (for_sure_no_nested_option = true); this interface
   uses an alias (flag false). The flag is per-module codegen conservatism
   derived from each side's own type, not part of the declaration, so both
   directions are accepted (stage 3 fixed the earlier rejection of this
   case, where the value types are identical after alias expansion). */
@obj external make_alias_intf: (~x: s2=?, unit) => _ = ""
