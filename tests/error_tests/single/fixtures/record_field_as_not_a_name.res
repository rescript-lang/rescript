/* An @as that does not name the field renames nothing. Nothing else reports
 it, so it warns as the unused attribute it is. */
type t = {@as(42) a: int}
