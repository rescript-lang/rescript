/* A settable field is an invariant occurrence of its payload (it can be
   both read and written), so a covariant parameter annotation is rejected,
   like a mutable record label. */
type box<+'a> = {@set "x": 'a}
