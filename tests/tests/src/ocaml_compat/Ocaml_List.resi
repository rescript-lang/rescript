// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

// Below is all deprecated and should be removed in v13

@deprecated("Use Core instead. This will be removed in v13")
type t<'a> = list<'a>

/** Return the length (number of elements) of the given list. */
@deprecated("Use Core instead. This will be removed in v13")
let length: list<'a> => int

/** Compare the lengths of two lists. [compare_lengths l1 l2] is
   equivalent to [compare (length l1) (length l2)], except that
   the computation stops after itering on the shortest list.
   @since 4.05.0
 */
@deprecated("Use Core instead. This will be removed in v13")
let compare_lengths: (list<'a>, list<'b>) => int

/** Compare the length of a list to an integer. [compare_length_with l n] is
   equivalent to [compare (length l) n], except that
   the computation stops after at most [n] iterations on the list.
   @since 4.05.0
*/
@deprecated("Use Core instead. This will be removed in v13")
let compare_length_with: (list<'a>, int) => int

/** [cons x xs] is [x :: xs]
    @since 4.03.0
*/
@deprecated("Use Core instead. This will be removed in v13")
let cons: ('a, list<'a>) => list<'a>

/** Return the first element of the given list. Raise
   [Failure "hd"] if the list is empty. */
@deprecated("Use Core instead. This will be removed in v13")
let hd: list<'a> => 'a

/** Return the given list without its first element. Raise
    [Failure "tl"] if the list is empty. */
@deprecated("Use Core instead. This will be removed in v13")
let tl: list<'a> => list<'a>

/** Return the [n]-th element of the given list.
   The first element (head of the list) is at position 0.
   Raise [Failure "nth"] if the list is too short.
   Raise [Invalid_argument "List.nth"] if [n] is negative. */
@deprecated("Use Core instead. This will be removed in v13")
let nth: (list<'a>, int) => 'a

/** Return the [n]-th element of the given list.
    The first element (head of the list) is at position 0.
    Return [None] if the list is too short.
    Raise [Invalid_argument "List.nth"] if [n] is negative.
    @since 4.05
*/
@deprecated("Use Core instead. This will be removed in v13")
let nth_opt: (list<'a>, int) => option<'a>

/** List reversal. */
@deprecated("Use Core instead. This will be removed in v13")
let rev: list<'a> => list<'a>

/** [List.init len f] is [f 0; f 1; ...; f (len-1)], evaluated left to right.

    @raise Invalid_argument if len < 0.
    @since 4.06.0
*/
@deprecated("Use Core instead. This will be removed in v13")
let init: (int, int => 'a) => list<'a>

/** Concatenate two lists.  Same as the infix operator [@].
   Not tail-recursive (length of the first argument).  */
@deprecated("Use Core instead. This will be removed in v13")
let append: (list<'a>, list<'a>) => list<'a>

/** [List.rev_append l1 l2] reverses [l1] and concatenates it to [l2].
   This is equivalent to {!List.rev}[ l1 @ l2], but [rev_append] is
   tail-recursive and more efficient. */
@deprecated("Use Core instead. This will be removed in v13")
let rev_append: (list<'a>, list<'a>) => list<'a>

/** Concatenate a list of lists.  The elements of the argument are all
   concatenated together (in the same order) to give the result.
   Not tail-recursive
   (length of the argument + length of the longest sub-list). */
@deprecated("Use Core instead. This will be removed in v13")
let concat: list<list<'a>> => list<'a>

/** An alias for [concat]. */
@deprecated("Use Core instead. This will be removed in v13")
let flatten: list<list<'a>> => list<'a>

/* {1 Iterators} */

/** [List.iter f [a1; ...; an]] applies function [f] in turn to
   [a1; ...; an]. It is equivalent to
   [begin f a1; f a2; ...; f an; () end]. */
@deprecated("Use Core instead. This will be removed in v13")
let iter: ('a => unit, list<'a>) => unit

/** Same as {!List.iter}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
   @since 4.00.0
*/
@deprecated("Use Core instead. This will be removed in v13")
let iteri: ((int, 'a) => unit, list<'a>) => unit

/** [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
   and builds the list [[f a1; ...; f an]]
   with the results returned by [f].  Not tail-recursive. */
@deprecated("Use Core instead. This will be removed in v13")
let map: ('a => 'b, list<'a>) => list<'b>

/** Same as {!List.map}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.  Not tail-recursive.
   @since 4.00.0
*/
@deprecated("Use Core instead. This will be removed in v13")
let mapi: ((int, 'a) => 'b, list<'a>) => list<'b>

/** [List.rev_map f l] gives the same result as
   {!List.rev}[ (]{!List.map}[ f l)], but is tail-recursive and
   more efficient. */
@deprecated("Use Core instead. This will be removed in v13")
let rev_map: ('a => 'b, list<'a>) => list<'b>

/** [List.fold_left f a [b1; ...; bn]] is
   [f (... (f (f a b1) b2) ...) bn]. */
@deprecated("Use Core instead. This will be removed in v13")
let fold_left: (('a, 'b) => 'a, 'a, list<'b>) => 'a

/** [List.fold_right f [a1; ...; an] b] is
   [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. */
@deprecated("Use Core instead. This will be removed in v13")
let fold_right: (('a, 'b) => 'b, list<'a>, 'b) => 'b

/* {1 Iterators on two lists} */

/** [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
   [f a1 b1; ...; f an bn].
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths. */
@deprecated("Use Core instead. This will be removed in v13")
let iter2: (('a, 'b) => unit, list<'a>, list<'b>) => unit

/** [List.map2 f [a1; ...; an] [b1; ...; bn]] is
   [[f a1 b1; ...; f an bn]].
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths.  Not tail-recursive. */
@deprecated("Use Core instead. This will be removed in v13")
let map2: (('a, 'b) => 'c, list<'a>, list<'b>) => list<'c>

/** [List.rev_map2 f l1 l2] gives the same result as
   {!List.rev}[ (]{!List.map2}[ f l1 l2)], but is tail-recursive and
   more efficient. */
@deprecated("Use Core instead. This will be removed in v13")
let rev_map2: (('a, 'b) => 'c, list<'a>, list<'b>) => list<'c>

/** [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
   [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths. */
@deprecated("Use Core instead. This will be removed in v13")
let fold_left2: (('a, 'b, 'c) => 'a, 'a, list<'b>, list<'c>) => 'a

/** [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
   [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths.  Not tail-recursive. */
@deprecated("Use Core instead. This will be removed in v13")
let fold_right2: (('a, 'b, 'c) => 'c, list<'a>, list<'b>, 'c) => 'c

/* {1 List scanning} */

/** [for_all p [a1; ...; an]] checks if all elements of the list
   satisfy the predicate [p]. That is, it returns
   [(p a1) && (p a2) && ... && (p an)]. */
@deprecated("Use Core instead. This will be removed in v13")
let for_all: ('a => bool, list<'a>) => bool

/** [exists p [a1; ...; an]] checks if at least one element of
   the list satisfies the predicate [p]. That is, it returns
   [(p a1) || (p a2) || ... || (p an)]. */
@deprecated("Use Core instead. This will be removed in v13")
let exists: ('a => bool, list<'a>) => bool

/** Same as {!List.for_all}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths. */
@deprecated("Use Core instead. This will be removed in v13")
let for_all2: (('a, 'b) => bool, list<'a>, list<'b>) => bool

/** Same as {!List.exists}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths. */
@deprecated("Use Core instead. This will be removed in v13")
let exists2: (('a, 'b) => bool, list<'a>, list<'b>) => bool

/** [mem a l] is true if and only if [a] is equal
   to an element of [l]. */
@deprecated("Use Core instead. This will be removed in v13")
let mem: ('a, list<'a>) => bool

/** Same as {!List.mem}, but uses physical equality instead of structural
   equality to compare list elements. */
@deprecated("Use Core instead. This will be removed in v13")
let memq: ('a, list<'a>) => bool

/* {1 List searching} */

/** [find p l] returns the first element of the list [l]
   that satisfies the predicate [p].
   Raise [Not_found] if there is no value that satisfies [p] in the
   list [l]. */
@deprecated("Use Core instead. This will be removed in v13")
let find: ('a => bool, list<'a>) => 'a

/** [find_opt p l] returns the first element of the list [l] that
    satisfies the predicate [p], or [None] if there is no value that
    satisfies [p] in the list [l].
    @since 4.05 */
@deprecated("Use Core instead. This will be removed in v13")
let find_opt: ('a => bool, list<'a>) => option<'a>

/** [filter p l] returns all the elements of the list [l]
   that satisfy the predicate [p].  The order of the elements
   in the input list is preserved.  */
@deprecated("Use Core instead. This will be removed in v13")
let filter: ('a => bool, list<'a>) => list<'a>

/** [find_all] is another name for {!List.filter}. */
let find_all: ('a => bool, list<'a>) => list<'a>

/** [partition p l] returns a pair of lists [(l1, l2)], where
   [l1] is the list of all the elements of [l] that
   satisfy the predicate [p], and [l2] is the list of all the
   elements of [l] that do not satisfy [p].
   The order of the elements in the input list is preserved. */
@deprecated("Use Core instead. This will be removed in v13")
let partition: ('a => bool, list<'a>) => (list<'a>, list<'a>)

/* {1 Association lists} */

/** [assoc a l] returns the value associated with key [a] in the list of
   pairs [l]. That is,
   [assoc a [ ...; (a,b); ...] = b]
   if [(a,b)] is the leftmost binding of [a] in list [l].
   Raise [Not_found] if there is no value associated with [a] in the
   list [l]. */
@deprecated("Use Core instead. This will be removed in v13")
let assoc: ('a, list<('a, 'b)>) => 'b

/** [assoc_opt a l] returns the value associated with key [a] in the list of
   pairs [l]. That is,
   [assoc_opt a [ ...; (a,b); ...] = b]
   if [(a,b)] is the leftmost binding of [a] in list [l].
   Returns [None] if there is no value associated with [a] in the
   list [l].
   @since 4.05 */
@deprecated("Use Core instead. This will be removed in v13")
let assoc_opt: ('a, list<('a, 'b)>) => option<'b>

/** Same as {!List.assoc}, but uses physical equality instead of structural
   equality to compare keys. */
@deprecated("Use Core instead. This will be removed in v13")
let assq: ('a, list<('a, 'b)>) => 'b

/** Same as {!List.assoc_opt}, but uses physical equality instead of structural
    equality to compare keys.
    @since 4.05 */
@deprecated("Use Core instead. This will be removed in v13")
let assq_opt: ('a, list<('a, 'b)>) => option<'b>

/** Same as {!List.assoc}, but simply return true if a binding exists,
   and false if no bindings exist for the given key. */
@deprecated("Use Core instead. This will be removed in v13")
let mem_assoc: ('a, list<('a, 'b)>) => bool

/** Same as {!List.mem_assoc}, but uses physical equality instead of
   structural equality to compare keys. */
@deprecated("Use Core instead. This will be removed in v13")
let mem_assq: ('a, list<('a, 'b)>) => bool

/** [remove_assoc a l] returns the list of
   pairs [l] without the first pair with key [a], if any.
   Not tail-recursive. */
@deprecated("Use Core instead. This will be removed in v13")
let remove_assoc: ('a, list<('a, 'b)>) => list<('a, 'b)>

/** Same as {!List.remove_assoc}, but uses physical equality instead
   of structural equality to compare keys.  Not tail-recursive. */
@deprecated("Use Core instead. This will be removed in v13")
let remove_assq: ('a, list<('a, 'b)>) => list<('a, 'b)>

/* {1 Lists of pairs} */

/** Transform a list of pairs into a pair of lists:
   [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
   Not tail-recursive.
*/
@deprecated("Use Core instead. This will be removed in v13")
let split: list<('a, 'b)> => (list<'a>, list<'b>)

/** Transform a pair of lists into a list of pairs:
   [combine [a1; ...; an] [b1; ...; bn]] is
   [[(a1,b1); ...; (an,bn)]].
   Raise [Invalid_argument] if the two lists
   have different lengths.  Not tail-recursive. */
@deprecated("Use Core instead. This will be removed in v13")
let combine: (list<'a>, list<'b>) => list<('a, 'b)>

/* {1 Sorting} */

/** Sort a list in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see Array.sort for
   a complete specification).  For example,
   {!Pervasives.compare} is a suitable comparison function.
   The resulting list is sorted in increasing order.
   [List.sort] is guaranteed to run in constant heap space
   (in addition to the size of the result list) and logarithmic
   stack space.

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
*/
@deprecated("Use Core instead. This will be removed in v13")
let sort: (('a, 'a) => int, list<'a>) => list<'a>

/** Same as {!List.sort}, but the sorting algorithm is guaranteed to
   be stable (i.e. elements that compare equal are kept in their
   original order) .

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
*/
@deprecated("Use Core instead. This will be removed in v13")
let stable_sort: (('a, 'a) => int, list<'a>) => list<'a>

/** Same as {!List.sort} or {!List.stable_sort}, whichever is faster
    on typical input. */
@deprecated("Use Core instead. This will be removed in v13")
let fast_sort: (('a, 'a) => int, list<'a>) => list<'a>

/** Same as {!List.sort}, but also remove duplicates.
    @since 4.02.0 */
@deprecated("Use Core instead. This will be removed in v13")
let sort_uniq: (('a, 'a) => int, list<'a>) => list<'a>

/** Merge two lists:
    Assuming that [l1] and [l2] are sorted according to the
    comparison function [cmp], [merge cmp l1 l2] will return a
    sorted list containing all the elements of [l1] and [l2].
    If several elements compare equal, the elements of [l1] will be
    before the elements of [l2].
    Not tail-recursive (sum of the lengths of the arguments).
*/
@deprecated("Use Core instead. This will be removed in v13")
let merge: (('a, 'a) => int, list<'a>, list<'a>) => list<'a>
