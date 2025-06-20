// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

// Caution: `Array.get` is implicitly used by `array[idx]` syntax
external get: (array<'a>, int) => 'a = "%array_safe_get"

// Caution: `Array.set` is implicitly used by `array[idx]` syntax
external set: (array<'a>, int, 'a) => unit = "%array_safe_set"

// Below is all deprecated and should be removed in v13

/** Return the length (number of elements) of the given array. */
@deprecated("Use Core instead. This will be removed in v13")
external length: array<'a> => int = "%array_length"

/** [Array.make n x] returns a fresh array of length [n],
   initialized with [x].
   All the elements of this new array are initially
   physically equal to [x] (in the sense of the [==] predicate).
   Consequently, if [x] is mutable, it is shared among all elements
   of the array, and modifying [x] through one of the array entries
   will modify all other entries at the same time.

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
   If the value of [x] is a floating-point number, then the maximum
   size is only [Sys.max_array_length / 2].*/
@deprecated("Use Core instead. This will be removed in v13")
let make: (int, 'a) => array<'a>

/** [Array.create_float n] returns a fresh float array of length [n],
    with uninitialized data.
    @since 4.03 */
@deprecated("Use Core instead. This will be removed in v13")
let create_float: int => array<float>

/** [Array.init n f] returns a fresh array of length [n],
   with element number [i] initialized to the result of [f i].
   In other terms, [Array.init n f] tabulates the results of [f]
   applied to the integers [0] to [n-1].

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
   If the return type of [f] is [float], then the maximum
   size is only [Sys.max_array_length / 2].*/
@deprecated("Use Core instead. This will be removed in v13")
let init: (int, int => 'a) => array<'a>

/** [Array.make_matrix dimx dimy e] returns a two-dimensional array
   (an array of arrays) with first dimension [dimx] and
   second dimension [dimy]. All the elements of this new matrix
   are initially physically equal to [e].
   The element ([x,y]) of a matrix [m] is accessed
   with the notation [m.(x).(y)].

   Raise [Invalid_argument] if [dimx] or [dimy] is negative or
   greater than {!Sys.max_array_length}.
   If the value of [e] is a floating-point number, then the maximum
   size is only [Sys.max_array_length / 2]. */
@deprecated("Use Core instead. This will be removed in v13")
let make_matrix: (int, int, 'a) => array<array<'a>>

/** [Array.append v1 v2] returns a fresh array containing the
   concatenation of the arrays [v1] and [v2]. */
@deprecated("Use Core instead. This will be removed in v13")
let append: (array<'a>, array<'a>) => array<'a>

/** Same as {!Array.append}, but concatenates a list of arrays. */
@deprecated("Use Core instead. This will be removed in v13")
let concat: list<array<'a>> => array<'a>

/** [Array.sub a start len] returns a fresh array of length [len],
   containing the elements number [start] to [start + len - 1]
   of array [a].

   Raise [Invalid_argument "Array.sub"] if [start] and [len] do not
   designate a valid subarray of [a]; that is, if
   [start < 0], or [len < 0], or [start + len > Array.length a]. */
@deprecated("Use Core instead. This will be removed in v13")
let sub: (array<'a>, int, int) => array<'a>

/** [Array.copy a] returns a copy of [a], that is, a fresh array
   containing the same elements as [a]. */
@deprecated("Use Core instead. This will be removed in v13") @send
external copy: array<'a> => array<'a> = "slice"

/** [Array.fill a ofs len x] modifies the array [a] in place,
   storing [x] in elements number [ofs] to [ofs + len - 1].

   Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not
   designate a valid subarray of [a]. */
@deprecated("Use Core instead. This will be removed in v13")
let fill: (array<'a>, int, int, 'a) => unit

/** [Array.blit v1 o1 v2 o2 len] copies [len] elements
   from array [v1], starting at element number [o1], to array [v2],
   starting at element number [o2]. It works correctly even if
   [v1] and [v2] are the same array, and the source and
   destination chunks overlap.

   Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not
   designate a valid subarray of [v1], or if [o2] and [len] do not
   designate a valid subarray of [v2]. */
@deprecated("Use Core instead. This will be removed in v13")
let blit: (array<'a>, int, array<'a>, int, int) => unit

/** [Array.to_list a] returns the list of all the elements of [a]. */
@deprecated("Use Core instead. This will be removed in v13")
let to_list: array<'a> => list<'a>

/** [Array.of_list l] returns a fresh array containing the elements
   of [l]. */
@deprecated("Use Core instead. This will be removed in v13")
let of_list: list<'a> => array<'a>

/* {1 Iterators} */

/** [Array.iter f a] applies function [f] in turn to all
   the elements of [a].  It is equivalent to
   [f a.(0); f a.(1); ...; f a.(Array.length a - 1); ()]. */
@deprecated("Use Core instead. This will be removed in v13")
let iter: ('a => unit, array<'a>) => unit

/** Same as {!Array.iter}, but the
   function is applied with the index of the element as first argument,
   and the element itself as second argument. */
@deprecated("Use Core instead. This will be removed in v13")
let iteri: ((int, 'a) => unit, array<'a>) => unit

/** [Array.map f a] applies function [f] to all the elements of [a],
   and builds an array with the results returned by [f]:
   [[| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |]]. */
@deprecated("Use Core instead. This will be removed in v13")
let map: ('a => 'b, array<'a>) => array<'b>

/** Same as {!Array.map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. */
@deprecated("Use Core instead. This will be removed in v13")
let mapi: ((int, 'a) => 'b, array<'a>) => array<'b>

/** [Array.fold_left f x a] computes
   [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
   where [n] is the length of the array [a]. */
@deprecated("Use Core instead. This will be removed in v13")
let fold_left: (('a, 'b) => 'a, 'a, array<'b>) => 'a

/** [Array.fold_right f a x] computes
   [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
   where [n] is the length of the array [a]. */
@deprecated("Use Core instead. This will be removed in v13")
let fold_right: (('b, 'a) => 'a, array<'b>, 'a) => 'a

/* {1 Iterators on two arrays} */

/** [Array.iter2 f a b] applies function [f] to all the elements of [a]
   and [b].
   Raise [Invalid_argument] if the arrays are not the same size.
   @since 4.03.0 */
@deprecated("Use Core instead. This will be removed in v13")
let iter2: (('a, 'b) => unit, array<'a>, array<'b>) => unit

/** [Array.map2 f a b] applies function [f] to all the elements of [a]
   and [b], and builds an array with the results returned by [f]:
   [[| f a.(0) b.(0); ...; f a.(Array.length a - 1) b.(Array.length b - 1)|]].
   Raise [Invalid_argument] if the arrays are not the same size.
   @since 4.03.0 */
@deprecated("Use Core instead. This will be removed in v13")
let map2: (('a, 'b) => 'c, array<'a>, array<'b>) => array<'c>

/* {1 Array scanning} */

/** [Array.for_all p [|a1; ...; an|]] checks if all elements of the array
   satisfy the predicate [p]. That is, it returns
   [(p a1) && (p a2) && ... && (p an)].
   @since 4.03.0 */
@deprecated("Use Core instead. This will be removed in v13")
let for_all: ('a => bool, array<'a>) => bool

/** [Array.exists p [|a1; ...; an|]] checks if at least one element of
    the array satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an)].
    @since 4.03.0 */
@deprecated("Use Core instead. This will be removed in v13")
let exists: ('a => bool, array<'a>) => bool

/** [mem a l] is true if and only if [a] is equal
   to an element of [l].
   @since 4.03.0 */
@deprecated("Use Core instead. This will be removed in v13")
let mem: ('a, array<'a>) => bool

/** Same as {!Array.mem}, but uses physical equality instead of structural
   equality to compare array elements.
   @since 4.03.0 */
@deprecated("Use Core instead. This will be removed in v13")
let memq: ('a, array<'a>) => bool

/* {1 Sorting} */

/** Sort an array in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see below for a
   complete specification).  For example, {!Pervasives.compare} is
   a suitable comparison function, provided there are no floating-point
   NaN values in the data.  After calling [Array.sort], the
   array is sorted in place in increasing order.
   [Array.sort] is guaranteed to run in constant heap space
   and (at most) logarithmic stack space.

   The current implementation uses Heap Sort.  It runs in constant
   stack space.

   Specification of the comparison function:
   Let [a] be the array and [cmp] the comparison function.  The following
   must be true for all x, y, z in a :
-   [cmp x y] > 0 if and only if [cmp y x] < 0
-   if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0

   When [Array.sort] returns, [a] contains the same elements as before,
   reordered in such a way that for all i and j valid indices of [a] :
-   [cmp a.(i) a.(j)] >= 0 if and only if i >= j
*/
@deprecated("Use Core instead. This will be removed in v13")
let sort: (('a, 'a) => int, array<'a>) => unit

/** Same as {!Array.sort}, but the sorting algorithm is stable (i.e.
   elements that compare equal are kept in their original order) and
   not guaranteed to run in constant heap space.

   The current implementation uses Merge Sort. It uses [n/2]
   words of heap space, where [n] is the length of the array.
   It is usually faster than the current implementation of {!Array.sort}.
*/
@deprecated("Use Core instead. This will be removed in v13")
let stable_sort: (('a, 'a) => int, array<'a>) => unit

/** Same as {!Array.sort} or {!Array.stable_sort}, whichever is faster
    on typical input.
*/
@deprecated("Use Core instead. This will be removed in v13")
let fast_sort: (('a, 'a) => int, array<'a>) => unit

@deprecated("Use Core instead. This will be removed in v13")
external unsafe_get: (array<'a>, int) => 'a = "%array_unsafe_get"

@deprecated("Use Core instead. This will be removed in v13")
external unsafe_set: (array<'a>, int, 'a) => unit = "%array_unsafe_set"
