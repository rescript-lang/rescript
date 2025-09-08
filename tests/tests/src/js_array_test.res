let suites = {
  open Mt
  list{
    /* es2015, unable to test because nothing currently implements array_like
    "from", (fun _ ->
      Eq(
        [| 0; 1 |],
        [| "a"; "b" |] |. Js.Array2.keys |. Js.Array2.from)
    );
 */

    /* es2015, unable to test because nothing currently implements array_like
    "fromMap", (fun _ ->
      Eq(
        [| (-1); 0 |],
        Js.Array2.fromMap
          ([| "a"; "b" |] |. Js.Array2.keys)
          ((fun x -> x - 1) [@bs]))
    );
 */

    /* es2015 */
    ("isArray_array", _ => Eq(true, []->Js.Array2.isArray)),
    ("isArray_int", _ => Eq(false, 34->Js.Array2.isArray)),
    ("length", _ => Eq(3, [1, 2, 3]->Js.Array2.length)),
    /* es2015 */
    ("copyWithin", _ => Eq([1, 2, 3, 1, 2], [1, 2, 3, 4, 5]->Js.Array2.copyWithin(~to_=-2))),
    (
      "copyWithinFrom",
      _ => Eq([4, 5, 3, 4, 5], [1, 2, 3, 4, 5]->Js.Array2.copyWithinFrom(~to_=0, ~from=3)),
    ),
    (
      "copyWithinFromRange",
      _ => Eq(
        [4, 2, 3, 4, 5],
        [1, 2, 3, 4, 5]->Js.Array2.copyWithinFromRange(~to_=0, ~start=3, ~end_=4),
      ),
    ),
    /* es2015 */
    ("fillInPlace", _ => Eq([4, 4, 4], [1, 2, 3]->Js.Array2.fillInPlace(4))),
    ("fillFromInPlace", _ => Eq([1, 4, 4], [1, 2, 3]->Js.Array2.fillFromInPlace(4, ~from=1))),
    (
      "fillRangeInPlace",
      _ => Eq([1, 4, 3], [1, 2, 3]->Js.Array2.fillRangeInPlace(4, ~start=1, ~end_=2)),
    ),
    ("pop", _ => Eq(Some(3), [1, 2, 3]->Js.Array2.pop)),
    ("pop - empty array", _ => Eq(None, []->Js.Array2.pop)),
    ("push", _ => Eq(4, [1, 2, 3]->Js.Array2.push(4))),
    ("pushMany", _ => Eq(5, [1, 2, 3]->Js.Array2.pushMany([4, 5]))),
    ("reverseInPlace", _ => Eq([3, 2, 1], [1, 2, 3]->Js.Array2.reverseInPlace)),
    ("shift", _ => Eq(Some(1), [1, 2, 3]->Js.Array2.shift)),
    ("shift - empty array", _ => Eq(None, []->Js.Array2.shift)),
    ("sortInPlace", _ => Eq([1, 2, 3], [3, 1, 2]->Js.Array2.sortInPlace)),
    ("sortInPlaceWith", _ => Eq([3, 2, 1], [3, 1, 2]->Js.Array2.sortInPlaceWith((a, b) => b - a))),
    (
      "spliceInPlace",
      _ => {
        let arr = [1, 2, 3, 4]
        let removed = arr->Js.Array2.spliceInPlace(~pos=2, ~remove=0, ~add=[5])

        Eq(([1, 2, 5, 3, 4], []), (arr, removed))
      },
    ),
    (
      "removeFromInPlace",
      _ => {
        let arr = [1, 2, 3, 4]
        let removed = arr->Js.Array2.removeFromInPlace(~pos=2)

        Eq(([1, 2], [3, 4]), (arr, removed))
      },
    ),
    (
      "removeCountInPlace",
      _ => {
        let arr = [1, 2, 3, 4]
        let removed = arr->Js.Array2.removeCountInPlace(~pos=2, ~count=1)

        Eq(([1, 2, 4], [3]), (arr, removed))
      },
    ),
    ("unshift", _ => Eq(4, [1, 2, 3]->Js.Array2.unshift(4))),
    ("unshiftMany", _ => Eq(5, [1, 2, 3]->Js.Array2.unshiftMany([4, 5]))),
    ("append", _ => Eq([1, 2, 3, 4], [1, 2, 3]->Js.Array2.concat([4]))),
    ("concat", _ => Eq([1, 2, 3, 4, 5], [1, 2, 3]->Js.Array2.concat([4, 5]))),
    (
      "concatMany",
      _ => Eq([1, 2, 3, 4, 5, 6, 7], [1, 2, 3]->Js.Array2.concatMany([[4, 5], [6, 7]])),
    ),
    /* es2016 */
    ("includes", _ => Eq(true, [1, 2, 3]->Js.Array2.includes(3))),
    ("indexOf", _ => Eq(1, [1, 2, 3]->Js.Array2.indexOf(2))),
    ("indexOfFrom", _ => Eq(3, [1, 2, 3, 2]->Js.Array2.indexOfFrom(2, ~from=2))),
    ("join", _ => Eq("1,2,3", [1, 2, 3]->Js.Array.join)),
    ("joinWith", _ => Eq("1;2;3", [1, 2, 3]->Js.Array2.joinWith(";"))),
    ("lastIndexOf", _ => Eq(1, [1, 2, 3]->Js.Array2.lastIndexOf(2))),
    ("lastIndexOfFrom", _ => Eq(1, [1, 2, 3, 2]->Js.Array2.lastIndexOfFrom(2, ~from=2))),
    ("slice", _ => Eq([2, 3], [1, 2, 3, 4, 5]->Js.Array2.slice(~start=1, ~end_=3))),
    ("copy", _ => Eq([1, 2, 3, 4, 5], [1, 2, 3, 4, 5]->Js.Array2.copy)),
    ("sliceFrom", _ => Eq([3, 4, 5], [1, 2, 3, 4, 5]->Js.Array2.sliceFrom(2))),
    ("toString", _ => Eq("1,2,3", [1, 2, 3]->Js.Array2.toString)),
    ("toLocaleString", _ => Eq("1,2,3", [1, 2, 3]->Js.Array2.toLocaleString)),
    /* es2015, iterator
    "entries", (fun _ ->
      Eq([| (0, "a"); (1, "b"); (2, "c") |],
         [| "a"; "b"; "c" |] |. Js.Array2.entries |. Js.Array2.from)
    );
 */

    ("every", _ => Eq(true, [1, 2, 3]->Js.Array2.every(n => n > 0))),
    ("everyi", _ => Eq(false, [1, 2, 3]->Js.Array2.everyi((_, i) => i > 0))),
    ("filter", _ => Eq([2, 4], [1, 2, 3, 4]->Js.Array2.filter(n => mod(n, 2) == 0))),
    ("filteri", _ => Eq([1, 3], [1, 2, 3, 4]->Js.Array2.filteri((_, i) => mod(i, 2) == 0))),
    /* es2015 */
    ("find", _ => Eq(Some(2), [1, 2, 3, 4]->Js.Array2.find(n => mod(n, 2) == 0))),
    ("find - no match", _ => Eq(None, [1, 2, 3, 4]->Js.Array2.find(n => mod(n, 2) == 5))),
    ("findi", _ => Eq(Some(1), [1, 2, 3, 4]->Js.Array2.findi((_, i) => mod(i, 2) == 0))),
    ("findi - no match", _ => Eq(None, [1, 2, 3, 4]->Js.Array2.findi((_, i) => mod(i, 2) == 5))),
    /* es2015 */
    ("findIndex", _ => Eq(1, [1, 2, 3, 4]->Js.Array2.findIndex(n => mod(n, 2) == 0))),
    ("findIndexi", _ => Eq(0, [1, 2, 3, 4]->Js.Array2.findIndexi((_, i) => mod(i, 2) == 0))),
    (
      "forEach",
      _ => {
        let sum = ref(0)
        let _ = [1, 2, 3]->Js.Array2.forEach(n => sum := sum.contents + n)

        Eq(6, sum.contents)
      },
    ),
    (
      "forEachi",
      _ => {
        let sum = ref(0)
        let _ = [1, 2, 3]->Js.Array2.forEachi((_, i) => sum := sum.contents + i)

        Eq(3, sum.contents)
      },
    ),
    /* es2015, iterator
    "keys", (fun _ ->
      Eq([| 0; 1; 2 |],
         [| "a"; "b"; "c" |] |. Js.Array2.keys |. Js.Array2.from)
    );
 */

    ("map", _ => Eq([2, 4, 6, 8], [1, 2, 3, 4]->Js.Array2.map(n => n * 2))),
    ("map", _ => Eq([0, 2, 4, 6], [1, 2, 3, 4]->Js.Array2.mapi((_, i) => i * 2))),
    ("reduce", _ => Eq(-10, [1, 2, 3, 4]->Js.Array2.reduce((acc, n) => acc - n, 0))),
    ("reducei", _ => Eq(-6, [1, 2, 3, 4]->Js.Array2.reducei((acc, _, i) => acc - i, 0))),
    ("reduceRight", _ => Eq(-10, [1, 2, 3, 4]->Js.Array2.reduceRight((acc, n) => acc - n, 0))),
    ("reduceRighti", _ => Eq(-6, [1, 2, 3, 4]->Js.Array2.reduceRighti((acc, _, i) => acc - i, 0))),
    ("some", _ => Eq(false, [1, 2, 3, 4]->Js.Array2.some(n => n <= 0))),
    ("somei", _ => Eq(true, [1, 2, 3, 4]->Js.Array2.somei((_, i) => i <= 0))),

    /* es2015, iterator
    "values", (fun _ ->
      Eq([| "a"; "b"; "c" |],
         [| "a"; "b"; "c" |] |. Js.Array2.values |. Js.Array2.from)
    );
 */
  }
}

Mt.from_pair_suites(__MODULE__, suites)
