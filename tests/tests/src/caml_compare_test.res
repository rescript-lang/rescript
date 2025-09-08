@@warning("-45")
type u = A(int) | B(int, bool) | C(int)

let function_equal_test = try (x => x + 1) == (x => x + 2) catch {
| Invalid_argument("equal: functional value") => true
| _ => false
}

let suites = ref({
  open Mt
  list{
    (__LOC__, _ => Eq(true, None < Some(1))),
    ("option2", _ => Eq(true, Some(1) < Some(2))),
    (__LOC__, _ => Eq(true, list{1} > list{})),
    ("listeq", _ => Eq(true, list{1, 2, 3} == list{1, 2, 3})),
    ("listneq", _ => Eq(true, list{1, 2, 3} > list{1, 2, 2})),
    ("custom_u", _ => Eq(true, (A(3), B(2, false), C(1)) > (A(3), B(2, false), C(0)))),
    ("custom_u2", _ => Eq(true, (A(3), B(2, false), C(1)) == (A(3), B(2, false), C(1)))),
    ("function", _ => Eq(true, function_equal_test)),
    (__LOC__, _ => Eq(true, None < Some(1))),
    (__LOC__, _ => Eq(true, None < Some([1, 30]))),
    (__LOC__, _ => Eq(true, Some([1, 30]) > None)),
    (
      __LOC__,
      _ => Eq(true, list{2, 6, 1, 1, 2, 1, 4, 2, 1} < list{2, 6, 1, 1, 2, 1, 4, 2, 1, 409}),
    ),
    (__LOC__, _ => Eq(true, list{1} < list{1, 409})),
    (__LOC__, _ => Eq(true, list{} < list{409})),
    (
      __LOC__,
      _ => Eq(true, list{2, 6, 1, 1, 2, 1, 4, 2, 1, 409} > list{2, 6, 1, 1, 2, 1, 4, 2, 1}),
    ),
    (__LOC__, _ => Eq(false, None == Some([1, 30]))),
    (__LOC__, _ => Eq(false, Some([1, 30]) == None)),
    (
      __LOC__,
      _ => Eq(false, list{2, 6, 1, 1, 2, 1, 4, 2, 1} == list{2, 6, 1, 1, 2, 1, 4, 2, 1, 409}),
    ),
    (
      __LOC__,
      _ => Eq(false, list{2, 6, 1, 1, 2, 1, 4, 2, 1, 409} == list{2, 6, 1, 1, 2, 1, 4, 2, 1}),
    ),
    ("cmp_id", _ => Eq(compare({"x": 1, "y": 2}, {"x": 1, "y": 2}), 0)),
    ("cmp_val", _ => Eq(compare({"x": 1}, {"x": 2}), -1)),
    ("cmp_val2", _ => Eq(compare({"x": 2}, {"x": 1}), 1)),
    ("cmp_empty", _ => Eq(compare(%raw("{}"), %raw("{}")), 0)),
    ("cmp_empty2", _ => Eq(compare(%raw("{}"), %raw("{x:1}")), -1)),
    ("cmp_swap", _ => Eq(compare({"x": 1, "y": 2}, {"y": 2, "x": 1}), 0)),
    ("cmp_size", _ => Eq(compare(%raw("{x:1}"), %raw("{x:1, y:2}")), -1)),
    ("cmp_size2", _ => Eq(compare(%raw("{x:1, y:2}"), %raw("{x:1}")), 1)),
    ("cmp_order", _ => Eq(compare({"x": 0, "y": 1}, {"x": 1, "y": 0}), -1)),
    ("cmp_order2", _ => Eq(compare({"x": 1, "y": 0}, {"x": 0, "y": 1}), 1)),
    ("cmp_in_list", _ => Eq(compare(list{{"x": 1}}, list{{"x": 2}}), -1)),
    ("cmp_in_list2", _ => Eq(compare(list{{"x": 2}}, list{{"x": 1}}), 1)),
    ("cmp_with_list", _ => Eq(compare({"x": list{0}}, {"x": list{1}}), -1)),
    ("cmp_with_list2", _ => Eq(compare({"x": list{1}}, {"x": list{0}}), 1)),
    ("eq_id", _ => Ok({"x": 1, "y": 2} == {"x": 1, "y": 2})),
    ("eq_val", _ => Eq({"x": 1} == {"x": 2}, false)),
    ("eq_val2", _ => Eq({"x": 2} == {"x": 1}, false)),
    ("eq_empty", _ => Eq(%raw("{}") == %raw("{}"), true)),
    ("eq_empty2", _ => Eq(%raw("{}") == %raw("{x:1}"), false)),
    ("eq_swap", _ => Ok({"x": 1, "y": 2} == {"y": 2, "x": 1})),
    ("eq_size", _ => Eq(%raw("{x:1}") == %raw("{x:1, y:2}"), false)),
    ("eq_size2", _ => Eq(%raw("{x:1, y:2}") == %raw("{x:1}"), false)),
    ("eq_in_list", _ => Eq(list{{"x": 1}} == list{{"x": 2}}, false)),
    ("eq_in_list2", _ => Eq(list{{"x": 2}} == list{{"x": 2}}, true)),
    ("eq_with_list", _ => Eq({"x": list{0}} == {"x": list{0}}, true)),
    ("eq_with_list2", _ => Eq({"x": list{0}} == {"x": list{1}}, false)),
    (
      "eq_no_prototype",
      _ => Eq(
        %raw("{x:1}") == %raw("(function(){let o = Object.create(null);o.x = 1;return o;})()"),
        true,
      ),
    ),
    (__LOC__, _ => Eq(compare(Js.null, Js.Null.return(list{3})), -1)),
    (__LOC__, _ => Eq(compare(Js.Null.return(list{3}), Js.null), 1)),
    (__LOC__, _ => Eq(compare(Js.null, Js.Null.return(0)), -1)),
    (__LOC__, _ => Eq(compare(Js.Null.return(0), Js.null), 1)),
    (__LOC__, _ => Eq(compare(Js.Nullable.undefined, Js.Nullable.return(0)), -1)),
    (__LOC__, _ => Eq(compare(Js.Nullable.return(0), Js.Nullable.undefined), 1)),
  }
})

let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

eq(__LOC__, true, Some(1) > None)
eq(__LOC__, true, list{} < list{1})
eq(__LOC__, false, None > Some(1))
eq(__LOC__, false, None > Some([1, 30]))
eq(__LOC__, false, Some([1, 30]) < None)
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
