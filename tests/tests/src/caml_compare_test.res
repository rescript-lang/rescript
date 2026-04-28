@@warning("-45")
open Mocha
open Test_utils

type u = A(int) | B(int, bool) | C(int)

let function_equal_test = try (x => x + 1) == (x => x + 2) catch {
| Invalid_argument("equal: functional value") => true
| _ => false
}

describe(__MODULE__, () => {
  test("option compare 1", () => {
    eq(__LOC__, true, None < Some(1))
  })

  test("option compare 2", () => {
    eq(__LOC__, true, Some(1) < Some(2))
  })

  test("list compare 1", () => {
    eq(__LOC__, true, list{1} > list{})
  })

  test("list equal", () => {
    eq(__LOC__, true, list{1, 2, 3} == list{1, 2, 3})
  })

  test("list not equal", () => {
    eq(__LOC__, true, list{1, 2, 3} > list{1, 2, 2})
  })

  test("custom variant compare", () => {
    eq(__LOC__, true, (A(3), B(2, false), C(1)) > (A(3), B(2, false), C(0)))
  })

  test("custom variant equal", () => {
    eq(__LOC__, true, (A(3), B(2, false), C(1)) == (A(3), B(2, false), C(1)))
  })

  test("function equal", () => {
    eq(__LOC__, true, function_equal_test)
  })

  test("option compare 3", () => {
    eq(__LOC__, true, None < Some([1, 30]))
  })

  test("option compare 4", () => {
    eq(__LOC__, true, Some([1, 30]) > None)
  })

  test("list compare long", () => {
    eq(__LOC__, true, list{2, 6, 1, 1, 2, 1, 4, 2, 1} < list{2, 6, 1, 1, 2, 1, 4, 2, 1, 409})
  })

  test("list compare short", () => {
    eq(__LOC__, true, list{1} < list{1, 409})
  })

  test("list compare empty", () => {
    eq(__LOC__, true, list{} < list{409})
  })

  test("list compare long 2", () => {
    eq(__LOC__, true, list{2, 6, 1, 1, 2, 1, 4, 2, 1, 409} > list{2, 6, 1, 1, 2, 1, 4, 2, 1})
  })

  test("option not equal 1", () => {
    eq(__LOC__, false, None == Some([1, 30]))
  })

  test("option not equal 2", () => {
    eq(__LOC__, false, Some([1, 30]) == None)
  })

  test("list not equal long", () => {
    eq(__LOC__, false, list{2, 6, 1, 1, 2, 1, 4, 2, 1} == list{2, 6, 1, 1, 2, 1, 4, 2, 1, 409})
  })

  test("list not equal long 2", () => {
    eq(__LOC__, false, list{2, 6, 1, 1, 2, 1, 4, 2, 1, 409} == list{2, 6, 1, 1, 2, 1, 4, 2, 1})
  })

  test("object compare id", () => {
    eq(__LOC__, compare({"x": 1, "y": 2}, {"x": 1, "y": 2}), 0)
  })

  test("object compare value", () => {
    eq(__LOC__, compare({"x": 1}, {"x": 2}), -1)
  })

  test("object compare value 2", () => {
    eq(__LOC__, compare({"x": 2}, {"x": 1}), 1)
  })

  test("object compare empty", () => {
    eq(__LOC__, compare(%raw("{}"), %raw("{}")), 0)
  })

  test("object compare empty 2", () => {
    eq(__LOC__, compare(%raw("{}"), %raw("{x:1}")), -1)
  })

  test("object compare swap", () => {
    eq(__LOC__, compare({"x": 1, "y": 2}, {"y": 2, "x": 1}), 0)
  })

  test("object compare size", () => {
    eq(__LOC__, compare(%raw("{x:1}"), %raw("{x:1, y:2}")), -1)
  })

  test("object compare size 2", () => {
    eq(__LOC__, compare(%raw("{x:1, y:2}"), %raw("{x:1}")), 1)
  })

  test("object compare order", () => {
    eq(__LOC__, compare({"x": 0, "y": 1}, {"x": 1, "y": 0}), -1)
  })

  test("object compare order 2", () => {
    eq(__LOC__, compare({"x": 1, "y": 0}, {"x": 0, "y": 1}), 1)
  })

  test("object compare in list", () => {
    eq(__LOC__, compare(list{{"x": 1}}, list{{"x": 2}}), -1)
  })

  test("object compare in list 2", () => {
    eq(__LOC__, compare(list{{"x": 2}}, list{{"x": 1}}), 1)
  })

  test("object compare with list", () => {
    eq(__LOC__, compare({"x": list{0}}, {"x": list{1}}), -1)
  })

  test("object compare with list 2", () => {
    eq(__LOC__, compare({"x": list{1}}, {"x": list{0}}), 1)
  })

  test("object equal id", () => {
    ok(__LOC__, {"x": 1, "y": 2} == {"x": 1, "y": 2})
  })

  test("object equal value", () => {
    eq(__LOC__, {"x": 1} == {"x": 2}, false)
  })

  test("object equal value 2", () => {
    eq(__LOC__, {"x": 2} == {"x": 1}, false)
  })

  test("object equal empty", () => {
    eq(__LOC__, %raw("{}") == %raw("{}"), true)
  })

  test("object equal empty 2", () => {
    eq(__LOC__, %raw("{}") == %raw("{x:1}"), false)
  })

  test("object equal swap", () => {
    ok(__LOC__, {"x": 1, "y": 2} == {"y": 2, "x": 1})
  })

  test("object equal size", () => {
    eq(__LOC__, %raw("{x:1}") == %raw("{x:1, y:2}"), false)
  })

  test("object equal size 2", () => {
    eq(__LOC__, %raw("{x:1, y:2}") == %raw("{x:1}"), false)
  })

  test("object equal in list", () => {
    eq(__LOC__, list{{"x": 1}} == list{{"x": 2}}, false)
  })

  test("object equal in list 2", () => {
    eq(__LOC__, list{{"x": 2}} == list{{"x": 2}}, true)
  })

  test("object equal with list", () => {
    eq(__LOC__, {"x": list{0}} == {"x": list{0}}, true)
  })

  test("object equal with list 2", () => {
    eq(__LOC__, {"x": list{0}} == {"x": list{1}}, false)
  })

  test("object equal no prototype", () => {
    eq(
      __LOC__,
      %raw("{x:1}") == %raw("(function(){let o = Object.create(null);o.x = 1;return o;})()"),
      true,
    )
  })

  test("null compare 1", () => {
    eq(__LOC__, compare(Js.null, Js.Null.return(list{3})), -1)
  })

  test("null compare 2", () => {
    eq(__LOC__, compare(Js.Null.return(list{3}), Js.null), 1)
  })

  test("null compare 3", () => {
    eq(__LOC__, compare(Js.null, Js.Null.return(0)), -1)
  })

  test("null compare 4", () => {
    eq(__LOC__, compare(Js.Null.return(0), Js.null), 1)
  })

  test("undefined compare 1", () => {
    eq(__LOC__, compare(Js.Nullable.undefined, Js.Nullable.return(0)), -1)
  })

  test("undefined compare 2", () => {
    eq(__LOC__, compare(Js.Nullable.return(0), Js.Nullable.undefined), 1)
  })

  test("additional option compare 1", () => {
    eq(__LOC__, true, Some(1) > None)
  })

  test("additional list compare 1", () => {
    eq(__LOC__, true, list{} < list{1})
  })

  test("additional option compare 2", () => {
    eq(__LOC__, false, None > Some(1))
  })

  test("additional option compare 3", () => {
    eq(__LOC__, false, None > Some([1, 30]))
  })

  test("additional option compare 4", () => {
    eq(__LOC__, false, Some([1, 30]) < None)
  })
})
