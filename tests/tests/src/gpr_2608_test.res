open Belt

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let nearestGroots = list{}

let oppHeroes = list{0}
let huntGrootCondition =
  List.length(nearestGroots) > 0 && {
      let x = oppHeroes->List.filter(h => List.headExn(nearestGroots) <= 1000)
      List.length(x) == 0
    }

let huntGrootCondition2 =
  List.length(nearestGroots) >= 0 || {
      let x = oppHeroes->List.filter(h => List.headExn(nearestGroots) <= 1000)
      List.length(x) == 0
    }

let () = {
  eq(__LOC__, huntGrootCondition, false)
  eq(__LOC__, huntGrootCondition2, true)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
