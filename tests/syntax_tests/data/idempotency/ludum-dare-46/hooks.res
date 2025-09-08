module Internal: {
  type t
  let empty: t
  let initialize: t => unit
  let finalize: unit => t
  let useState: (string, ~id: string=?, 'a) => (ref<'a>, 'a => unit)
} = {
  module StringMap = Map.Make(String)

  type t = StringMap.t<string>

  let globalRef: ref<t> = ref(StringMap.empty)

  let empty = StringMap.empty

  let initialize = hooks => globalRef := hooks

  let finalize = () => globalRef.contents

  let useState = (loc, ~id="", initialState) => {
    let key = loc ++ id
    let initialState = Obj.magic(initialState)
    let v = switch StringMap.find(key, globalRef.contents) {
    | v => ref(v)
    | exception Not_found =>
      globalRef := StringMap.add(key, initialState, globalRef.contents)
      ref(initialState)
    }
    let setState = newVal => {
      let newVal = Obj.magic(newVal)
      v := newVal
      globalRef := StringMap.add(key, newVal, globalRef.contents)
    }
    (Obj.magic(v), setState)
  }
}

include Internal
