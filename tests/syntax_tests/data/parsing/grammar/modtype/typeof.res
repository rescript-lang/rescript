module type MyHash = {
  include module type of { include Hashtbl }
  let replace: (t<'a, 'b>, 'a, 'b) => unit
}

module type MyHash = {
  include @onModuleTypeOf module type of { include Hashtbl }
  let replace: (t<'a, 'b>, 'a, 'b) => unit
}
