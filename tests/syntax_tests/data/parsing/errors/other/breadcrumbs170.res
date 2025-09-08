// it should report an error on the `}` on line 9
let l = Some(list{1, 2, 3})->Obj.magic
module M = {
  switch l {
  | None => list{}
  | Some(l) => l["prop"]
  }
}
}from now on all syntax is "valid", 
it compiles and will be ignored in js/ removed on format
