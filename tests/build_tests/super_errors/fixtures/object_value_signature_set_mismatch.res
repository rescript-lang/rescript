/* A value signature claims an instance of the implementation's type:
   moregeneral requires equal field mutability, so an interface cannot
   silently drop @set from a value's object type. */
@val external impl: {@set "x": int} = "impl"
module M: {
  let v: {"x": int}
} = {
  let v = impl
}
