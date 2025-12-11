module F = {
  let g = "hello"
  let xxx = _ => Console.log(g)
}

let ff = <div onClick={F.xxx}></div>
//                       ^xfm

