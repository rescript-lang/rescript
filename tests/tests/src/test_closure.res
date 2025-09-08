let v = ref(0)

/** Test when i is captured 
    {[
    ((fun _ -> j = i; (fun _ -> v := !v + j)) ())
    ]}

    babel does this way:
{[
var arr = []
var f = function(){
  for(let i = 0; i < 10 ; ++ i){
    arr [i] = ()=>{console.log(i)}
    
  }
}]}


{[
var arr = [];
var f = function f() {
  var _loop = function (i) {
    arr[i] = function () {
      console.log(i);
    };
  };

  for (var i = 0; i < 10; ++i) {
    _loop(i);
  }
};
]}    

This means inline is tricky in javascript, here we try to inline [_loop]?

    


 */
let f = () => {
  let n = 10
  let arr = Belt.Array.make(10, _ => ())
  for i in 0 to n - 1 {
    arr[i] = _ => v := v.contents + i
  }
  arr
}

let () = {
  let u = f()
  u->Belt.Array.forEach(x => x())
  assert(v.contents == 45)
}
