/*
   To do the right [tail call] conversion
   when the arguments are functions which captures some variables, 
   we need create a scope to capture it 
   the naive version:
   {[
   var f = 
   function(n,acc)
   {f_tailcall_0001:
     while(/* true */1)
     {if(n===0)
       {return acc(/* () */0);}
      else
       {n=n-1;
        acc=
        function()
         {console.log(Pervasives.Js.Int.toString(n));
          return acc(/* () */0);};
        continue f_tailcall_0001;}};};
   
   ]}

   A correct but less optimal version: 
   {[
   var f = 
   function(n,acc)
   {f_tailcall_0001:
     while(/* true */1)
     { (funtcion(n,acc){if(n===0)
       {return acc(/* () */0);}
      else
       {n=n-1;
        acc=
        function()
         {console.log(Pervasives.Js.Int.toString(n));
          return acc(/* () */0);};
        continue f_tailcall_0001;}}(n,acc)) };};   
   ]}

   It does not work, since `continue` can not cross functions


   the right version: 
   {[
   var f = 
    function(n,acc)
      {f_tailcall_0001:
     while(/* true */1)
     {if(n===0)
       {return acc(/* () */0);}
      else
       {
        acc = (function(){
            var acc1 = acc ;
            var n1 = n;
            return function() {
                console.log(Pervasives.Js.Int.toString(n1));
                return acc1(/* () */0);}
        }());
           n=n-1;
        continue f_tailcall_0001;}};};

   ]}

   with [let]
*/

let rec f = (n, acc) =>
  if n == 0 {
    acc()
  } else {
    f(n - 1, _ => {
      n->Js.Int.toString->Console.log
      acc()
    })
  }

/**
    Here a naive version would be 
    {[
    for(var i = 0;i<=n;++i)
     {arr[i]=function(){return i;};}
    ]}

    The correct version should be 
    {[
    for(var i = 0;i<=n;++i)
     {arr[i]= (function(){ var i$1 = i; function(){return i$1;};}())
    ]}
 */
let test_closure = () => {
  let n = 6
  let arr = Belt.Array.make(n, x => x)
  for i in 0 to n {
    arr[i] = _ => i
  }
  arr
}

f(10, _ => ())
