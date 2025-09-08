open Ws

let wss = Server.make({port: 82})
let address = wss->Server.address
let log = msg => Js.log(`> Server: ${msg}`)
log(`Running on: ${address.address}:${address.port->string_of_int} (${address.family})`)

module ClientSet = {
  module T = Belt.Id.MakeComparable({
    type t = Client.t;
    let cmp = (a, b) => {
      compare(a->Client.getUniqueId, b->Client.getUniqueId);
    };
  });

  let empty = Belt.Set.make(~id=module (T));


})))))))))))))))))))))))))))))); // this ")" here, this can even be "))))))))))))))))" and should error

Js.log("test") // should not be omitted
