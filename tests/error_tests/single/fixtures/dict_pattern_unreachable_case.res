type rec kind<_> =
  | Int: kind<int>
  | String: kind<string>

let f = (type a, x: kind<a>, y: kind<a>, payload: JSON.t) =>
  switch (x, y, payload) {
  | (Int, Int, _) => 0
  | (String, String, _) => 1
  | (_, _, JSON.Object(dict{"operationName": JSON.String(_)})) => 2
  | _ => 3
  }
