type 'a component = {members: 'a list}

let compute ~successors nodes =
  let index = ref 0 in
  let stack : 'a list ref = ref [] in
  let on_stack : ('a, unit) Hashtbl.t = Hashtbl.create 64 in
  let indices : ('a, int) Hashtbl.t = Hashtbl.create 64 in
  let lowlinks : ('a, int) Hashtbl.t = Hashtbl.create 64 in
  let components = ref [] in
  let push v =
    stack := v :: !stack;
    Hashtbl.replace on_stack v ()
  in
  let pop_until v =
    let rec loop acc =
      match !stack with
      | [] -> (List.rev acc, [])
      | x :: rest ->
          stack := rest;
          Hashtbl.remove on_stack x;
          let acc = x :: acc in
          if x == v then (List.rev acc, rest) else loop acc
    in
    let members, remaining = loop [] in
    stack := remaining;
    members
  in
  let rec strongconnect v =
    Hashtbl.replace indices v !index;
    Hashtbl.replace lowlinks v !index;
    incr index;
    push v;
    successors v
    |> List.iter (fun w ->
           if not (Hashtbl.mem indices w) then (
             strongconnect w;
             let low_v = Hashtbl.find lowlinks v in
             let low_w = Hashtbl.find lowlinks w in
             Hashtbl.replace lowlinks v (min low_v low_w))
           else if Hashtbl.mem on_stack w then
             let low_v = Hashtbl.find lowlinks v in
             let index_w = Hashtbl.find indices w in
             Hashtbl.replace lowlinks v (min low_v index_w));
    let low_v = Hashtbl.find lowlinks v in
    let index_v = Hashtbl.find indices v in
    if low_v = index_v then
      let members = pop_until v in
      components := {members} :: !components
  in
  nodes |> List.iter (fun v -> if not (Hashtbl.mem indices v) then strongconnect v);
  List.rev !components

