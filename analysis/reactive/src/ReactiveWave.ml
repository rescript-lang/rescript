type ('k, 'v) t = {keys: Obj.t array; vals: Obj.t array; mutable len: int}

let create ~max_entries =
  if max_entries <= 0 then
    invalid_arg "ReactiveWave.create: max_entries must be > 0";
  {
    keys = Array.make max_entries (Obj.repr ());
    vals = Array.make max_entries (Obj.repr ());
    len = 0;
  }

let clear t = t.len <- 0

let push (type k v) (t : (k, v) t) (k : k) (v : v) =
  if t.len >= Array.length t.keys then
    invalid_arg "ReactiveWave.push: capacity exceeded";
  t.keys.(t.len) <- Obj.repr k;
  t.vals.(t.len) <- Obj.repr v;
  t.len <- t.len + 1

let iter (type k v) (t : (k, v) t) (f : k -> v -> unit) =
  for i = 0 to t.len - 1 do
    f (Obj.obj t.keys.(i) : k) (Obj.obj t.vals.(i) : v)
  done

let iter_with (type a k v) (t : (k, v) t) (f : a -> k -> v -> unit) (arg : a) =
  for i = 0 to t.len - 1 do
    f arg (Obj.obj t.keys.(i) : k) (Obj.obj t.vals.(i) : v)
  done

let count t = t.len
