open Belt

exception Foo

let rec test = n =>
  if n == 0 {
    throw(Foo)
  } else {
    try test(n - 1) catch {
    | Foo => ()
    }
  }

let () = test(100)

type in_channel
@val external input_line: in_channel => string = "input_line"

let read_lines = inc => {
  let rec loop = acc =>
    switch try Some(input_line(inc)) catch {
    | End_of_file => None
    } {
    | Some(l) => loop(list{l, ...acc})
    | None => List.reverse(acc)
    }

  loop(list{})
}

let read_lines2 = inc => {
  let rec loop = acc =>
    switch input_line(inc) {
    | l => loop(list{l, ...acc})
    | exception End_of_file => List.reverse(acc)
    }

  loop(list{})
}

let read_lines3 = inc => {
  let rec loop = acc =>
    try {
      let l = input_line(inc)
      loop(list{l, ...acc})
    } catch {
    | End_of_file => List.reverse(acc)
    }

  loop(list{})
}

let rec fff = (f, x) =>
  try fff(f, x) catch {
  | _ => x + 1
  }
