// ocaml style list with array syntax
let flags = reasonFormat ? {
  let parts = Utils.split_on_char(' ', flags)
  let rec loop = (items) => {
    switch(items) {
      | ["-pp", _ppFlag, ...rest] => loop(rest)
      | [x, ...rest] => [x, ...loop(rest)]
      | [] => []
    }
  };
  loop(parts)->String.concat(" ")
} : flags
