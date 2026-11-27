@@config({flags: ["-w", "a"]})

module T0 = {
  let myList = list{1, 2}

  let list{head, ...tail} = myList
}

module T1 = {
  let myList = list{(1, 2, 3)}

  let list{h0, h1, ...h2} = myList
}
