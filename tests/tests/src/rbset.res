type elt = int
type color = Black | Red
type rec t =
  | Node(color, t, elt, t)
  | Empty

type rec enum =
  | End
  | More(elt, t, enum)

let blackify = x =>
  switch x {
  | Node(Red, l, x, r) => (Node(Black, l, x, r), false)
  | s => (s, true)
  }

let empty = Empty

let is_empty = x =>
  switch x {
  | Empty => true
  | _ => false
  }

let rec mem = (x, x_) =>
  switch x_ {
  | Empty => false
  | Node(_, l, y, r) =>
    if x == y {
      true
    } else if x < y {
      mem(x, l)
    } else {
      mem(x, r)
    }
  }

let balance_left = (l, x, r) =>
  switch (l, x, r) {
  | (Node(Red, Node(Red, a, x, b), y, c), z, d)
  | (Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
    Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
  | (l, x, r) => Node(Black, l, x, r)
  }

let balance_right = (l, x, r) =>
  switch (l, x, r) {
  | (a, x, Node(Red, Node(Red, b, y, c), z, d))
  | (a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
    Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
  | (l, x, r) => Node(Black, l, x, r)
  }

let singleton = x => Node(Black, Empty, x, Empty)

let unbalanced_left = x =>
  switch x {
  | Node(Red, Node(Black, a, x, b), y, c) => (balance_left(Node(Red, a, x, b), y, c), false)
  | Node(Black, Node(Black, a, x, b), y, c) => (balance_left(Node(Red, a, x, b), y, c), true)
  | Node(Black, Node(Red, a, x, Node(Black, b, y, c)), z, d) => (
      Node(Black, a, x, balance_left(Node(Red, b, y, c), z, d)),
      false,
    )
  | _ => assert(false)
  }

let unbalanced_right = x =>
  switch x {
  | Node(Red, a, x, Node(Black, b, y, c)) => (balance_right(a, x, Node(Red, b, y, c)), false)
  | Node(Black, a, x, Node(Black, b, y, c)) => (balance_right(a, x, Node(Red, b, y, c)), true)
  | Node(Black, a, x, Node(Red, Node(Black, b, y, c), z, d)) => (
      Node(Black, balance_right(a, x, Node(Red, b, y, c)), z, d),
      false,
    )
  | _ => assert(false)
  }

let lbalance = (x1, x2, x3) =>
  switch x1 {
  | Node(Red, Node(Red, a, x, b), y, c) =>
    Node(Red, Node(Black, a, x, b), y, Node(Black, c, x2, x3))
  | Node(Red, a, x, Node(Red, b, y, c)) =>
    Node(Red, Node(Black, a, x, b), y, Node(Black, c, x2, x3))
  | _ => Node(Black, x1, x2, x3)
  }

let lbalance = (x1, x2, x3) =>
  switch x1 {
  | Node(Red, l, y, r) =>
    switch (l, r) {
    | (Node(Red, a, x, b), _) => Node(Red, Node(Black, a, x, b), y, Node(Black, r, x2, x3))
    | (_, Node(Red, b, y, c)) => Node(Red, Node(Black, l, y, b), y, Node(Black, c, x2, x3))
    | _ => Node(Black, x1, x2, x3)
    }
  | _ => Node(Black, x1, x2, x3)
  }
let rbalance = (x1, x2, x3) =>
  switch (x1, x2, x3) {
  | (a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
    Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
  | (a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
    Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
  | (a, x, b) => Node(Black, a, x, b)
  }

let rec ins = (x, x_) =>
  switch x_ {
  | Empty => Node(Red, Empty, x, Empty)
  | Node(Red, a, y, b) as s =>
    if x == y {
      s
    } else if x < y {
      Node(Red, ins(x, a), y, b)
    } else {
      Node(Red, a, y, ins(x, b))
    }

  | Node(Black, a, y, b) as s =>
    if x == y {
      s
    } else if x < y {
      lbalance(ins(x, a), y, b)
    } else {
      rbalance(a, y, ins(x, b))
    }
  }

let add = (x, s) =>
  switch ins(x, s) {
  | Node(Red, a, y, b) => Node(Black, a, y, b)
  | s => s
  }

let rec remove_min = x =>
  switch x {
  | Empty
  | Node(Black, Empty, _, Node(Black, _, _, _)) =>
    assert(false)
  | Node(Black, Empty, x, Empty) => (Empty, x, true)
  | Node(Black, Empty, x, Node(Red, l, y, r)) => (Node(Black, l, y, r), x, false)
  | Node(Red, Empty, x, r) => (r, x, false)
  | Node(c, l, x, r) =>
    let (l, y, d) = remove_min(l)
    let s = Node(c, l, x, r)
    if d {
      let (s, d) = unbalanced_right(s)
      (s, y, d)
    } else {
      (s, y, false)
    }
  }

let rec remove_aux = (x, n) =>
  switch n {
  | Empty => (Empty, false)
  | Node(c, l, y, r) =>
    if x == y {
      switch r {
      | Empty =>
        if c == Red {
          (l, false)
        } else {
          blackify(l)
        }
      | _ =>
        let (r, y, d) = remove_min(r)
        let n = Node(c, l, y, r)
        if d {
          unbalanced_left(n)
        } else {
          (n, false)
        }
      }
    } else if x < y {
      let (l, d) = remove_aux(x, l)
      let n = Node(c, l, y, r)
      if d {
        unbalanced_right(n)
      } else {
        (n, false)
      }
    } else {
      let (r, d) = remove_aux(x, r)
      let n = Node(c, l, y, r)
      if d {
        unbalanced_left(n)
      } else {
        (n, false)
      }
    }
  }

let remove = (x, s) => fst(remove_aux(x, s))

let rec cardinal = x =>
  switch x {
  | Empty => 0
  | Node(_, l, x, r) => 1 + cardinal(l) + cardinal(r)
  }
