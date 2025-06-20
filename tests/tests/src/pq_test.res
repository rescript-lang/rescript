module PrioQueue = {
  type priority = int
  type rec queue<'a> = Empty | Node(priority, 'a, queue<'a>, queue<'a>)
  let empty = Empty
  let rec insert = (queue, prio, elt) =>
    switch queue {
    | Empty => Node(prio, elt, Empty, Empty)
    | Node(p, e, left, right) =>
      if prio <= p {
        Node(prio, elt, insert(right, p, e), left)
      } else {
        Node(p, e, insert(right, prio, elt), left)
      }
    }
  exception Queue_is_empty
  let rec remove_top = x =>
    switch x {
    | Empty => throw(Queue_is_empty)
    | Node(prio, elt, left, Empty) => left
    | Node(prio, elt, Empty, right) => right
    | Node(prio, elt, Node(lprio, lelt, _, _) as left, Node(rprio, relt, _, _) as right) =>
      if lprio <= rprio {
        Node(lprio, lelt, remove_top(left), right)
      } else {
        Node(rprio, relt, left, remove_top(right))
      }
    }
  let extract = x =>
    switch x {
    | Empty => throw(Queue_is_empty)
    | Node(prio, elt, _, _) as queue => (prio, elt, remove_top(queue))
    }
}
