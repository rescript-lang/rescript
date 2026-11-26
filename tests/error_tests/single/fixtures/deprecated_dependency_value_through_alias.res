module O = Belt.Option
module P = O

let kept = P.keepU(Some(1), x => x > 0)
