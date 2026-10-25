let foo = () =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x =>
  bar(x => x)
  )))))))))))))))))))))

let drawLines = map =>
  List.iteri((y, row) => List.iteri((x, tile) =>
      switch tile {
      | Player(moves) =>
        let positions = List.fold_left(
          (acc, move) =>
            switch acc {
            | list{prevPoint, ...rest} => list{Point.add(delta(move), prevPoint), prevPoint, ...rest}
            | _ => assert(false)
            },
          list{Point.create(x, y)},
          moves,
        )
        let _ = List.fold_left((acc, pos) =>
          switch acc {
          | None => Some(pos)
          | Some(prevPos) => Some(prevPos)
          }
        , None, positions)
      | _ => ()
      }
    , row), map)
