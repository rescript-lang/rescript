let faultyTuple = (a, )

switch faultyTuple {
| (a, ) => ()
| _ => ()
}

type faultyTuple = (string, )
