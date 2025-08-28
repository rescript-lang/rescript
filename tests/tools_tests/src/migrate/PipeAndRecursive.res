module Target = {
  let a = x => x + 1
  let b = x => x + 2
}

@deprecated({
  reason: "test piped vs non-piped",
  migrate: PipeAndRecursive.Target.a(),
  migrateInPipeChain: PipeAndRecursive.Target.b(),
})
external dep: int => int = "dep"

let id = x => x

/* Should use migrate (Target.a), since lhs has 0 pipes */
let onePipe = 1->dep

/* Still migrate (Target.a), since lhs has 1 pipe (< 2) */
let twoPipes = 1->id->dep

/* Should use migrateInPipeChain (Target.b), since lhs has 2 pipes */
let threePipes = 1->id->id->dep

/* Recursion: all dep steps should migrate */
let many = 1->dep->dep->dep->dep->dep->dep->dep->dep->dep->dep
