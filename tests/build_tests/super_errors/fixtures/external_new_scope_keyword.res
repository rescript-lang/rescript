type t

@new @scope("default") external make: unit => t = "Klass"

let k = make()
