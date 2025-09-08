type t =
  | Unloaded
  | PartiallyLoaded(array<CoursesStudents__TeamInfo.t>, string)
  | FullyLoaded(array<CoursesStudents__TeamInfo.t>)

let toArray = t =>
  switch t {
  | Unloaded => []
  | PartiallyLoaded(teams, _) => teams
  | FullyLoaded(teams) => teams
  }
