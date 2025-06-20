type rec t =
  | Unloaded
  | PartiallyLoaded(array<StudentsEditor__Team.t>, cursor)
  | FullyLoaded(array<StudentsEditor__Team.t>)
and cursor = string

let updateTeam = (team, t) =>
  switch t {
  | Unloaded => Unloaded
  | PartiallyLoaded(teams, cursor) =>
    PartiallyLoaded(teams->StudentsEditor__Team.replaceTeam(team), cursor)
  | FullyLoaded(teams) => FullyLoaded(teams->StudentsEditor__Team.replaceTeam(team))
  }

let teams = t =>
  switch t {
  | Unloaded => []
  | PartiallyLoaded(teams, _cursor) => teams
  | FullyLoaded(teams) => teams
  }
