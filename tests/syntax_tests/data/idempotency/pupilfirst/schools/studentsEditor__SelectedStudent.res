type t = {
  name: string,
  id: string,
  teamId: string,
  avatarUrl: option<string>,
  levelId: string,
  teamSize: int,
}

let id = t => t.id

let avatarUrl = t => t.avatarUrl

let name = t => t.name

let selectedAcrossTeams = selectedStudents =>
  selectedStudents->Array.map(s => s.teamId)->ArrayUtils.distinct->Array.length > 1

let partOfTeamSelected = selectedStudents => {
  let selectedTeamSize = selectedStudents->Array.length

  selectedStudents
  ->Js.Array.filter(s => s.teamSize > selectedTeamSize)
  ->Array.length == selectedTeamSize
}

let selectedWithinLevel = selectedStudents =>
  selectedStudents
  ->Array.map(s => s.levelId)
  ->ArrayUtils.sort_uniq(String.compare)
  ->Array.length == 1

let isGroupable = selectedStudents =>
  if selectedStudents->Array.length > 1 {
    (selectedWithinLevel(selectedStudents) && selectedAcrossTeams(selectedStudents)) ||
      partOfTeamSelected(selectedStudents)
  } else {
    false
  }

let isMoveOutable = selectedStudents =>
  selectedStudents->Array.length == 1 && selectedStudents->Array.map(s => s.teamSize) != [1]

let make = (~name, ~id, ~teamId, ~avatarUrl, ~levelId, ~teamSize) => {
  name: name,
  id: id,
  teamId: teamId,
  avatarUrl: avatarUrl,
  levelId: levelId,
  teamSize: teamSize,
}
