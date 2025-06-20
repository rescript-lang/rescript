module Level = CoursesReview__Level

type filter = {
  level: option<Level.t>,
  coach: option<UserProxy.t>,
}

let makeFilter = (level, coach) => {level: level, coach: coach}

let filterLevelId = level => level->OptionUtils.mapWithDefault(Level.id, "none")
let filterCoachId = coach => coach->OptionUtils.mapWithDefault(UserProxy.id, "none")

let filterEq = (level, coach, filter) =>
  filter.level->filterLevelId == filterLevelId(level) &&
    filter.coach->filterCoachId == filterCoachId(coach)

type sortDirection = [#Ascending | #Descending]

type data = {
  submissions: array<CoursesReview__IndexSubmission.t>,
  filter: filter,
  sortDirection: sortDirection,
  totalCount: int,
}

type cursor = string

type t =
  | Unloaded
  | PartiallyLoaded(data, cursor)
  | FullyLoaded(data)

let totalCount = t =>
  switch t {
  | Unloaded => None
  | PartiallyLoaded({totalCount}, _)
  | FullyLoaded({totalCount}) =>
    Some(totalCount)
  }

let unloaded = Unloaded

let partiallyLoaded = (
  ~submissions,
  ~filter,
  ~sortDirection,
  ~totalCount,
  ~cursor,
) => PartiallyLoaded(
  {submissions: submissions, filter: filter, sortDirection: sortDirection, totalCount: totalCount},
  cursor,
)

let fullyLoaded = (~submissions, ~filter, ~sortDirection, ~totalCount) => FullyLoaded({
  submissions: submissions,
  filter: filter,
  sortDirection: sortDirection,
  totalCount: totalCount,
})

let needsReloading = (selectedLevel, selectedCoach, sortDirection, t) =>
  switch t {
  | Unloaded => true
  | FullyLoaded(data)
  | PartiallyLoaded(data, _) =>
    !(data.filter->filterEq(selectedLevel, selectedCoach) && data.sortDirection == sortDirection)
  }

let toArray = t =>
  switch t {
  | Unloaded => []
  | PartiallyLoaded(data, _cursor) => data.submissions
  | FullyLoaded(data) => data.submissions
  }
