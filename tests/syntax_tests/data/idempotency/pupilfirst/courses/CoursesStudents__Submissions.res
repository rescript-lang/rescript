type rec t =
  | Unloaded
  | PartiallyLoaded(array<CoursesStudents__Submission.t>, cursor)
  | FullyLoaded(array<CoursesStudents__Submission.t>)
and cursor = string
