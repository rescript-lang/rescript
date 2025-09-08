type t =
  | Preview
  | CourseEnded
  | CourseComplete
  | AccessEnded
  | LevelUp
  | LevelUpBlocked(int)
  | Nothing

@module("../images/course-ended.svg")
external courseEndedImage: string = "default"
@module("../images/course-complete.svg")
external courseCompleteImage: string = "default"
@module("../images/access-ended.svg")
external accessEndedImage: string = "default"
@module("../images/level-up.svg")
external levelUpImage: string = "default"
@module("../images/preview-mode.svg")
external previewModeImage: string = "default"
@module("../images/level-up-blocked.svg")
external levelUpBlockedImage: string = "default"

let icon = t =>
  switch t {
  | Preview => previewModeImage
  | CourseEnded => courseEndedImage
  | CourseComplete => courseCompleteImage
  | AccessEnded => accessEndedImage
  | LevelUp => levelUpImage
  | LevelUpBlocked(_) => levelUpBlockedImage
  | Nothing => ""
  }
