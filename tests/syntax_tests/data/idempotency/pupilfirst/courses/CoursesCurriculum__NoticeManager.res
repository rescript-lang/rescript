open CoursesCurriculum__Types

let str = React.string

let showNotice = (
  ~title,
  ~description,
  ~notice,
  ~classes="max-w-3xl mx-auto text-center mt-4 bg-white lg:rounded-lg shadow-md px-6 pt-6 pb-8",
  (),
) =>
  <div className=classes>
    <img className="h-50 mx-auto" src={notice->Notice.icon} />
    <div className="max-w-xl font-bold text-xl mx-auto mt-2 leading-tight"> {title->str} </div>
    <div className="text-sm max-w-lg mx-auto mt-2"> {description->str} </div>
  </div>

let courseCompletedMessage = () => {
  let title = "Congratulations! You have completed all milestone targets in this course."
  let description = "You've completed our coursework. Feel free to complete targets that you might have left out, and read up on attached links and resources."
  showNotice(~title, ~description, ~notice=Notice.CourseComplete, ())
}

let courseEndedMessage = () => {
  let title = "Course Ended"
  let description = "The course has ended and submissions are disabled for all targets!"
  showNotice(~title, ~description, ~notice=Notice.CourseEnded, ())
}

let showPreviewMessage = () =>
  <div
    className="flex max-w-lg md:mx-auto mx-3 mt-4 rounded-lg px-3 py-2 shadow-lg items-center border border-primary-300 bg-gray-200 ">
    <img className="w-20 md:w-22 flex-no-shrink" src=Notice.previewModeImage />
    <div className="flex-1 text-left ml-4">
      <h4 className="font-bold text-lg leading-tight"> {"Preview Mode"->str} </h4>
      <p className="text-sm mt-1">
        {"You are accessing the preview mode for this course"->str}
      </p>
    </div>
  </div>

let accessEndedMessage = () => {
  let title = "Access Ended"
  let description = "Your access to this course has ended."
  showNotice(~title, ~description, ~notice=Notice.AccessEnded, ())
}

let levelUpBlockedMessage = levelNumber => {
  let title = "Level Up Blocked"
  let currentLevel = levelNumber->string_of_int
  let lastLevel = levelNumber - 1->string_of_int
  let description =
    "You're at Level " ++
    (currentLevel ++
    (", but you have targets in the Level " ++
    (lastLevel ++
    (" that are failed, or are pending review by a coach. You'll need to pass all milestone targets in Level " ++
    (lastLevel ++
    " to continue leveling up.")))))
  showNotice(~title, ~description, ~notice=Notice.LevelUpBlocked(levelNumber), ())
}

let renderLevelUp = course => {
  let title = "Ready to Level Up!"
  let description = "Congratulations! You have successfully completed all milestone targets required to level up. Click the button below to proceed to the next level. New challenges await!"
  <div
    className="max-w-3xl mx-3 lg:mx-auto text-center mt-4 bg-white rounded-lg shadow px-6 pt-4 pb-8">
    {showNotice(~title, ~description, ~notice=Notice.LevelUp, ~classes="", ())}
    <CoursesCurriculum__LevelUpButton course />
  </div>
}

@react.component
let make = (~notice, ~course) =>
  switch notice {
  | Notice.Preview => showPreviewMessage()
  | CourseEnded => courseEndedMessage()
  | CourseComplete => courseCompletedMessage()
  | AccessEnded => accessEndedMessage()
  | LevelUp => renderLevelUp(course)
  | LevelUpBlocked(levelNumber) => levelUpBlockedMessage(levelNumber)
  | Nothing => React.null
  }
