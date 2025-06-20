open CoursesCurriculum__Types

let str = React.string

module LevelUpQuery = %graphql(`
   mutation LevelUpMutation($courseId: ID!) {
    levelUp(courseId: $courseId){
      success
      }
    }
 `)

let handleSubmitButton = saving => {
  let submitButtonText = (title, iconClasses) =>
    <span> <FaIcon classes={iconClasses ++ " mr-2"} /> {title->str} </span>

  saving
    ? submitButtonText("Saving", "fas fa-spinner fa-spin")
    : submitButtonText("Level Up", "fas fa-flag")
}

let refreshPage = () => {
  open Webapi.Dom
  location->Location.reload
}

let createLevelUpQuery = (course, setSaving, event) => {
  event->ReactEvent.Mouse.preventDefault
  setSaving(_ => true)

  LevelUpQuery.make(~courseId=course->Course.id, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    response["levelUp"]["success"] ? refreshPage() : setSaving(_ => false)
    Js.Promise.resolve()
  })
  ->ignore
}

@react.component
let make = (~course) => {
  let (saving, setSaving) = React.useState(() => false)
  <button
    disabled=saving
    onClick={createLevelUpQuery(course, setSaving)}
    className="btn btn-success btn-large w-full md:w-4/6 mt-4">
    {handleSubmitButton(saving)}
  </button>
}
