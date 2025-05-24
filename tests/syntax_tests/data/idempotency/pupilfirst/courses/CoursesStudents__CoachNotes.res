%raw(`require("./CoursesStudents__StudentOverlay.css")`)

open CoursesStudents__Types

type state = {
  newNote: string,
  saving: bool,
}

let str = React.string

module CreateCoachNotesMutation = %graphql(`
   mutation CreateCoachNoteMutation($studentId: ID!, $note: String!) {
    createCoachNote(studentId: $studentId, note: $note ) {
       coachNote {
         id
         note
         createdAt
         author {
            id
            avatarUrl
            name
            title
         }
       }
      }
    }
  `)

let saveNote = (studentId, setState, state, addNoteCB) => {
  setState(state => {...state, saving: true})
  CreateCoachNotesMutation.make(~studentId, ~note=state.newNote, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    switch response["createCoachNote"]["coachNote"] {
    | Some(note) =>
      let newNote = CoachNote.makeFromJs(note)
      addNoteCB(newNote)
      setState(_ => {newNote: "", saving: false})
    | None => setState(state => {...state, saving: false})
    }
    Js.Promise.resolve()
  })
  ->Js.Promise.catch(_error => {
    setState(state => {...state, saving: false})
    Js.Promise.resolve()
  })
  ->ignore
}

let updateCoachNoteCB = (setState, newNote) => setState(state => {...state, newNote: newNote})

let saveNoteButtonText = (title, iconClasses) =>
  <span> <FaIcon classes={iconClasses ++ " mr-2"} /> {title->str} </span>

@react.component
let make = (~studentId, ~coachNotes, ~addNoteCB, ~removeNoteCB, ~userId) => {
  let (state, setState) = React.useState(() => {newNote: "", saving: false})
  <div className="mt-3 text-sm">
    <label
      htmlFor="course-students__coach-notes-new-note" className="font-semibold text-sm block mb-1">
      {"Add a New Note"->str}
    </label>
    <DisablingCover disabled=state.saving message="Saving...">
      <MarkdownEditor
        textareaId="course-students__coach-notes-new-note"
        onChange={updateCoachNoteCB(setState)}
        value=state.newNote
        profile=Markdown.Permissive
        maxLength=10000
      />
    </DisablingCover>
    <button
      disabled={state.newNote->String.length < 1 || state.saving}
      onClick={_ => saveNote(studentId, setState, state, addNoteCB)}
      className="btn btn-primary mt-2">
      {state.saving
        ? saveNoteButtonText("Saving", "fas fa-spinner")
        : saveNoteButtonText("Save Note", "")}
    </button>
    <div>
      <h6 className="font-semibold mt-6"> {"All Notes"->str} </h6>
      {coachNotes->ArrayUtils.isEmpty
        ? <div
            className="bg-gray-200 rounded text-center p-4 md:p-6 items-center justify-center mt-2">
            <i className="fas fa-sticky-note text-gray-400 text-4xl" />
            <p className="text-xs font-semibold text-gray-700 mt-2"> {"No notes here!"->str} </p>
          </div>
        : React.null}
      {coachNotes
      ->CoachNote.sort
      ->Array.map(note =>
        <CoursesStudents__CoachNoteShow key={note->CoachNote.id} note userId removeNoteCB />
      )
      ->React.array}
    </div>
  </div>
}
