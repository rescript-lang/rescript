open CoursesStudents__Types

let str = React.string

type state = {archiving: bool}

module ArchiveCoachNoteMutation = %graphql(`
   mutation ArchiveCoachNoteMutation($id: ID!) {
    archiveCoachNote(id: $id) {
       success
      }
    }
   `)

let removeCoachNote = (id, removeNoteCB, setArchiving, event) => {
  event->ReactEvent.Mouse.preventDefault
  setArchiving(_ => true)
  if {
    open Webapi.Dom
    window->Window.confirm("Are you sure you want to delete this note?")
  } {
    ArchiveCoachNoteMutation.make(~id, ())
    ->GraphqlQuery.sendQuery
    ->Js.Promise.then_(response => {
      if response["archiveCoachNote"]["success"] {
        removeNoteCB(id)
      } else {
        setArchiving(_ => false)
      }
      Js.Promise.resolve()
    })
    ->ignore
  } else {
    ()
  }
}

let deleteIcon = (note, removeCoachNote, removeNoteCB, setArchiving, archiving) =>
  <button
    className="w-10 text-sm text-gray-700 hover:text-gray-900 cursor-pointer flex items-center justify-center rounded hover:bg-gray-200 focus:outline-none "
    disabled=archiving
    title={"Delete " ++ (note->CoachNote.id)}
    onClick={removeCoachNote(note->CoachNote.id, removeNoteCB, setArchiving)}>
    <FaIcon classes={archiving ? "fas fa-spinner fa-spin" : "fas fa-trash-alt"} />
  </button>

@react.component
let make = (~note, ~userId, ~removeNoteCB) => {
  let (archiving, setArchiving) = React.useState(() => false)
  <div className="mt-4" key={note->CoachNote.id} ariaLabel={"Note " ++ (note->CoachNote.id)}>
    <div className="flex justify-between">
      <div className="flex">
        {switch note->CoachNote.author {
        | Some(user) =>
          switch user->User.avatarUrl {
          | Some(avatarUrl) =>
            <img
              className="w-8 h-8 md:w-10 md:h-10 text-xs border border-gray-400 rounded-full overflow-hidden flex-shrink-0 mt-1 md:mt-0 mr-2 md:mr-3 object-cover"
              src=avatarUrl
            />
          | None =>
            <Avatar
              name={user->User.name}
              className="w-8 h-8 md:w-10 md:h-10 text-xs border border-gray-400 rounded-full overflow-hidden flex-shrink-0 mt-1 md:mt-0 mr-2 md:mr-3 object-cover"
            />
          }

        | None =>
          <Avatar
            name="?"
            className="w-8 h-8 md:w-10 md:h-10 text-xs border rounded-full overflow-hidden flex-shrink-0 mt-1 md:mt-0 mr-2 md:mr-3 object-cover"
          />
        }}
        <div>
          <p className="text-sm font-semibold inline-block leading-snug">
            {switch note->CoachNote.author {
            | Some(user) => user->User.name
            | None => "Deleted Coach"
            }->str}
          </p>
          <p className="text-gray-600 font-semibold text-xs mt-px leading-snug">
            {switch note->CoachNote.author {
            | Some(user) => user->User.title
            | None => "Unknown"
            }->str}
          </p>
        </div>
      </div>
      {
        let showDeleteIcon = switch note->CoachNote.author {
        | None => false
        | Some(user) => User.id(user) == userId
        }
        showDeleteIcon
          ? deleteIcon(note, removeCoachNote, removeNoteCB, setArchiving, archiving)
          : React.null
      }
    </div>
    <div className="ml-10 md:ml-13 mt-2">
      <p
        className="inline-block text-xs font-semibold leading-tight bg-gray-300 text-gray-800 mt-px px-1 py-px rounded">
        {"on " ++ (note->CoachNote.noteOn)->str}
      </p>
      <MarkdownBlock
        className="pt-1 text-sm" profile=Markdown.Permissive markdown={note->CoachNote.note}
      />
    </div>
  </div>
}
