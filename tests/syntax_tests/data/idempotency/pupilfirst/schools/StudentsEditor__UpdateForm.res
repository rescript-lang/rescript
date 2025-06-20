open StudentsEditor__Types

type teamCoachlist = (string, string, bool)

type view =
  | DetailsTab
  | ActionsTab

let str = ReasonReact.string

let selectedTabClasses = selected =>
  "flex items-center focus:outline-none justify-center w-1/2 p-3 font-semibold rounded-t-lg leading-relaxed border border-gray-400 text-gray-600 cursor-pointer " ++ (
    selected ? "text-primary-500 bg-white border-b-0" : "bg-gray-100"
  )

let tabItemsClasses = selected => selected ? "" : "hidden"

@react.component
let make = (~student, ~team, ~studentTags, ~courseCoaches, ~updateFormCB, ~reloadTeamsCB) => {
  let (view, setView) = React.useState(() => DetailsTab)
  <div className="mx-auto bg-white">
    <div className="pt-6 border-b border-gray-400 bg-gray-100">
      <div className="max-w-2xl mx-auto">
        <div className="flex">
          {switch student->Student.avatarUrl {
          | Some(avatarUrl) => <img className="w-12 h-12 rounded-full mr-4" src=avatarUrl />
          | None => <Avatar name={student->Student.name} className="w-12 h-12 mr-4" />
          }}
          <div className="text-sm flex flex-col justify-center">
            <div className="text-black font-bold inline-block">
              {student->Student.name->str}
            </div>
            <div className="text-gray-600 inline-block"> {student->Student.email->str} </div>
          </div>
        </div>
        <div className="w-full pt-6">
          <div className="flex flex-wrap w-full max-w-3xl mx-auto text-sm px-3 -mb-px">
            <button
              className={selectedTabClasses(view == DetailsTab)}
              onClick={_ => setView(_ => DetailsTab)}>
              <i className="fa fa-edit" /> <span className="ml-2"> {"Details"->str} </span>
            </button>
            <button
              className={"-ml-px " ++ selectedTabClasses(view == ActionsTab)}
              onClick={_ => setView(_ => ActionsTab)}>
              <i className="fa fa-cog" /> <span className="ml-2"> {"Actions"->str} </span>
            </button>
          </div>
        </div>
      </div>
    </div>
    <div className="max-w-2xl mx-auto">
      <div className={tabItemsClasses(view == DetailsTab)}>
        <StudentsEditor__UpdateDetailsForm student team studentTags courseCoaches updateFormCB />
      </div>
      <div className={tabItemsClasses(view == ActionsTab)}>
        <StudentsEditor__ActionsForm student reloadTeamsCB />
      </div>
    </div>
  </div>
}
