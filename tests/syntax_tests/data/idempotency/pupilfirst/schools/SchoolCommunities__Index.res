let str = React.string

open SchoolCommunities__IndexTypes

type editorAction =
  | ShowEditor(option<Community.t>)
  | Hidden

@react.component
let make = (~communities, ~courses, ~connections) => {
  let (editorAction, setEditorAction) = React.useState(() => Hidden)
  let (stateConnections, setStateConnections) = React.useState(() => connections)
  let (stateCommunities, setStateCommunities) = React.useState(() => communities)

  let updateCommunitiesCB = (community, connections) => {
    setStateCommunities(_ => stateCommunities->Community.updateList(community))
    setStateConnections(_ => connections)
    setEditorAction(_ => Hidden)
  }

  let addCommunityCB = (community, connections) => {
    setStateCommunities(_ => communities->List.append(list{community}))
    setStateConnections(_ => connections)
    setEditorAction(_ => Hidden)
  }
  <div className="flex-1 flex flex-col overflow-y-scroll bg-gray-200">
    {switch editorAction {
    | Hidden => React.null
    | ShowEditor(community) =>
      <SchoolAdmin__EditorDrawer closeDrawerCB={() => setEditorAction(_ => Hidden)}>
        <SchoolCommunities__Editor
          courses community connections=stateConnections addCommunityCB updateCommunitiesCB
        />
      </SchoolAdmin__EditorDrawer>
    }}
    <div className="flex px-6 py-2 items-center justify-between">
      <button
        onClick={_ => setEditorAction(_ => ShowEditor(None))}
        className="max-w-2xl w-full flex mx-auto items-center justify-center relative bg-white text-primary-500 hover:bg-gray-100 hover:text-primary-600 hover:shadow-lg focus:outline-none border-2 border-gray-400 border-dashed hover:border-primary-300 p-6 rounded-lg mt-8 cursor-pointer">
        <i className="fas fa-plus-circle" />
        <h5 className="font-semibold ml-2"> {"Add New Community"->str} </h5>
      </button>
    </div>
    <div className="px-6 pb-4 mt-5 flex flex-1">
      <div className="max-w-2xl w-full mx-auto relative">
        {stateCommunities
        ->List.map(community =>
          <div
            key={community->Community.id}
            className="flex items-center shadow bg-white rounded-lg mb-4">
            <div className="course-faculty__list-item flex w-full items-center">
              <a
                onClick={_event => {
                  ReactEvent.Mouse.preventDefault(_event)
                  setEditorAction(_ => ShowEditor(Some(community)))
                }}
                className="course-faculty__list-item-details flex flex-1 items-center justify-between border border-transparent cursor-pointer rounded-l-lg hover:bg-gray-100 hover:text-primary-500 hover:border-primary-400">
                <div className="flex w-full text-sm justify-between">
                  <span className="flex-1 font-semibold py-5 px-5">
                    {community->Community.name->str}
                  </span>
                  <span
                    className="ml-2 py-5 px-5 font-semibold text-gray-700 hover:text-primary-500">
                    <i className="fas fa-edit text-normal" />
                    <span className="ml-1"> {"Edit"->str} </span>
                  </span>
                </div>
              </a>
              <a
                target="_blank"
                href={"/communities/" ++ (community->Community.id)}
                className="text-sm flex items-center border-l text-gray-700 hover:bg-gray-100 hover:text-primary-500 font-semibold px-5 py-5">
                <i className="fas fa-external-link-alt text-normal" />
                <span className="ml-1"> {"View"->str} </span>
              </a>
            </div>
          </div>
        )
        ->Array.of_list
        ->ReasonReact.array}
      </div>
    </div>
  </div>
}
