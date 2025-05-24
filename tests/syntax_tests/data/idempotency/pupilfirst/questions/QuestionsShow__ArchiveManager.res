let str = React.string

module ArchiveQuery = %graphql(`
   mutation ArchiveCommunityResourceMutation($id: ID!, $resourceType: String!) {
    archiveCommunityResource(id: $id, resourceType: $resourceType){
       success
     }
   }
 `)

let archive = (id, resourceType, archiveCB, setSaving, event) =>
  Webapi.Dom.window->Webapi.Dom.Window.confirm(
    "Are you sure you want to delete this " ++
    ((resourceType->Js.String.toLowerCase) ++
    ". You cannot undo this."),
  )
    ? {
        event->ReactEvent.Mouse.preventDefault
        setSaving(_ => true)
        ArchiveQuery.make(~id, ~resourceType, ())
        ->GraphqlQuery.sendQuery
        ->Js.Promise.then_(response => {
          response["archiveCommunityResource"]["success"]
            ? {
                Notification.success("Success", resourceType ++ " archived successfully")
                archiveCB(id, resourceType)
              }
            : Notification.error("Something went wrong", "Please refresh the page and try again")
          Js.Promise.resolve()
        })
        ->ignore
      }
    : ()

@react.component
let make = (~id, ~resourceType, ~archiveCB) => {
  let (saving, setSaving) = React.useState(() => false)
  <a
    title={"Archive " ++ resourceType}
    onClick={archive(id, resourceType, archiveCB, setSaving)}
    className="flex items-center justify-center whitespace-no-wrap text-xs font-semibold py-1 px-3 flex-shrink-0 bg-transparent text-gray-700 hover:bg-red-100 hover:text-red-700 cursor-pointer">
    {saving ? <FaIcon classes="fas fa-spinner fa-spin" /> : <FaIcon classes="fas fa-trash-alt" />}
    {resourceType == "Comment" ? React.null : <span className="ml-1"> {"Delete"->str} </span>}
  </a>
}
