open CourseExports__Types

let str = React.string

type state = {
  drawerOpen: bool,
  selectedTags: array<Tag.t>,
  saving: bool,
  reviewedOnly: bool,
  tagSearch: string,
  courseExports: array<CourseExport.t>,
  exportType: CourseExport.exportType,
}

let computeInitialState = exports => {
  drawerOpen: false,
  selectedTags: [],
  saving: false,
  reviewedOnly: false,
  tagSearch: "",
  courseExports: exports,
  exportType: CourseExport.Students,
}

type action =
  | OpenDrawer
  | CloseDrawer
  | BeginSaving
  | FinishSaving(CourseExport.t)
  | FailSaving
  | SetReviewedOnly(bool)
  | SelectTag(Tag.t)
  | DeselectTag(Tag.t)
  | SelectExportType(CourseExport.exportType)
  | UpdateTagSearch(string)

let reducer = (state, action) =>
  switch action {
  | OpenDrawer => {...state, drawerOpen: true}
  | CloseDrawer => {...state, drawerOpen: false}
  | BeginSaving => {...state, saving: true}
  | FinishSaving(courseExport) => {
      ...state,
      saving: false,
      courseExports: state.courseExports->Array.append([courseExport]),
      drawerOpen: false,
    }
  | FailSaving => {...state, saving: false}
  | SetReviewedOnly(reviewedOnly) => {...state, reviewedOnly: reviewedOnly}
  | SelectTag(tag) => {
      ...state,
      selectedTags: state.selectedTags->Array.append([tag]),
    }
  | DeselectTag(tag) => {
      ...state,
      selectedTags: state.selectedTags->Js.Array.filter(t => t->Tag.id != Tag.id(tag)),
    }
  | SelectExportType(exportType) => {...state, exportType: exportType}
  | UpdateTagSearch(tagSearch) => {...state, tagSearch: tagSearch}
  }

let readinessString = courseExport =>
  switch courseExport->CourseExport.file {
  | None =>
    let timeDistance =
      courseExport
      ->CourseExport.createdAt
      ->DateFns.parseString
      ->DateFns.distanceInWordsToNow(~addSuffix=true)
    "Requested " ++ timeDistance
  | Some(file) =>
    let timeDistance =
      file
      ->CourseExport.fileCreatedAt
      ->DateFns.parseString
      ->DateFns.distanceInWordsToNow(~addSuffix=true)
    "Prepared " ++ timeDistance
  }

module Selectable = {
  type t = Tag.t
  let value = t => t->Tag.name
  let searchString = t => t->Tag.name
}

module TagsSelector = MultiselectInline.Make(Selectable)

let unselected = (allTags, selectedTags) => {
  let selectedTagIds = selectedTags->Array.map(Tag.id)
  allTags->Js.Array.filter(t => !(selectedTagIds->Array.mem(t->Tag.id)))
}

module CreateCourseExportQuery = %graphql(`
 mutation CreateCourseExportMutation ($courseId: ID!, $tagIds: [ID!]!, $reviewedOnly: Boolean!, $exportType: Export!) {
  createCourseExport(courseId: $courseId, tagIds: $tagIds, reviewedOnly: $reviewedOnly, exportType: $exportType){
    courseExport {
      id
      createdAt
      tags
      reviewedOnly
    }
   }
 }
`)

let createCourseExport = (state, send, course, event) => {
  event->ReactEvent.Mouse.preventDefault
  send(BeginSaving)

  let (tagIds, exportType) = switch state.exportType {
  | CourseExport.Students => (state.selectedTags->Array.map(Tag.id), #Students)
  | Teams => ([], #Teams)
  }

  CreateCourseExportQuery.make(
    ~courseId=course->Course.id,
    ~tagIds,
    ~reviewedOnly=state.reviewedOnly,
    ~exportType,
    (),
  )
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    switch response["createCourseExport"]["courseExport"] {
    | Some(\"export") =>
      /* Add the new course export to the list of exports known by this component. */
      let courseExport = CourseExport.make(
        ~id=\"export"["id"],
        ~exportType=state.exportType,
        ~createdAt=\"export"["createdAt"],
        ~tags=\"export"["tags"],
        ~reviewedOnly=\"export"["reviewedOnly"],
      )

      send(FinishSaving(courseExport))
    | None => send(FailSaving)
    }

    Js.Promise.resolve()
  })
  ->Js.Promise.catch(e => {
    Js.log(e)
    send(FailSaving)
    Js.Promise.resolve()
  })
  ->ignore
}

let toggleChoiceClasses = value => {
  let defaultClasses = "relative flex flex-col items-center bg-white border border-gray-400 hover:bg-gray-200 text-sm font-semibold focus:outline-none rounded p-4 w-full"
  value
    ? defaultClasses ++ " bg-gray-200 text-primary-500 border-primary-500"
    : defaultClasses ++ " opacity-75 text-gray-900"
}

@react.component
let make = (~course, ~exports, ~tags) => {
  let (state, send) = React.useReducerWithMapState(reducer, exports, computeInitialState)

  <div key="School admin coaches course index" className="flex flex-1 h-screen overflow-y-scroll">
    {state.drawerOpen
      ? <SchoolAdmin__EditorDrawer
          closeDrawerCB={() => send(CloseDrawer)} closeButtonTitle="Close Export Form">
          <div className="mx-auto bg-white">
            <div className="max-w-2xl pt-6 px-6 mx-auto">
              <h5 className="uppercase text-center border-b border-gray-400 pb-2">
                {"Export course data"->str}
              </h5>
              <div className="mt-4">
                <label className="block tracking-wide text-xs font-semibold mr-6 mb-2">
                  {"Please select the kind of export you need:"->str}
                </label>
                <div className="flex -mx-2">
                  <div className="w-1/2 px-2">
                    <button
                      onClick={_ => send(SelectExportType(CourseExport.Students))}
                      className={toggleChoiceClasses(state.exportType == CourseExport.Students)}>
                      <i className="fas fa-user" />
                      <div className="mt-1"> {"Students"->str} </div>
                    </button>
                  </div>
                  <div className="w-1/2 px-2">
                    <button
                      onClick={_ => send(SelectExportType(CourseExport.Teams))}
                      className={toggleChoiceClasses(state.exportType == CourseExport.Teams)}>
                      <i className="fas fa-user-friends" />
                      <div className="mt-1"> {"Teams"->str} </div>
                    </button>
                  </div>
                </div>
              </div>
              {switch state.exportType {
              | CourseExport.Students =>
                <div className="mt-4">
                  <label className="block tracking-wide text-xs font-semibold mb-2">
                    {"Export only students with the following tags:"->str}
                  </label>
                  <TagsSelector
                    placeholder="Search for a tag"
                    emptySelectionMessage="No tags selected"
                    selected=state.selectedTags
                    unselected={unselected(tags, state.selectedTags)}
                    onChange={tagSearch => send(UpdateTagSearch(tagSearch))}
                    value=state.tagSearch
                    onSelect={tag => send(SelectTag(tag))}
                    onDeselect={tag => send(DeselectTag(tag))}
                  />
                </div>
              | Teams => React.null
              }}
              <div className="mt-5">
                <label
                  className="block tracking-wide text-xs font-semibold mr-6 mb-2"
                  htmlFor="targets_filter">
                  {"Which targets should the export include?"->str}
                </label>
                <div id="targets_filter" className="flex -mx-2">
                  <div className="w-1/2 px-2">
                    <button
                      onClick={_ => send(SetReviewedOnly(false))}
                      className={toggleChoiceClasses(!state.reviewedOnly)}>
                      <i className="fas fa-list" />
                      <div className="mt-1"> {"All targets"->str} </div>
                    </button>
                  </div>
                  <div className="w-1/2 px-2">
                    <button
                      onClick={_event => send(SetReviewedOnly(true))}
                      className={toggleChoiceClasses(state.reviewedOnly)}>
                      <i className="fas fa-tasks" />
                      <div className="mt-1">
                        {"Only targets with reviewed submissions"->str}
                      </div>
                    </button>
                  </div>
                </div>
              </div>
              <div className="flex max-w-2xl w-full mt-5 pb-5 mx-auto">
                <button
                  disabled=state.saving
                  className="w-full btn btn-primary btn-large"
                  onClick={createCourseExport(state, send, course)}>
                  {if state.saving {
                    <span>
                      <FaIcon classes="fas fa-spinner fa-pulse" />
                      <span className="ml-2"> {"Setting up an export..."->str} </span>
                    </span>
                  } else {
                    "Create Export"->str
                  }}
                </button>
              </div>
            </div>
          </div>
        </SchoolAdmin__EditorDrawer>
      : React.null}
    <div className="flex-1 flex flex-col bg-gray-100">
      <div className="flex px-6 py-2 items-center justify-between">
        <button
          onClick={_ => send(OpenDrawer)}
          className="max-w-2xl w-full flex mx-auto items-center justify-center relative bg-white text-primary-500 hover:bg-gray-100 hover:text-primary-600 hover:shadow-lg focus:outline-none border-2 border-gray-400 border-dashed hover:border-primary-300 p-6 rounded-lg mt-20 cursor-pointer">
          <i className="fas fa-file-export text-lg" />
          <h5 className="font-semibold ml-2"> {"Create a new export"->str} </h5>
        </button>
      </div>
      {state.courseExports->ArrayUtils.isEmpty
        ? <div
            className="flex justify-center bg-gray-100 border rounded p-3 italic mx-auto max-w-2xl w-full">
            {"You haven't exported anything yet!"->str}
          </div>
        : <div className="px-6 pb-4 mt-5 flex flex-1 bg-gray-100">
            <div className="max-w-2xl w-full mx-auto relative">
              <h4 className="mt-5 w-full"> {"Exports"->str} </h4>
              <div className="flex mt-4 -mx-3 items-start flex-wrap">
                {state.courseExports
                ->ArrayUtils.copyAndSort((x, y) =>
                  DateFns.differenceInSeconds(
                    y->CourseExport.createdAt->DateFns.parseString,
                    x->CourseExport.createdAt->DateFns.parseString,
                  )->int_of_float
                )
                ->Array.map(courseExport =>
                  <div
                    key={courseExport->CourseExport.id}
                    ariaLabel={"Export " ++ (courseExport->CourseExport.id)}
                    className="flex w-1/2 items-center mb-4 px-3">
                    <div
                      className="course-faculty__list-item shadow bg-white overflow-hidden rounded-lg flex flex-col w-full">
                      <div className="flex flex-1 justify-between">
                        <div className="pt-4 pb-3 px-4">
                          <div className="text-sm">
                            <p className="text-black font-semibold">
                              {courseExport->CourseExport.exportTypeToString->str}
                            </p>
                            <p className="text-gray-600 font-semibold text-xs mt-px">
                              {courseExport->readinessString->str}
                            </p>
                          </div>
                          <div className="flex flex-wrap text-gray-600 font-semibold text-xs mt-1">
                            {courseExport->CourseExport.reviewedOnly
                              ? <span
                                  className="px-2 py-1 border rounded bg-secondary-100 text-primary-600 mt-1 mr-1">
                                  {"Reviewed Submissions Only"->str}
                                </span>
                              : React.null}
                            {switch courseExport->CourseExport.exportType {
                            | Teams => ReasonReact.null
                            | Students =>
                              courseExport
                              ->CourseExport.tags
                              ->Array.map(tag =>
                                <span
                                  key=tag
                                  className="px-2 py-1 border rounded bg-primary-100 text-primary-600 mt-1 mr-1">
                                  {tag->str}
                                </span>
                              )
                              ->React.array
                            }}
                          </div>
                        </div>
                        {switch courseExport->CourseExport.file {
                        | None => ReasonReact.null
                        | Some(file) =>
                          <a
                            ariaLabel={"Download Course Export " ++
                            (courseExport->CourseExport.id)}
                            className="w-10 text-xs text-sm course-faculty__list-item-remove text-gray-700 hover:text-gray-900 cursor-pointer flex items-center justify-center hover:bg-gray-200"
                            href={file->CourseExport.filePath}>
                            <FaIcon classes="fas fa-file-download" />
                          </a>
                        }}
                      </div>
                    </div>
                  </div>
                )
                ->React.array}
              </div>
            </div>
          </div>}
    </div>
  </div>
}
