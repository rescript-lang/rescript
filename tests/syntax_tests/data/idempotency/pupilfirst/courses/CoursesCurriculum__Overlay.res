exception UnexpectedSubmissionStatus(string)

%raw(`require("./CoursesCurriculum__Overlay.css")`)

open CoursesCurriculum__Types
module TargetStatus = CoursesCurriculum__TargetStatus

let str = React.string

type tab =
  | Learn
  | Discuss
  | Complete(TargetDetails.completionType)

type state = {
  targetDetails: option<TargetDetails.t>,
  tab: tab,
}

type action =
  | Select(tab)
  | SetTargetDetails(TargetDetails.t)
  | AddSubmission(Target.role)
  | ClearTargetDetails

let initialState = {targetDetails: None, tab: Learn}

let reducer = (state, action) =>
  switch action {
  | Select(tab) => {...state, tab: tab}
  | SetTargetDetails(targetDetails) => {
      ...state,
      targetDetails: Some(targetDetails),
    }
  | ClearTargetDetails => {...state, targetDetails: None}
  | AddSubmission(role) =>
    switch role {
    | Target.Student => state
    | Team => {
        ...state,
        targetDetails: state.targetDetails->OptionUtils.map(TargetDetails.clearPendingUserIds),
      }
    }
  }

let closeOverlay = course =>
  ReasonReactRouter.push("/courses/" ++ ((course->Course.id) ++ "/curriculum"))

let loadTargetDetails = (target, send, ()) => {
  {
    open Js.Promise
    Fetch.fetch("/targets/" ++ ((target->Target.id) ++ "/details_v2"))
    ->then_(Fetch.Response.json)
    ->then_(json => send(SetTargetDetails(json->TargetDetails.decode))->resolve)
  }->ignore

  None
}

let completionTypeToString = (completionType, targetStatus) =>
  switch (targetStatus->TargetStatus.status, (completionType: TargetDetails.completionType)) {
  | (Pending, Evaluated) => "Complete"
  | (Pending, TakeQuiz) => "Take Quiz"
  | (Pending, LinkToComplete) => "Visit Link to Complete"
  | (Pending, MarkAsComplete) => "Mark as Complete"
  | (
      Submitted | Passed | Failed | Locked(CourseLocked | AccessLocked),
      Evaluated | TakeQuiz,
    ) => "Submissions & Feedback"
  | (Submitted | Passed | Failed, LinkToComplete | MarkAsComplete) => "Completed"
  | (Locked(_), Evaluated | TakeQuiz | LinkToComplete | MarkAsComplete) => "Locked"
  }

let tabToString = (targetStatus, tab) =>
  switch tab {
  | Learn => "Learn"
  | Discuss => "Discuss"
  | Complete(completionType) => completionTypeToString(completionType, targetStatus)
  }

let selectableTabs = targetDetails =>
  targetDetails->TargetDetails.communities->ListUtils.isNotEmpty
    ? list{Learn, Discuss}
    : list{Learn}

let tabClasses = (selection, tab) =>
  "course-overlay__body-tab-item p-2 md:px-3 md:py-4 flex w-full items-center justify-center text-sm -mx-px font-semibold" ++ (
    tab == selection
      ? " course-overlay__body-tab-item--selected"
      : " bg-gray-100 hover:text-primary-400 hover:bg-gray-200 cursor-pointer"
  )

let scrollCompleteButtonIntoViewEventually = () => Js.Global.setTimeout(() => {
    let element = Webapi.Dom.document->Webapi.Dom.Document.getElementById("auto-verify-target")
    switch element {
    | Some(e) =>
      Webapi.Dom.Element.scrollIntoView(e)
      e->Webapi.Dom.Element.setClassName("mt-4 complete-button-selected")
    | None => Rollbar.error("Could not find the 'Complete' button to scroll to.")
    }
  }, 50)->ignore

let handleTablink = (send, _event) => {
  send(Select(Learn))
  scrollCompleteButtonIntoViewEventually()
}

let tabButton = (tab, state, send, targetStatus) =>
  <span
    key={"select-" ++ (tab->tabToString(targetStatus))}
    className={tabClasses(tab, state.tab)}
    onClick={_e => send(Select(tab))}>
    {tab->tabToString(targetStatus)->str}
  </span>

let tabLink = (tab, state, send, targetStatus) =>
  <span onClick={handleTablink(send)} className={tabClasses(tab, state.tab)}>
    {tab->tabToString(targetStatus)->str}
  </span>

let tabOptions = (state, send, targetDetails, targetStatus) => {
  let completionType = targetDetails->TargetDetails.computeCompletionType

  <div className="flex justify-between max-w-3xl mx-auto -mb-px mt-5 md:mt-7">
    {selectableTabs(targetDetails)
    ->List.map(selection => tabButton(selection, state, send, targetStatus))
    ->Array.of_list
    ->React.array}
    {switch (targetStatus->TargetStatus.status, completionType) {
    | (Pending | Submitted | Passed | Failed, Evaluated | TakeQuiz) =>
      tabButton(Complete(completionType), state, send, targetStatus)
    | (Locked(CourseLocked | AccessLocked), Evaluated | TakeQuiz) =>
      targetDetails->TargetDetails.submissions->ListUtils.isNotEmpty
        ? tabButton(Complete(completionType), state, send, targetStatus)
        : React.null
    | (Pending | Submitted | Passed | Failed, LinkToComplete | MarkAsComplete) =>
      tabLink(Complete(completionType), state, send, targetStatus)
    | (Locked(_), _) => React.null
    }}
  </div>
}

let addSubmission = (target, state, send, addSubmissionCB, submission) => {
  switch state.targetDetails {
  | Some(targetDetails) =>
    let newTargetDetails = targetDetails->TargetDetails.addSubmission(submission)

    send(SetTargetDetails(newTargetDetails))
  | None => ()
  }

  switch submission->Submission.status {
  | MarkedAsComplete =>
    addSubmissionCB(LatestSubmission.make(~pending=false, ~targetId=target->Target.id))
  | Pending => addSubmissionCB(LatestSubmission.make(~pending=true, ~targetId=target->Target.id))
  | Passed =>
    raise(
      UnexpectedSubmissionStatus(
        "CoursesCurriculum__Overlay.addSubmission cannot handle a submsision with status Passed",
      ),
    )
  | Failed =>
    raise(
      UnexpectedSubmissionStatus(
        "CoursesCurriculum__Overlay.addSubmission cannot handle a submsision with status Failed",
      ),
    )
  }
}

let addVerifiedSubmission = (target, state, send, addSubmissionCB, submission) => {
  switch state.targetDetails {
  | Some(targetDetails) =>
    let newTargetDetails = targetDetails->TargetDetails.addSubmission(submission)
    send(SetTargetDetails(newTargetDetails))
  | None => ()
  }

  addSubmissionCB(LatestSubmission.make(~pending=false, ~targetId=target->Target.id))
}

let targetStatusClass = (prefix, targetStatus) =>
  prefix ++ (targetStatus->TargetStatus.statusToString->Js.String.toLowerCase)

let targetStatusClasses = targetStatus =>
  "curriculum__target-status bg-white text-xs mt-2 md:mt-0 py-1 px-2 md:px-4 " ++
  targetStatusClass("curriculum__target-status--", targetStatus)

let overlayHeaderTitleCardClasses = targetStatus =>
  "course-overlay__header-title-card relative flex justify-between items-center px-3 py-5 md:p-6 " ++
  targetStatusClass("course-overlay__header-title-card--", targetStatus)

let renderLocked = text =>
  <div
    className="mx-auto text-center bg-gray-900 text-white max-w-fc px-4 py-2 text-sm font-semibold relative z-10 rounded-b-lg">
    <i className="fas fa-lock text-lg" /> <span className="ml-2"> {text->str} </span>
  </div>
let overlayStatus = (course, target, targetStatus, preview) =>
  <div>
    <div className={overlayHeaderTitleCardClasses(targetStatus)}>
      <button
        className={"course-overlay__close xl:absolute flex flex-col items-center justify-center absolute rounded-t-lg lg:rounded-t-none lg:rounded-b-lg leading-tight px-4 py-1 h-8 lg:h-full cursor-pointer border border-b-0 lg:border-transparent lg:border-t-0 lg:shadow hover:text-gray-900 hover:shadow-md focus:border-gray-300 focus:outline-none focus:shadow-inner " ++
        targetStatusClass("course-overlay__close--", targetStatus)}
        onClick={_e => closeOverlay(course)}>
        <Icon className="if i-times-regular text-xl lg:text-2xl mt-1 lg:mt-0" />
        <span className="text-xs hidden lg:inline-block mt-px"> {"Close"->str} </span>
      </button>
      <div className="w-full flex flex-wrap md:flex-no-wrap items-center justify-between relative">
        <h1 className="text-base leading-snug md:mr-6 md:text-xl">
          {target->Target.title->str}
        </h1>
        <div className={targetStatusClasses(targetStatus)}>
          {targetStatus->TargetStatus.statusToString->str}
        </div>
      </div>
    </div>
    {preview
      ? <div> {renderLocked("You are currently looking at a preview of this course.")} </div>
      : React.null}
  </div>

let renderLockReason = reason => renderLocked(reason->TargetStatus.lockReasonToString)

let prerequisitesIncomplete = (reason, target, targets, statusOfTargets) => {
  let prerequisiteTargetIds = target->Target.prerequisiteTargetIds
  let prerequisiteTargets =
    targets->List.filter(target => (target->Target.id)->List.mem(prerequisiteTargetIds))
  <div className="relative px-3 md:px-0">
    {renderLockReason(reason)}
    <div
      className="course-overlay__prerequisite-targets z-10 max-w-3xl mx-auto bg-white text-center rounded-lg overflow-hidden shadow mt-6">
      {prerequisiteTargets
      ->List.map(target => {
        let targetStatus =
          statusOfTargets->List.find(ts => ts->TargetStatus.targetId == (target->Target.id))

        <Link
          href={"/targets/" ++ (target->Target.id)}
          ariaLabel={"Select Target " ++ (target->Target.id)}
          key={target->Target.id}
          className="bg-white border-t px-6 py-4 relative z-10 flex items-center justify-between hover:bg-gray-200 hover:text-primary-500 cursor-pointer">
          <span className="font-semibold text-left leading-snug">
            {target->Target.title->str}
          </span>
          <span className={targetStatusClasses(targetStatus)}>
            {targetStatus->TargetStatus.statusToString->str}
          </span>
        </Link>
      })
      ->Array.of_list
      ->React.array}
    </div>
  </div>
}

let handleLocked = (target, targets, targetStatus, statusOfTargets) =>
  switch targetStatus->TargetStatus.status {
  | Locked(reason) =>
    switch reason {
    | PrerequisitesIncomplete => prerequisitesIncomplete(reason, target, targets, statusOfTargets)
    | CourseLocked
    | AccessLocked
    | LevelLocked =>
      renderLockReason(reason)
    }
  | Pending
  | Submitted
  | Passed
  | Failed => React.null
  }

let overlayContentClasses = bool => bool ? "" : "hidden"

let learnSection = (targetDetails, tab) =>
  <div className={overlayContentClasses(tab == Learn)}>
    <CoursesCurriculum__Learn targetDetails />
  </div>

let discussSection = (target, targetDetails, tab) =>
  <div className={overlayContentClasses(tab == Discuss)}>
    <CoursesCurriculum__Discuss
      targetId={target->Target.id} communities={targetDetails->TargetDetails.communities}
    />
  </div>

let completeSectionClasses = (tab, completionType: TargetDetails.completionType) =>
  switch (tab, completionType) {
  | (Learn, Evaluated | TakeQuiz)
  | (Discuss, Evaluated | TakeQuiz | MarkAsComplete | LinkToComplete) => "hidden"
  | (Learn, MarkAsComplete | LinkToComplete)
  | (Complete(_), Evaluated | TakeQuiz | MarkAsComplete | LinkToComplete) => ""
  }

let completeSection = (
  state,
  send,
  target,
  targetDetails,
  targetStatus,
  addSubmissionCB,
  evaluationCriteria,
  coaches,
  users,
  preview,
) => {
  let completionType = targetDetails->TargetDetails.computeCompletionType

  let addVerifiedSubmissionCB = addVerifiedSubmission(target, state, send, addSubmissionCB)

  <div className={completeSectionClasses(state.tab, completionType)}>
    {switch (targetStatus->TargetStatus.status, completionType) {
    | (Pending, Evaluated) =>
      [
        <CoursesCurriculum__CompletionInstructions
          key="completion-instructions" targetDetails title="Instructions"
        />,
        <CoursesCurriculum__SubmissionBuilder
          key="courses-curriculum-submission-form"
          target
          checklist={targetDetails->TargetDetails.checklist}
          addSubmissionCB={addSubmission(target, state, send, addSubmissionCB)}
          preview
        />,
      ]->React.array
    | (Pending, TakeQuiz) =>
      [
        <CoursesCurriculum__CompletionInstructions
          key="completion-instructions" targetDetails title="Instructions"
        />,
        <CoursesCurriculum__Quiz
          key="courses-curriculum-quiz"
          target
          targetDetails
          addSubmissionCB=addVerifiedSubmissionCB
          preview
        />,
      ]->React.array

    | (Submitted | Passed | Failed | Locked(CourseLocked | AccessLocked), Evaluated | TakeQuiz) =>
      <CoursesCurriculum__SubmissionsAndFeedback
        targetDetails
        target
        evaluationCriteria
        addSubmissionCB={addSubmission(target, state, send, addSubmissionCB)}
        targetStatus
        coaches
        users
        preview
        checklist={targetDetails->TargetDetails.checklist}
      />
    | (Pending | Submitted | Passed | Failed, LinkToComplete | MarkAsComplete) =>
      <CoursesCurriculum__AutoVerify
        target targetDetails targetStatus addSubmissionCB=addVerifiedSubmissionCB preview
      />
    | (Locked(_), Evaluated | TakeQuiz | MarkAsComplete | LinkToComplete) => React.null
    }}
  </div>
}

let renderPendingStudents = (pendingUserIds, users) =>
  <div className="max-w-3xl mx-auto text-center mt-4">
    <div className="font-semibold text-md">
      {"You have team members who are yet to complete this target:"->str}
    </div>
    <div className="flex justify-center flex-wrap">
      {pendingUserIds
      ->List.map(studentId => {
        let user =
          users->ListUtils.unsafeFind(
            u => u->User.id == studentId,
            "Unable to find user with id " ++ (studentId ++ "in CoursesCurriculum__Overlay"),
          )

        <div
          title={(user->User.name) ++ " has not completed this target."}
          className="w-10 h-10 rounded-full border border-yellow-400 flex items-center justify-center overflow-hidden mx-1 shadow-md flex-shrink-0 mt-2">
          <img src={user->User.avatarUrl} />
        </div>
      })
      ->Array.of_list
      ->React.array}
    </div>
  </div>

let handlePendingStudents = (targetStatus, targetDetails, users) =>
  switch (targetDetails, targetStatus->TargetStatus.status) {
  | (Some(targetDetails), Submitted | Passed) =>
    let pendingUserIds = targetDetails->TargetDetails.pendingUserIds
    pendingUserIds->ListUtils.isNotEmpty
      ? renderPendingStudents(pendingUserIds, users)
      : React.null
  | (Some(_) | None, Locked(_) | Pending | Submitted | Passed | Failed) => React.null
  }

let performQuickNavigation = (send, _event) => {
  {
    open // Scroll to the top of the overlay before pushing the new URL.
    Webapi.Dom
    switch document->Document.getElementById("target-overlay") {
    | Some(element) => Webapi.Dom.Element.setScrollTop(element, 0.0)
    | None => ()
    }
  }

  // Clear loaded target details.
  send(ClearTargetDetails)
}

let navigationLink = (direction, url, send) => {
  let (leftIcon, text, rightIcon) = switch direction {
  | #Previous => (Some("fa-arrow-left"), "Previous Target", None)
  | #Next => (None, "Next Target", Some("fa-arrow-right"))
  }

  let arrow = icon =>
    icon->Belt.Option.mapWithDefault(React.null, icon => <FaIcon classes={"fas " ++ icon} />)

  <Link
    href=url
    onClick={performQuickNavigation(send)}
    className="block p-2 md:p-4 text-center border rounded-lg bg-gray-100 hover:bg-gray-200">
    {arrow(leftIcon)}
    <span className="mx-2 hidden md:inline"> {text->str} </span>
    {arrow(rightIcon)}
  </Link>
}

let scrollOverlayToTop = _event => {
  let element = {
    open Webapi.Dom
    document->Document.getElementById("target-overlay")
  }
  element->Belt.Option.mapWithDefault((), element => element->Webapi.Dom.Element.setScrollTop(0.0))
}

let quickNavigationLinks = (targetDetails, send) => {
  let (previous, next) = targetDetails->TargetDetails.navigation

  <div className="pb-6">
    <hr className="my-6" />
    <div className="container mx-auto max-w-3xl flex px-3 lg:px-0">
      <div className="w-1/3 mr-2">
        {previous->Belt.Option.mapWithDefault(React.null, previousUrl =>
          navigationLink(#Previous, previousUrl, send)
        )}
      </div>
      <div className="w-1/3 mx-2">
        <button
          onClick=scrollOverlayToTop
          className="block w-full focus:outline-none p-2 md:p-4 text-center border rounded-lg bg-gray-100 hover:bg-gray-200">
          <span className="mx-2 hidden md:inline"> {"Scroll to Top"->str} </span>
          <span className="mx-2 md:hidden"> <i className="fas fa-arrow-up" /> </span>
        </button>
      </div>
      <div className="w-1/3 ml-2">
        {next->Belt.Option.mapWithDefault(React.null, nextUrl =>
          navigationLink(#Next, nextUrl, send)
        )}
      </div>
    </div>
  </div>
}

let updatePendingUserIdsWhenAddingSubmission = (send, target, addSubmissionCB, submission) => {
  send(AddSubmission(target->Target.role))
  addSubmissionCB(submission)
}

@react.component
let make = (
  ~target,
  ~course,
  ~targetStatus,
  ~addSubmissionCB,
  ~targets,
  ~statusOfTargets,
  ~users,
  ~evaluationCriteria,
  ~coaches,
  ~preview,
) => {
  let (state, send) = React.useReducer(reducer, initialState)

  React.useEffect1(loadTargetDetails(target, send), [target->Target.id])

  React.useEffect(() => {
    ScrollLock.activate()
    Some(() => ScrollLock.deactivate())
  })

  <div
    id="target-overlay"
    className="fixed z-30 top-0 left-0 w-full h-full overflow-y-scroll bg-white">
    <div className="bg-gray-100 border-b border-gray-400 px-3">
      <div className="course-overlay__header-container pt-12 lg:pt-0 mx-auto">
        {overlayStatus(course, target, targetStatus, preview)}
        {handleLocked(target, targets, targetStatus, statusOfTargets)}
        {handlePendingStudents(targetStatus, state.targetDetails, users)}
        {switch state.targetDetails {
        | Some(targetDetails) => tabOptions(state, send, targetDetails, targetStatus)
        | None =>
          <div className="course-overlay__skeleton-head-container max-w-3xl w-full mx-auto">
            <div
              className="course-overlay__skeleton-head-wrapper bg-white h-13 flex items-center justify-between border border-b-0 rounded-t-lg mt-5 md:mt-7">
              <div
                className="course-overlay__skeleton-line-placeholder-sm w-1/3 mx-8 skeleton-animate"
              />
              <div
                className="course-overlay__skeleton-line-placeholder-sm w-1/3 mx-8 skeleton-animate"
              />
              <div
                className="course-overlay__skeleton-line-placeholder-sm w-1/3 mx-8 skeleton-animate"
              />
            </div>
          </div>
        }}
      </div>
    </div>
    {switch state.targetDetails {
    | Some(targetDetails) =>
      <div>
        <div className="container mx-auto mt-6 md:mt-8 max-w-3xl px-3 lg:px-0">
          {learnSection(targetDetails, state.tab)}
          {discussSection(target, targetDetails, state.tab)}
          {completeSection(
            state,
            send,
            target,
            targetDetails,
            targetStatus,
            updatePendingUserIdsWhenAddingSubmission(send, target, addSubmissionCB),
            evaluationCriteria,
            coaches,
            users,
            preview,
          )}
        </div>
        {switch state.tab {
        | Learn => quickNavigationLinks(targetDetails, send)
        | Discuss
        | Complete(_) => React.null
        }}
      </div>

    | None =>
      <div className="course-overlay__skeleton-body-container max-w-3xl w-full pb-4 mx-auto">
        <div className="course-overlay__skeleton-body-wrapper mt-8 px-3 lg:px-0">
          <div
            className="course-overlay__skeleton-line-placeholder-md mt-4 w-2/4 skeleton-animate"
          />
          <div className="course-overlay__skeleton-line-placeholder-sm mt-4 skeleton-animate" />
          <div className="course-overlay__skeleton-line-placeholder-sm mt-4 skeleton-animate" />
          <div
            className="course-overlay__skeleton-line-placeholder-sm mt-4 w-3/4 skeleton-animate"
          />
          <div className="course-overlay__skeleton-image-placeholder mt-5 skeleton-animate" />
          <div
            className="course-overlay__skeleton-line-placeholder-sm mt-4 w-2/5 skeleton-animate"
          />
        </div>
        <div className="course-overlay__skeleton-body-wrapper mt-8 px-3 lg:px-0">
          <div
            className="course-overlay__skeleton-line-placeholder-sm mt-4 w-3/4 skeleton-animate"
          />
          <div className="course-overlay__skeleton-line-placeholder-sm mt-4 skeleton-animate" />
          <div
            className="course-overlay__skeleton-line-placeholder-sm mt-4 w-3/4 skeleton-animate"
          />
        </div>
      </div>
    }}
  </div>
}
