%raw(`require("./CoursesCurriculum__SubmissionsAndFeedback.css")`)

let str = React.string

open CoursesCurriculum__Types

let gradeBar = (evaluationCriteria, grade) => {
  let criterion =
    evaluationCriteria->ListUtils.findOpt(c =>
      c->EvaluationCriterion.id == (grade->Grade.evaluationCriterionId)
    )

  switch criterion {
  | Some(criterion) =>
    let criterionId = criterion->EvaluationCriterion.id
    let criterionName = criterion->EvaluationCriterion.name
    let gradeNumber = grade->Grade.grade
    let grading = Grading.make(~criterionId, ~criterionName, ~grade=gradeNumber)

    <div key={gradeNumber->string_of_int} className="mb-4">
      <CoursesCurriculum__GradeBar grading criterion />
    </div>
  | None => React.null
  }
}

let statusBar = (~color, ~text) => {
  let textColor = "text-" ++ (color ++ "-500 ")
  let bgColor = "bg-" ++ (color ++ "-100 ")

  let icon = switch color {
  | "green" =>
    <span>
      <i className="fas fa-certificate fa-stack-2x" />
      <i className="fas fa-check fa-stack-1x fa-inverse" />
    </span>
  | _anyOtherColor => <i className="fas fa-exclamation-triangle text-3xl text-red-500 mx-1" />
  }

  <div
    className={"font-semibold p-2 py-4 flex border-t w-full items-center justify-center " ++
    (textColor ++
    bgColor)}>
    <span className={"fa-stack text-lg mr-1 " ++ textColor}> icon </span> {text->str}
  </div>
}

let submissionStatusIcon = (~passed) => {
  let text = passed ? "Passed" : "Failed"
  let color = passed ? "green" : "red"

  <div className="max-w-fc">
    <div className={"flex border-2 rounded-lg border-" ++ (color ++ "-500 px-4 py-6")}>
      {passed
        ? <span className="fa-stack text-green-500 text-lg">
            <i className="fas fa-certificate fa-stack-2x" />
            <i className="fas fa-check fa-stack-1x fa-inverse" />
          </span>
        : <i className="fas fa-exclamation-triangle text-3xl text-red-500 mx-1" />}
    </div>
    <div className={"text-center text-" ++ (color ++ "-500 font-bold mt-2")}> {text->str} </div>
  </div>
}

let undoSubmissionCB = () => {
  open Webapi.Dom
  location->Location.reload
}

let gradingSection = (~grades, ~evaluationCriteria, ~gradeBar, ~passed) =>
  <div>
    <div className="w-full md:hidden">
      {statusBar(~color=passed ? "green" : "red", ~text=passed ? "Passed" : "Failed")}
    </div>
    <div className="bg-white flex border-t flex-wrap items-center py-4">
      <div className="w-full md:w-1/2 flex-shrink-0 justify-center hidden md:flex border-l px-6">
        {submissionStatusIcon(~passed)}
      </div>
      <div className="w-full md:w-1/2 flex-shrink-0 md:order-first px-4 md:px-6">
        <h5 className="pb-1 border-b"> {"Grading"->str} </h5>
        <div className="mt-3">
          {grades
          ->Grade.sort(evaluationCriteria)
          ->List.map(grade => gradeBar(grade))
          ->Array.of_list
          ->React.array}
        </div>
      </div>
    </div>
  </div>

let handleAddAnotherSubmission = (setShowSubmissionForm, event) => {
  event->ReactEvent.Mouse.preventDefault
  setShowSubmissionForm(showSubmissionForm => !showSubmissionForm)
}

let submissions = (target, targetStatus, targetDetails, evaluationCriteria, coaches, users) => {
  let curriedGradeBar = gradeBar(evaluationCriteria)

  targetDetails
  ->TargetDetails.submissions
  ->List.sort((s1, s2) => {
    let s1CreatedAt = s1->Submission.createdAtDate
    let s2CreatedAt = s2->Submission.createdAtDate

    s1CreatedAt->DateFns.differenceInSeconds(s2CreatedAt)->int_of_float
  })
  ->List.map(submission => {
    let grades = targetDetails->TargetDetails.grades(submission->Submission.id)

    <div
      key={submission->Submission.id}
      className="mt-4 pb-4 relative curriculum__submission-feedback-container"
      ariaLabel={"Details about your submission on " ++ (submission->Submission.createdAtPretty)}>
      <div
        className="text-xs font-semibold bg-gray-100 inline-block px-3 py-1 ml-2 rounded-t-lg border-t border-r border-l text-gray-800 leading-tight">
        {"Submitted on " ++ (submission->Submission.createdAtPretty)->str}
      </div>
      <div className="rounded-lg bg-gray-100 border shadow-md overflow-hidden">
        <div className="px-4 py-4 md:px-6 md:pt-6 md:pb-5">
          <SubmissionChecklistShow
            checklist={submission->Submission.checklist}
            updateChecklistCB=None
            pending={submission->Submission.pending}
          />
        </div>
        {switch submission->Submission.status {
        | MarkedAsComplete => statusBar(~color="green", ~text="Marked as complete")
        | Pending =>
          <div
            className="bg-white p-3 md:px-6 md:py-4 flex border-t justify-between items-center w-full">
            <div
              className="flex items-center justify-center font-semibold text-sm pl-2 pr-3 py-1 bg-orange-100 text-orange-600 rounded">
              <span className="fa-stack text-orange-400 mr-2 flex-shrink-0">
                <i className="fas fa-circle fa-stack-2x" />
                <i className="fas fa-hourglass-half fa-stack-1x fa-inverse" />
              </span>
              {"Review pending"->str}
            </div>
            {switch targetStatus->TargetStatus.status {
            | Submitted =>
              <CoursesCurriculum__UndoButton undoSubmissionCB targetId={target->Target.id} />

            | Pending
            | Passed
            | Failed
            | Locked(_) => React.null
            }}
          </div>
        | Passed =>
          gradingSection(~grades, ~evaluationCriteria, ~passed=true, ~gradeBar=curriedGradeBar)
        | Failed =>
          gradingSection(~grades, ~evaluationCriteria, ~passed=false, ~gradeBar=curriedGradeBar)
        }}
        {targetDetails
        ->TargetDetails.feedback
        ->List.filter(feedback =>
          feedback->Feedback.submissionId == (submission->Submission.id)
        )
        ->List.map(feedback => {
          let coach =
            coaches->ListUtils.findOpt(c => c->Coach.id == (feedback->Feedback.coachId))

          let user = switch coach {
          | Some(coach) =>
            users->ListUtils.findOpt(up => up->User.id == (coach->Coach.userId))
          | None => None
          }

          let (coachName, coachTitle, coachAvatar) = switch user {
          | Some(user) =>
            let name = user->User.name
            let avatar = user->User.avatarUrl
            let title = user->User.title
            (name, title, <img src=avatar />)
          | None => (
              "Unknown Coach",
              None,
              <div
                className="w-10 h-10 rounded-full bg-gray-400 inline-block flex items-center justify-center">
                <i className="fas fa-user-times" />
              </div>,
            )
          }

          <div className="bg-white border-t p-4 md:p-6" key={feedback->Feedback.id}>
            <div className="flex items-center">
              <div
                className="flex-shrink-0 w-12 h-12 bg-gray-300 rounded-full overflow-hidden mr-3 object-cover">
                coachAvatar
              </div>
              <div>
                <p className="text-xs leading-tight"> {"Feedback from:"->str} </p>
                <div>
                  <h4
                    className="font-semibold text-base leading-tight block md:inline-flex self-end">
                    {coachName->str}
                  </h4>
                  {switch coachTitle {
                  | Some(title) =>
                    <span
                      className="block md:inline-flex text-xs text-gray-800 md:ml-2 leading-tight self-end">
                      {"(" ++ (title ++ ")")->str}
                    </span>
                  | None => React.null
                  }}
                </div>
              </div>
            </div>
            <MarkdownBlock
              profile=Markdown.Permissive
              className="md:ml-15"
              markdown={feedback->Feedback.feedback}
            />
          </div>
        })
        ->Array.of_list
        ->React.array}
      </div>
    </div>
  })
  ->Array.of_list
  ->React.array
}

let addSubmission = (setShowSubmissionForm, addSubmissionCB, submission) => {
  setShowSubmissionForm(_ => false)
  addSubmissionCB(submission)
}

@react.component
let make = (
  ~targetDetails,
  ~target,
  ~evaluationCriteria,
  ~addSubmissionCB,
  ~targetStatus,
  ~coaches,
  ~users,
  ~preview,
  ~checklist,
) => {
  let (showSubmissionForm, setShowSubmissionForm) = React.useState(() => false)

  <div>
    <div className="flex justify-between items-end border-b pb-2">
      <h4 className="text-base md:text-xl"> {"Your Submissions"->str} </h4>
      {targetStatus->TargetStatus.canSubmit(~resubmittable=target->Target.resubmittable)
        ? showSubmissionForm
            ? <button
                className="btn btn-subtle"
                onClick={handleAddAnotherSubmission(setShowSubmissionForm)}>
                <PfIcon className="if i-times-regular text-lg mr-2" />
                <span className="hidden md:inline"> {"Cancel"->str} </span>
                <span className="md:hidden"> {"Cancel"->str} </span>
              </button>
            : <button
                className="btn btn-primary"
                onClick={handleAddAnotherSubmission(setShowSubmissionForm)}>
                <PfIcon className="if i-plus-regular text-lg mr-2" />
                <span className="hidden md:inline"> {"Add another submission"->str} </span>
                <span className="md:hidden"> {"Add another"->str} </span>
              </button>
        : React.null}
    </div>
    {showSubmissionForm
      ? <CoursesCurriculum__SubmissionBuilder
          target
          addSubmissionCB={addSubmission(setShowSubmissionForm, addSubmissionCB)}
          checklist
          preview
        />
      : submissions(target, targetStatus, targetDetails, evaluationCriteria, coaches, users)}
  </div>
}
