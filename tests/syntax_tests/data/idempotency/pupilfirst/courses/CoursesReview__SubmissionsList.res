open CoursesReview__Types
let str = React.string

let showSubmissionStatus = overlaySubmission => {
  let (text, classes) = switch (
    overlaySubmission->OverlaySubmission.passedAt,
    overlaySubmission->OverlaySubmission.evaluatorName,
  ) {
  | (None, None) => ("Pending", "bg-orange-100 border border-orange-500 text-orange-800 ")
  | (None, Some(_)) => ("Failed", "bg-red-100 border border-red-500 text-red-700")
  | (Some(_), None)
  | (Some(_), Some(_)) => ("Passed", "bg-green-100 border border-green-500 text-green-800")
  }
  <div className={"font-semibold px-3 py-px rounded " ++ classes}> {text->str} </div>
}

let showFeedbackSent = feedbackSent =>
  feedbackSent
    ? <div
        className="bg-primary-100 text-primary-600 border border-transparent font-semibold px-3 py-px rounded mr-3">
        {"Feedback Sent"->str}
      </div>
    : React.null

let cardClasses = overlaySubmission =>
  "mt-6 rounded-b-lg bg-white border-t-3 " ++
  switch (
    overlaySubmission->OverlaySubmission.passedAt,
    overlaySubmission->OverlaySubmission.evaluatorName,
  ) {
  | (None, None) => "border-orange-300"
  | (None, Some(_)) => "border-red-500"
  | (Some(_), None)
  | (Some(_), Some(_)) => "border-green-500"
  }

let updateSubmission = (
  ~feedbackUpdate,
  ~grades,
  ~passed,
  ~newFeedback,
  ~overlaySubmission,
  ~currentCoach,
  ~addGradingCB,
  ~checklist,
) => {
  let feedback = switch newFeedback {
  | Some(f) =>
    f->String.trim == ""
      ? overlaySubmission->OverlaySubmission.feedback
      : overlaySubmission
        ->OverlaySubmission.feedback
        ->Array.append([
          Feedback.make(
            ~coachName=currentCoach->Coach.name,
            ~coachAvatarUrl=currentCoach->Coach.avatarUrl,
            ~coachTitle=currentCoach->Coach.title,
            ~createdAt=Js.Date.make(),
            ~value=f,
          ),
        ])
  | None => overlaySubmission->OverlaySubmission.feedback
  }

  let (passedAt, evaluatedAt, newGrades) = switch passed {
  | Some(p) => (p ? Some(Js.Date.make()) : None, Some(Js.Date.make()), grades)
  | None => (
      overlaySubmission->OverlaySubmission.passedAt,
      overlaySubmission->OverlaySubmission.evaluatedAt,
      overlaySubmission->OverlaySubmission.grades,
    )
  }

  let newSubmission = OverlaySubmission.make(
    ~id=overlaySubmission->OverlaySubmission.id,
    ~createdAt=overlaySubmission->OverlaySubmission.createdAt,
    ~passedAt,
    ~evaluatorName=if feedbackUpdate {
      overlaySubmission->OverlaySubmission.evaluatorName
    } else {
      Some(currentCoach->Coach.name)
    },
    ~feedback,
    ~grades=newGrades,
    ~evaluatedAt,
    ~checklist,
  )

  addGradingCB(newSubmission)
}

let updateFeedbackArray = (currentCoach, overlaySubmission, newFeedback) =>
  newFeedback->String.trim == ""
    ? overlaySubmission->OverlaySubmission.feedback
    : overlaySubmission
      ->OverlaySubmission.feedback
      ->Array.append([
        Feedback.make(
          ~coachName=currentCoach->Coach.name,
          ~coachAvatarUrl=currentCoach->Coach.avatarUrl,
          ~coachTitle=currentCoach->Coach.title,
          ~createdAt=Js.Date.make(),
          ~value=newFeedback,
        ),
      ])

let addGrading = (
  ~addGradingCB,
  ~currentCoach,
  ~overlaySubmission,
  ~newFeedback,
  ~passed,
  ~grades,
  ~checklist,
) => {
  let feedback = updateFeedbackArray(currentCoach, overlaySubmission, newFeedback)

  let passedAt = passed ? Some(Js.Date.make()) : None
  let evaluatedAt = Some(Js.Date.make())

  OverlaySubmission.make(
    ~id=overlaySubmission->OverlaySubmission.id,
    ~createdAt=overlaySubmission->OverlaySubmission.createdAt,
    ~passedAt,
    ~evaluatorName=Some(currentCoach->Coach.name),
    ~feedback,
    ~grades,
    ~evaluatedAt,
    ~checklist,
  )->addGradingCB
}

let addFeedback = (addFeedbackCB, currentCoach, overlaySubmission, newFeedback) => {
  let feedback = updateFeedbackArray(currentCoach, overlaySubmission, newFeedback)

  overlaySubmission->OverlaySubmission.updateFeedback(feedback)->addFeedbackCB
}

@react.component
let make = (
  ~overlaySubmission,
  ~teamSubmission,
  ~addGradingCB,
  ~addFeedbackCB,
  ~submissionNumber,
  ~currentCoach,
  ~evaluationCriteria,
  ~reviewChecklist,
  ~updateReviewChecklistCB,
  ~targetId,
  ~targetEvaluationCriteriaIds,
) =>
  <div
    ariaLabel={"submissions-overlay-card-" ++ (overlaySubmission->OverlaySubmission.id)}
    className={cardClasses(overlaySubmission)}>
    <div className="rounded-b-lg shadow">
      <div
        className="p-4 md:px-6 md:py-5 border-b bg-white flex flex-col sm:flex-row items-center justify-between">
        <div className="flex flex-col w-full sm:w-auto">
          <h2 className="font-semibold text-sm lg:text-base leading-tight">
            {"Submission #" ++ (submissionNumber->string_of_int)->str}
          </h2>
          <span className="text-xs text-gray-800 pt-px">
            {overlaySubmission
            ->OverlaySubmission.createdAt
            ->DateFns.format("MMMM D, YYYY")
            ->str}
          </span>
        </div>
        <div className="text-xs flex w-full sm:w-auto mt-2 sm:mt-0">
          {showFeedbackSent(
            overlaySubmission->OverlaySubmission.feedback->ArrayUtils.isNotEmpty,
          )}
          {showSubmissionStatus(overlaySubmission)}
        </div>
      </div>
      <CoursesReview__GradeCard
        overlaySubmission
        teamSubmission
        evaluationCriteria
        targetEvaluationCriteriaIds
        reviewChecklist
        addGradingCB={addGrading(~addGradingCB, ~currentCoach, ~overlaySubmission)}
        updateReviewChecklistCB
        targetId
      />
      <CoursesReview__ShowFeedback
        feedback={overlaySubmission->OverlaySubmission.feedback}
        reviewed={overlaySubmission->OverlaySubmission.grades->ArrayUtils.isNotEmpty}
        submissionId={overlaySubmission->OverlaySubmission.id}
        reviewChecklist
        addFeedbackCB={addFeedback(addFeedbackCB, currentCoach, overlaySubmission)}
        updateReviewChecklistCB
        targetId
      />
    </div>
  </div>
