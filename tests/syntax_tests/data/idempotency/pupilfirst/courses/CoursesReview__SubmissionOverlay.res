%raw(`require("./CoursesReview__SubmissionOverlay.css")`)

open CoursesReview__Types
let str = React.string

type state =
  | Loading
  | Loaded(SubmissionDetails.t)

module SubmissionDetailsQuery = %graphql(`
    query SubmissionDetailsQuery($submissionId: ID!) {
      submissionDetails(submissionId: $submissionId) {
        targetId, targetTitle, levelNumber, levelId, inactiveStudents
        students {
          id
          name
        },
        evaluationCriteria{
          id, name, maxGrade, passGrade, gradeLabels { grade label}
        },
        reviewChecklist{
          title
          result{
            title
            feedback
          }
        },
        targetEvaluationCriteriaIds,
        submissions{
          id, evaluatorName, passedAt, createdAt, evaluatedAt
          files{
            url, title, id
          },
          grades {
            evaluationCriterionId, grade
          },
          feedback{
            id, coachName, coachAvatarUrl, coachTitle, createdAt,value
          },
          checklist
        }
        coachIds
      }
    }
  `)

/*
 * Use the sync submission callback to allow the index to reload its data if
 * state of this submission is different from the one on the index.
 *
 * Then, set the state of this component to Loaded.
 */
let updateSubmissionDetails = (setState, submissionId, syncSubmissionCB, details) => {
  let submissionDetails = details->SubmissionDetails.decodeJs

  submissionDetails
  ->SubmissionDetails.submissions
  ->ArrayUtils.unsafeFind(
    submission => submission->OverlaySubmission.id == submissionId,
    "Could not find overlaySubmission with ID " ++ (submissionId ++ " in loaded submissions"),
  )
  ->syncSubmissionCB

  setState(_ => Loaded(submissionDetails))
}

let getSubmissionDetails = (submissionId, setState, syncSubmissionCB, ()) => {
  setState(_ => Loading)
  SubmissionDetailsQuery.make(~submissionId, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    response["submissionDetails"]->updateSubmissionDetails(
      setState,
      submissionId,
      syncSubmissionCB,
    )
    Js.Promise.resolve()
  })
  ->ignore

  None
}

let closeOverlay = courseId => ReasonReactRouter.push("/courses/" ++ (courseId ++ "/review"))

let headerSection = (submissionDetails, courseId, assignedCoaches) =>
  <div
    ariaLabel="submissions-overlay-header"
    className="bg-gray-100 border-b border-gray-300 px-3 pt-12 xl:pt-10 flex justify-center">
    <div
      className="relative bg-white border lg:border-transparent p-4 lg:px-6 lg:py-5 flex flex-wrap items-center justify-between rounded-lg shadow container max-w-3xl -mb-12">
      <div
        ariaLabel="submissions-overlay-close"
        onClick={_ => closeOverlay(courseId)}
        className="review-submission-overlay__close flex flex-col items-center justify-center absolute rounded-t-lg lg:rounded-lg leading-tight px-4 py-1 h-8 lg:h-full cursor-pointer border border-b-0 border-gray-400 lg:border-0 lg:shadow lg:border-gray-300 bg-white text-gray-700 hover:text-gray-900 hover:bg-gray-100">
        <Icon className="if i-times-regular text-xl lg:text-2xl mt-1 lg:mt-0" />
        <span className="text-xs hidden lg:inline-block mt-px"> {"close"->str} </span>
      </div>
      <div>
        <div className="block text-sm md:pr-2">
          <span className="bg-gray-300 text-xs font-semibold px-2 py-px rounded">
            {"Level " ++ (submissionDetails->SubmissionDetails.levelNumber)->str}
          </span>
          <a
            href={"/targets/" ++ (submissionDetails->SubmissionDetails.targetId)}
            target="_blank"
            className="ml-2 font-semibold underline text-gray-900 hover:bg-primary-100 hover:text-primary-600 text-sm md:text-lg">
            {submissionDetails->SubmissionDetails.targetTitle->str}
          </a>
        </div>
        <div className="text-left mt-1 text-xs text-gray-800">
          <span> {"Submitted by "->str} </span>
          {
            let studentCount = submissionDetails->SubmissionDetails.students->Array.length

            submissionDetails
            ->SubmissionDetails.students
            ->Array.mapi((index, student) => {
              let commaRequired = index + 1 != studentCount
              <span key={student->Student.id}>
                <a
                  className="font-semibold underline"
                  href={"/students/" ++ ((student->Student.id) ++ "/report")}
                  target="_blank">
                  {student->Student.name->str}
                </a>
                {(commaRequired ? ", " : "")->str}
              </span>
            })
            ->React.array
          }
        </div>
      </div>
      <CoursesStudents__TeamCoaches
        tooltipPosition=#Bottom
        defaultAvatarSize="8"
        mdAvatarSize="8"
        title={<span className="mr-2"> {"Assigned Coaches"->str} </span>}
        className="mt-2 flex w-full md:w-auto items-center flex-shrink-0"
        coaches=assignedCoaches
      />
    </div>
  </div>

let updateSubmissionDetails = (setState, submissionDetails, overlaySubmission) => {
  // Create new details for overlay with updated overlaySubmission.
  let newSubmissionDetails =
    submissionDetails->SubmissionDetails.updateSubmission(overlaySubmission)

  // Re-render the overlay with the updated submission details.
  setState(_ => Loaded(newSubmissionDetails))

  newSubmissionDetails
}

let addGrading = (setState, removePendingSubmissionCB, submissionDetails, overlaySubmission) => {
  updateSubmissionDetails(setState, submissionDetails, overlaySubmission)->ignore

  removePendingSubmissionCB()
}

let addFeedbackToReviewedSubmission = (
  setState,
  updateReviewedSubmissionCB,
  submissionDetails,
  overlaySubmission,
) =>
  updateSubmissionDetails(setState, submissionDetails, overlaySubmission)
  ->SubmissionDetails.makeIndexSubmission(overlaySubmission)
  ->updateReviewedSubmissionCB

let updateReviewChecklist = (submissionDetails, setState, reviewChecklist) =>
  setState(_ => Loaded(
    submissionDetails->SubmissionDetails.updateReviewChecklist(reviewChecklist),
  ))

let inactiveWarning = submissionDetails =>
  if submissionDetails->SubmissionDetails.inactiveStudents {
    let warning = if submissionDetails->SubmissionDetails.students->Array.length > 1 {
      "This submission is linked to one or more students whose access to the course has ended, or have dropped out."
    } else {
      "This submission is from a student whose access to the course has ended, or has dropped out."
    }

    <div className="border border-yellow-400 rounded bg-yellow-400 py-2 px-3">
      <i className="fas fa-exclamation-triangle" /> <span className="ml-2"> {warning->str} </span>
    </div>
  } else {
    React.null
  }

@react.component
let make = (
  ~courseId,
  ~submissionId,
  ~teamCoaches,
  ~currentCoach,
  ~syncSubmissionCB,
  ~removePendingSubmissionCB,
  ~updateReviewedSubmissionCB,
) => {
  let (state, setState) = React.useState(() => Loading)

  React.useEffect(() => {
    ScrollLock.activate()
    Some(() => ScrollLock.deactivate())
  })

  React.useEffect1(getSubmissionDetails(submissionId, setState, syncSubmissionCB), [submissionId])
  <div className="fixed z-30 top-0 left-0 w-full h-full overflow-y-scroll bg-white">
    {switch state {
    | Loaded(submissionDetails) =>
      let assignedCoaches =
        teamCoaches->Js.Array.filter(coach =>
          submissionDetails->SubmissionDetails.coachIds->Array.mem(coach->Coach.id)
        )

      <div>
        {headerSection(submissionDetails, courseId, assignedCoaches)}
        <div className="container mx-auto mt-16 md:mt-18 max-w-3xl px-3 lg:px-0">
          {inactiveWarning(submissionDetails)}
        </div>
        <div
          className="review-submission-overlay__submission-container relative container mx-auto max-w-3xl px-3 lg:px-0 pb-8">
          {submissionDetails
          ->SubmissionDetails.submissions
          ->Array.mapi((index, overlaySubmission) =>
            <CoursesReview__SubmissionsList
              key={index->string_of_int}
              overlaySubmission
              teamSubmission={submissionDetails->SubmissionDetails.students->Array.length > 1}
              targetEvaluationCriteriaIds={submissionDetails->SubmissionDetails.targetEvaluationCriteriaIds}
              addGradingCB={addGrading(setState, removePendingSubmissionCB, submissionDetails)}
              addFeedbackCB={addFeedbackToReviewedSubmission(
                setState,
                updateReviewedSubmissionCB,
                submissionDetails,
              )}
              submissionNumber={(submissionDetails
              ->SubmissionDetails.submissions
              ->Array.length) - index}
              currentCoach
              evaluationCriteria={submissionDetails->SubmissionDetails.evaluationCriteria}
              reviewChecklist={submissionDetails->SubmissionDetails.reviewChecklist}
              updateReviewChecklistCB={updateReviewChecklist(submissionDetails, setState)}
              targetId={submissionDetails->SubmissionDetails.targetId}
            />
          )
          ->React.array}
        </div>
      </div>

    | Loading =>
      <div>
        <div className="bg-gray-100 py-4">
          <div className="max-w-3xl mx-auto"> {SkeletonLoading.card()} </div>
        </div>
        <div className="max-w-3xl mx-auto">
          {SkeletonLoading.heading()}
          {SkeletonLoading.paragraph()}
          {SkeletonLoading.profileCard()}
          {SkeletonLoading.paragraph()}
        </div>
      </div>
    }}
  </div>
}
