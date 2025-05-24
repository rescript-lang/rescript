let str = React.string

@react.component
let make = (
  ~feedback,
  ~updateFeedbackCB,
  ~label,
  ~reviewChecklist,
  ~updateReviewChecklistCB,
  ~checklistVisible,
  ~targetId,
) => {
  let (checklistVisible, setChecklistVisible) = React.useState(() => checklistVisible)
  let reviewChecklistIsNotEmpty = reviewChecklist->ArrayUtils.isNotEmpty
  <div>
    <div>
      {switch (checklistVisible, reviewChecklistIsNotEmpty) {
      | (true, _)
      | (false, false) =>
        <CoursesReview__Checklist
          reviewChecklist updateFeedbackCB feedback updateReviewChecklistCB targetId
        />

      | (false, true) =>
        <div className="px-4 pt-4 md:px-6 pt-6">
          <button
            className="flex items-center bg-gray-100 border p-4 rounded-lg w-full text-left text-primary-500 font-semibold hover:bg-gray-200 hover:border-primary-300 focus:outline-none"
            onClick={_ => setChecklistVisible(_ => true)}>
            <span
              className="inline-flex w-10 h-10 border border-white items-center justify-center rounded-full bg-primary-100 text-primary-500">
              <i className="fas fa-list" />
            </span>
            <span className="ml-3"> {"Show Review Checklist"->str} </span>
          </button>
        </div>
      }}
    </div>
    <div className="px-4 pt-4 md:px-6 md:pt-6 course-review__feedback-editor text-sm">
      <h5 className="font-semibold text-sm flex items-center">
        <PfIcon
          className="if i-comment-alt-regular text-gray-800 text-base md:text-lg inline-block"
        />
        <span className="ml-2 md:ml-3 tracking-wide"> {label->str} </span>
      </h5>
      <div className="mt-2 md:ml-7" ariaLabel="feedback">
        <MarkdownEditor
          onChange=updateFeedbackCB
          value=feedback
          profile=Markdown.Permissive
          maxLength=10000
          placeholder="This feedback will be emailed to students when you finish grading."
        />
      </div>
    </div>
  </div>
}
