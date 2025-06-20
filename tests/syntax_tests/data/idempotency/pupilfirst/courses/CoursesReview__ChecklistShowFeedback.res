%raw(`require("./CoursesReview__ChecklistShowFeedback.css")`)

let str = React.string

let feedbackClasses = truncated => truncated ? "relative overflow-hidden h-12" : "relative h-auto"

let optionalStringLength = feedback =>
  switch feedback {
  | Some(f) => f->String.length
  | None => 0
  }

@react.component
let make = (~feedback) => {
  let truncationRequired = feedback->optionalStringLength > 150

  let (truncated, setTruncated) = React.useState(() => truncationRequired)

  switch feedback {
  | Some(feedback) =>
    <div className={feedbackClasses(truncated)}>
      <MarkdownBlock markdown=feedback className="text-sm" profile=Markdown.Permissive />
      {truncationRequired && truncated
        ? <div className="checklist-show-feedback__show-all-button absolute bottom-0 w-full">
            <button
              className="block w-full text-center rounded-lg text-primary-500 text-xs font-semibold focus:outline-none hover:text-primary-400"
              onClick={_ => setTruncated(_ => false)}>
              <span className="inline-block bg-gray-100 px-2"> {"Show All"->str} </span>
            </button>
          </div>
        : React.null}
    </div>
  | None => React.null
  }
}
