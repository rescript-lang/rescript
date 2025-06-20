let str = React.string

open CoursesCurriculum__Types

@react.component
let make = (~targetDetails, ~title) =>
  switch targetDetails->TargetDetails.completionInstructions {
  | Some(completionInstructions) =>
    <div
      className="flex flex-col sm:flex-row mt-4 bg-yellow-100 rounded-lg border border-yellow-500 overflow-hidden">
      <div className="bg-yellow-500 p-4 sm:py-8 sm:px-7 flex-shrink-0 text-center">
        <Icon className="if i-lamp-solid text-3xl sm:text-5xl text-yellow-800" />
      </div>
      <div className="p-5 text-center sm:text-left">
        <h4 className="font-semibold text-lg"> {title->str} </h4>
        <p className="mt-1 leading-snug"> {completionInstructions->str} </p>
      </div>
    </div>
  | None => React.null
  }
