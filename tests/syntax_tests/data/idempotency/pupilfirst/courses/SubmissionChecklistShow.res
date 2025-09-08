let str = React.string

@react.component
let make = (~checklist, ~updateChecklistCB, ~pending) =>
  <div>
    {checklist->ArrayUtils.isEmpty
      ? <div> {"Target was marked as complete."->str} </div>
      : checklist
        ->Array.mapi((index, checklistItem) =>
          <SubmissionChecklistItemShow
            key={index->string_of_int} index checklistItem updateChecklistCB checklist pending
          />
        )
        ->React.array}
  </div>
