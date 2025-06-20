%raw(`require("./CoursesReview__ChecklistEditor.css")`)

open CoursesReview__Types

type state = {
  reviewChecklist: array<ReviewChecklistItem.t>,
  saving: bool,
}

let str = React.string

module UpdateReviewChecklistMutation = %graphql(`
    mutation UpdateReviewChecklistMutation($targetId: ID!, $reviewChecklist: JSON!) {
      updateReviewChecklist(targetId: $targetId, reviewChecklist: $reviewChecklist){
        success
      }
    }
  `)

let updateReviewChecklist = (targetId, reviewChecklist, setState, updateReviewChecklistCB) => {
  setState(state => {...state, saving: true})

  let trimmedChecklist = reviewChecklist->Array.map(ReviewChecklistItem.trim)

  UpdateReviewChecklistMutation.make(
    ~targetId,
    ~reviewChecklist=ReviewChecklistItem.encodeArray(trimmedChecklist),
    (),
  )
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    if response["updateReviewChecklist"]["success"] {
      updateReviewChecklistCB(trimmedChecklist)
    }

    setState(state => {...state, saving: false})
    Js.Promise.resolve()
  })
  ->ignore
}

let updateChecklistItem = (checklistItem, itemIndex, setState) =>
  setState(state => {
    ...state,
    reviewChecklist: state.reviewChecklist->ReviewChecklistItem.replace(checklistItem, itemIndex),
  })

let updateChecklistItemTitle = (itemIndex, title, checklistItem, setState) =>
  updateChecklistItem(ReviewChecklistItem.updateTitle(title, checklistItem), itemIndex, setState)

let updateChecklistResultTitle = (
  itemIndex,
  resultIndex,
  title,
  reviewChecklistItem,
  resultItem,
  setState,
) => {
  let newReviewChecklistItem =
    reviewChecklistItem->ReviewChecklistItem.updateChecklist(
      reviewChecklistItem
      ->ReviewChecklistItem.result
      ->ReviewChecklistResult.updateTitle(title, resultItem, resultIndex),
    )
  updateChecklistItem(newReviewChecklistItem, itemIndex, setState)
}

let updateChecklistResultFeedback = (
  itemIndex,
  resultIndex,
  feedback,
  reviewChecklistItem,
  resultItem,
  setState,
) => {
  let newReviewChecklistItem =
    reviewChecklistItem->ReviewChecklistItem.updateChecklist(
      reviewChecklistItem
      ->ReviewChecklistItem.result
      ->ReviewChecklistResult.updateFeedback(feedback, resultItem, resultIndex),
    )
  updateChecklistItem(newReviewChecklistItem, itemIndex, setState)
}

let addEmptyResultItem = (reviewChecklistItem, itemIndex, setState) =>
  updateChecklistItem(
    reviewChecklistItem->ReviewChecklistItem.appendEmptyChecklistItem,
    itemIndex,
    setState,
  )

let addEmptyChecklistItem = setState =>
  setState(state => {
    ...state,
    reviewChecklist: ReviewChecklistItem.empty()->Array.append(state.reviewChecklist),
  })

let removeChecklistResult = (itemIndex, resultIndex, reviewChecklistItem, setState) =>
  updateChecklistItem(
    reviewChecklistItem->ReviewChecklistItem.deleteResultItem(resultIndex),
    itemIndex,
    setState,
  )

let removeChecklistItem = (itemIndex, setState) =>
  setState(state => {
    ...state,
    reviewChecklist: state.reviewChecklist->Js.Array.filteri((_el, i) => i != itemIndex),
  })

let initialStateForReviewChecklist = reviewChecklist =>
  reviewChecklist->ArrayUtils.isEmpty ? ReviewChecklistItem.emptyTemplate() : reviewChecklist

let invalidTitle = title => title->String.trim == ""

let invalidChecklist = reviewChecklist =>
  reviewChecklist
  ->Array.map(reviewChecklistItem =>
    reviewChecklistItem->ReviewChecklistItem.title->invalidTitle ||
      reviewChecklistItem
      ->ReviewChecklistItem.result
      ->Js.Array.filter(resultItem => resultItem->ReviewChecklistResult.title->invalidTitle)
      ->ArrayUtils.isNotEmpty
  )
  ->Js.Array.filter(valid => valid)
  ->ArrayUtils.isNotEmpty

@react.component
let make = (~reviewChecklist, ~updateReviewChecklistCB, ~closeEditModeCB, ~targetId) => {
  let (state, setState) = React.useState(() => {
    reviewChecklist: reviewChecklist->initialStateForReviewChecklist,
    saving: false,
  })
  <div
    className="bg-gray-100 border border-primary-200 shadow-inner rounded-lg p-2 pt-0 md:p-4 md:pt-0">
    <DisablingCover disabled=state.saving>
      {state.reviewChecklist
      ->Array.mapi((itemIndex, reviewChecklistItem) =>
        <div
          className="pt-5"
          key={itemIndex->string_of_int}
          ariaLabel={"checklist-item-" ++ (itemIndex->string_of_int)}>
          <div className="flex">
            <div className="w-full">
              <input
                className="checklist-editor__checklist-item-title h-11 text-sm focus:outline-none focus:bg-white focus:border-primary-300"
                id="checklist_title"
                type_="text"
                placeholder="Add an item to the checklist"
                value={reviewChecklistItem->ReviewChecklistItem.title}
                onChange={event =>
                  updateChecklistItemTitle(
                    itemIndex,
                    ReactEvent.Form.target(event)["value"],
                    reviewChecklistItem,
                    setState,
                  )}
              />
              <School__InputGroupError
                message="A checklist item cannot be blank"
                active={reviewChecklistItem->ReviewChecklistItem.title->invalidTitle}
              />
            </div>
            <button
              title="Remove checklist item"
              className="bg-gray-200 p-2 w-11 border border-gray-400 text-gray-700 rounded ml-2 hover:text-red-600 hover:bg-red-100 focus:outline-none"
              onClick={_ => removeChecklistItem(itemIndex, setState)}>
              <i className="fas fa-trash-alt" />
            </button>
          </div>
          <div>
            {reviewChecklistItem
            ->ReviewChecklistItem.result
            ->Array.mapi((resultIndex, resultItem) => {
              let feedback = switch resultItem->ReviewChecklistResult.feedback {
              | Some(f) => f
              | None => ""
              }
              <div
                ariaLabel={"result-item-" ++ (resultIndex->string_of_int)}
                className="pl-2 md:pl-4 mt-2"
                key={(itemIndex->string_of_int) ++ (resultIndex->string_of_int)}>
                <div className="flex">
                  <label
                    title="Disabled"
                    className="flex-shrink-0 rounded border border-gray-400 bg-gray-100 w-4 h-4 mr-2 mt-3 cursor-not-allowed"
                  />
                  <div className="w-full bg-gray-100 relative">
                    <div className="relative">
                      <input
                        className="checklist-editor__checklist-result-item-title h-10 pr-12 focus:outline-none focus:bg-white focus:border-primary-300"
                        id={"result_" ++ ((resultIndex->string_of_int) ++ "_title")}
                        type_="text"
                        placeholder="Add a result for this check"
                        value={resultItem->ReviewChecklistResult.title}
                        onChange={event =>
                          updateChecklistResultTitle(
                            itemIndex,
                            resultIndex,
                            ReactEvent.Form.target(event)["value"],
                            reviewChecklistItem,
                            resultItem,
                            setState,
                          )}
                      />
                      <div
                        className="flex w-10 h-10 absolute top-0 right-0 mr-1 items-center justify-center">
                        <button
                          title="Remove checklist result"
                          className="flex items-center justify-center bg-gray-100 w-7 h-7 mt-px text-sm text-gray-700 hover:text-red-600 hover:bg-red-100 rounded-full ml-2 border border-transparent text-center"
                          onClick={_ =>
                            removeChecklistResult(
                              itemIndex,
                              resultIndex,
                              reviewChecklistItem,
                              setState,
                            )}>
                          <Icon className="if i-times-regular" />
                        </button>
                      </div>
                    </div>
                    <textarea
                      rows=2
                      cols=33
                      className="appearance-none border border-gray-400 bg-transparent rounded-b text-xs align-top py-2 px-4 leading-relaxed w-full focus:outline-none focus:bg-white focus:border-primary-300"
                      id={"result_" ++ ((resultIndex->string_of_int) ++ "_feedback")}
                      type_="text"
                      placeholder="Add feedback (optional)"
                      value=feedback
                      onChange={event =>
                        updateChecklistResultFeedback(
                          itemIndex,
                          resultIndex,
                          ReactEvent.Form.target(event)["value"],
                          reviewChecklistItem,
                          resultItem,
                          setState,
                        )}
                    />
                    <School__InputGroupError
                      message="A check's result cannot be blank"
                      active={resultItem->ReviewChecklistResult.title->invalidTitle}
                    />
                  </div>
                </div>
              </div>
            })
            ->React.array}
            <button
              onClick={_ => addEmptyResultItem(reviewChecklistItem, itemIndex, setState)}
              className="checklist-editor__add-result-btn ml-2 md:ml-4 mt-3 flex items-center focus:outline-none">
              <span
                title="Add Result"
                className="checklist-editor__add-result-btn-check flex-shrink-0 rounded border border-gray-400 bg-gray-100 w-4 h-4 mr-2"
              />
              <span
                className="checklist-editor__add-result-btn-text flex items-center text-sm font-semibold bg-gray-200 px-3 py-1 rounded border border-dashed border-gray-600">
                <i className="fas fa-plus text-xs mr-2" /> {"Add Result"->str}
              </span>
            </button>
          </div>
        </div>
      )
      ->React.array}
      <div className="pt-5">
        <button
          className="flex items-center text-sm font-semibold bg-gray-200 rounded border border-dashed border-gray-600 w-full hover:text-primary-500 hover:bg-white hover:border-primary-500 hover:shadow-md focus:outline-none"
          onClick={_ => addEmptyChecklistItem(setState)}>
          <span className="bg-gray-300 py-2 w-10"> <i className="fas fa-plus text-sm" /> </span>
          <span className="px-3 py-2"> {"Add Checklist Item"->str} </span>
        </button>
      </div>
      <div className="py-2 mt-4 flex flex-row-reverse">
        <button
          disabled={state.saving || invalidChecklist(state.reviewChecklist)}
          onClick={_ =>
            updateReviewChecklist(
              targetId,
              state.reviewChecklist,
              setState,
              updateReviewChecklistCB,
            )}
          className="btn btn-success w-1/2 md:w-auto">
          {"Save Checklist"->str}
        </button>
        <button className="btn btn-subtle w-1/2 md:w-auto mr-4" onClick={_ => closeEditModeCB()}>
          {"Cancel"->str}
        </button>
      </div>
    </DisablingCover>
  </div>
}
