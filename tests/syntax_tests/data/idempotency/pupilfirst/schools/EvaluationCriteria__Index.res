let str = React.string

type editorAction =
  | ShowEditor(option<EvaluationCriterion.t>)
  | Hidden

type state = {
  editorAction: editorAction,
  evaluationCriteria: array<EvaluationCriterion.t>,
}

let openEditor = (event, evaluationCriterion, setState) => {
  event->ReactEvent.Mouse.preventDefault
  setState(state => {...state, editorAction: ShowEditor(Some(evaluationCriterion))})
}

let showEvaluationCriterion = (evaluationCriterion, setState) =>
  <div
    key={evaluationCriterion->EvaluationCriterion.id}
    className="flex items-center shadow bg-white rounded-lg mb-4">
    <div className="course-faculty__list-item flex w-full items-center">
      <a
        title={"Edit " ++ (evaluationCriterion->EvaluationCriterion.name)}
        onClick={event => openEditor(event, evaluationCriterion, setState)}
        className="course-faculty__list-item-details flex flex-1 items-center justify-between border border-transparent cursor-pointer rounded-l-lg hover:bg-gray-100 hover:text-primary-500 hover:border-primary-400">
        <div className="flex w-full text-sm justify-between">
          <span className="flex-1 font-semibold py-5 px-5">
            {evaluationCriterion->EvaluationCriterion.name->str}
          </span>
          <span className="ml-2 py-5 px-5 font-semibold text-gray-700 hover:text-primary-500">
            <i className="fas fa-edit text-normal" />
            <span className="ml-1"> {"Edit"->str} </span>
          </span>
        </div>
      </a>
    </div>
  </div>

let addOrUpdateCriterionCB = (state, setState, criterion) => {
  let updatedCriteria =
    state.evaluationCriteria
    ->Js.Array.filter(ec => EvaluationCriterion.id(ec) != EvaluationCriterion.id(criterion))
    ->Array.append([criterion])
  setState(_ => {evaluationCriteria: updatedCriteria, editorAction: Hidden})
}

@react.component
let make = (~courseId, ~evaluationCriteria) => {
  let (state, setState) = React.useState(() => {
    editorAction: Hidden,
    evaluationCriteria: evaluationCriteria,
  })

  <div className="flex-1 flex flex-col overflow-y-scroll bg-gray-200">
    {switch state.editorAction {
    | Hidden => React.null
    | ShowEditor(evaluationCriterion) =>
      <SchoolAdmin__EditorDrawer
        closeDrawerCB={() => setState(state => {...state, editorAction: Hidden})}>
        <EvaluationCriterionEditor__Form
          evaluationCriterion
          courseId
          addOrUpdateCriterionCB={addOrUpdateCriterionCB(state, setState)}
        />
      </SchoolAdmin__EditorDrawer>
    }}
    <div className="flex px-6 py-2 items-center justify-between">
      <button
        onClick={_ => setState(state => {...state, editorAction: ShowEditor(None)})}
        className="max-w-2xl w-full flex mx-auto items-center justify-center relative bg-white text-primary-500 hover:bg-gray-100 hover:text-primary-600 hover:shadow-lg focus:outline-none border-2 border-gray-400 border-dashed hover:border-primary-300 p-6 rounded-lg mt-8 cursor-pointer">
        <i className="fas fa-plus-circle" />
        <h5 className="font-semibold ml-2"> {"Add New Evaluation Criterion"->str} </h5>
      </button>
    </div>
    <div className="px-6 pb-4 mt-5 flex flex-1">
      <div className="max-w-2xl w-full mx-auto relative">
        {state.evaluationCriteria
        ->EvaluationCriterion.sort
        ->Array.map(ec => showEvaluationCriterion(ec, setState))
        ->React.array}
      </div>
    </div>
  </div>
}
