%raw(`require("./EvaluationCriterionEditor__Form.css")`)

let str = React.string

type state = {
  name: string,
  maxGrade: int,
  passGrade: int,
  gradesAndLabels: array<GradeLabel.t>,
  saving: bool,
  dirty: bool,
}

module CreateEvaluationCriterionQuery = %graphql(`
   mutation CreateEvaluationCriterionMutation($name: String!, $courseId: ID!, $maxGrade: Int!, $passGrade: Int!, $gradesAndLabels: [GradeAndLabelInput!]!) {
     createEvaluationCriterion(courseId: $courseId, name: $name, maxGrade: $maxGrade, passGrade: $passGrade, gradesAndLabels: $gradesAndLabels ) {
       evaluationCriterion {
        id
        name
        maxGrade
        passGrade
        gradeLabels {
          grade
          label
        }
       }
     }
   }
   `)

module UpdateEvaluationCriterionQuery = %graphql(`
   mutation UpdateEvaluationCriterionMutation($id: ID!, $name: String!, $gradesAndLabels: [GradeAndLabelInput!]!) {
    updateEvaluationCriterion(id: $id, name: $name, gradesAndLabels: $gradesAndLabels){
       evaluationCriterion {
        id
        name
        maxGrade
        passGrade
        gradeLabels {
          grade
          label
        }
       }
      }
   }
   `)

let formClasses = value =>
  value ? "drawer-right-form w-full opacity-50" : "drawer-right-form w-full"

let possibleGradeValues: list<int> = list{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

let gradeBarBulletClasses = (selected, passed, empty) => {
  let classes = selected ? " grade-bar__pointer--selected" : " "
  if empty {
    classes ++ " grade-bar__pointer--pulse"
  } else if passed {
    classes ++ " grade-bar__pointer--passed"
  } else {
    classes ++ " grade-bar__pointer--failed"
  }
}

let updateMaxGrade = (value, state, setState) =>
  if value < state.passGrade {
    setState(state => {...state, passGrade: value, maxGrade: value})
  } else {
    setState(state => {...state, maxGrade: value})
  }

let updatePassGrade = (value, setState) => setState(state => {...state, passGrade: value})

let updateGradeLabel = (value, gradeAndLabel, state, setState) => {
  let updatedGradeAndLabel = GradeLabel.update(value, gradeAndLabel)
  let gradesAndLabels =
    state.gradesAndLabels->Array.map(gl =>
      gl->GradeLabel.grade == (updatedGradeAndLabel->GradeLabel.grade)
        ? updatedGradeAndLabel
        : gl
    )
  setState(state => {...state, gradesAndLabels: gradesAndLabels, dirty: true})
}

let updateEvaluationCriterion = (state, setState, addOrUpdateCriterionCB, criterion) => {
  setState(state => {...state, saving: true})

  let jsGradeAndLabelArray =
    state.gradesAndLabels
    ->Js.Array.filter(gradesAndLabel => gradesAndLabel->GradeLabel.grade <= state.maxGrade)
    ->Array.map(gl => gl->GradeLabel.asJsObject)

  UpdateEvaluationCriterionQuery.make(
    ~id=criterion->EvaluationCriterion.id,
    ~name=state.name,
    ~gradesAndLabels=jsGradeAndLabelArray,
    (),
  )
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(result => {
    switch result["updateEvaluationCriterion"]["evaluationCriterion"] {
    | Some(criterion) =>
      let updatedCriterion = EvaluationCriterion.makeFromJs(criterion)
      addOrUpdateCriterionCB(updatedCriterion)
      setState(state => {...state, saving: false})
    | None => setState(state => {...state, saving: false})
    }
    Js.Promise.resolve()
  })
  ->ignore
}

let createEvaluationCriterion = (state, setState, addOrUpdateCriterionCB, courseId) => {
  setState(state => {...state, saving: true})

  let jsGradeAndLabelArray =
    state.gradesAndLabels
    ->Js.Array.filter(gradesAndLabel => gradesAndLabel->GradeLabel.grade <= state.maxGrade)
    ->Array.map(gl => gl->GradeLabel.asJsObject)

  CreateEvaluationCriterionQuery.make(
    ~name=state.name,
    ~maxGrade=state.maxGrade,
    ~passGrade=state.passGrade,
    ~courseId,
    ~gradesAndLabels=jsGradeAndLabelArray,
    (),
  )
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(result => {
    switch result["createEvaluationCriterion"]["evaluationCriterion"] {
    | Some(criterion) =>
      let newCriterion = EvaluationCriterion.makeFromJs(criterion)
      addOrUpdateCriterionCB(newCriterion)
      setState(state => {...state, saving: false})
    | None => setState(state => {...state, saving: false})
    }
    Js.Promise.resolve()
  })
  ->ignore
}

let updateName = (setState, value) => setState(state => {...state, dirty: true, name: value})

let saveDisabled = state => {
  let hasValidName = state.name->String.trim->String.length > 0
  !state.dirty || (state.saving || !hasValidName)
}

let labelClasses = (grade, passGrade) => {
  let failGradeClasses = "bg-red-300 text-red-700 border-red-500"
  let passGradeClasses = "bg-green-300 text-green-700 border-green-500"
  "w-12 p-3 text-center  mr-3 rounded-lg border  leading-tight " ++ (
    grade < passGrade ? failGradeClasses : passGradeClasses
  )
}

let labels = (state, setState) =>
  state.gradesAndLabels
  ->Js.Array.filter(gnl => gnl->GradeLabel.grade <= state.maxGrade)
  ->Array.map(gradeAndLabel => {
    let grade = gradeAndLabel->GradeLabel.grade

    <div key={grade->string_of_int} className="flex flex-wrap mt-2">
      <div className={labelClasses(grade, state.passGrade)}> {grade->string_of_int->str} </div>
      <div className="flex-1">
        <input
          id={"grade-label-for-" ++ (grade->string_of_int)}
          className=" appearance-none border rounded w-full p-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
          type_="text"
          maxLength=40
          value={gradeAndLabel->GradeLabel.label}
          onChange={event =>
            updateGradeLabel(
              ReactEvent.Form.target(event)["value"],
              gradeAndLabel,
              state,
              setState,
            )}
          placeholder={"Label for grade " ++ (gradeAndLabel->GradeLabel.grade->string_of_int)}
        />
      </div>
    </div>
  })

@react.component
let make = (~evaluationCriterion, ~courseId, ~addOrUpdateCriterionCB) => {
  let (state, setState) = React.useState(() =>
    switch evaluationCriterion {
    | None => {
        name: "",
        maxGrade: 5,
        passGrade: 2,
        gradesAndLabels: possibleGradeValues->List.map(i => GradeLabel.empty(i))->Array.of_list,
        saving: false,
        dirty: false,
      }
    | Some(ec) => {
        name: ec->EvaluationCriterion.name,
        maxGrade: ec->EvaluationCriterion.maxGrade,
        passGrade: ec->EvaluationCriterion.passGrade,
        gradesAndLabels: ec->EvaluationCriterion.gradesAndLabels,
        saving: false,
        dirty: false,
      }
    }
  )
  <div className="mx-auto bg-white">
    <div className="max-w-2xl p-6 mx-auto">
      <h5 className="uppercase text-center border-b border-gray-400 pb-2">
        {switch evaluationCriterion {
        | None => "Add Evaluation Criterion"
        | Some(ec) => ec->EvaluationCriterion.name
        }->str}
      </h5>
      <DisablingCover
        disabled=state.saving
        message={switch evaluationCriterion {
        | Some(_ec) => "Updating..."
        | None => "Saving..."
        }}>
        <div key="evaluation-criterion-editor" className="mt-3">
          <div className="mt-5">
            <label className="inline-block tracking-wide text-xs font-semibold " htmlFor="name">
              {"Name"->str}
            </label>
            <input
              className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
              id="name"
              onChange={event => updateName(setState, ReactEvent.Form.target(event)["value"])}
              type_="text"
              placeholder="Evaluation criterion name"
              maxLength=50
              value=state.name
            />
            <School__InputGroupError
              message="Enter a valid name"
              active={state.dirty && state.name->String.trim->String.length < 1}
            />
          </div>
        </div>
        <div className="mx-auto">
          <div className="max-w-2xl pt-6 mx-auto">
            <div className="mb-4">
              <span
                className="inline-block tracking-wide text-sm font-semibold mr-2"
                htmlFor="max_grades">
                {"Maximum grade is"->str}
              </span>
              {switch evaluationCriterion {
              | Some(_) =>
                <span
                  className="cursor-not-allowed inline-block bg-white border-b-2 text-2xl font-semibold text-center border-blue px-3 py-2 leading-tight rounded-none focus:outline-none">
                  {state.maxGrade->string_of_int->str}
                </span>
              | None =>
                <select
                  onChange={event =>
                    updateMaxGrade(
                      ReactEvent.Form.target(event)["value"]->int_of_string,
                      state,
                      setState,
                    )}
                  id="max_grade"
                  value={state.maxGrade->string_of_int}
                  className="cursor-pointer inline-block appearance-none bg-white border-b-2 text-2xl font-semibold text-center border-blue hover:border-gray-500 px-3 py-2 leading-tight rounded-none focus:outline-none">
                  {possibleGradeValues
                  ->List.filter(g => g != 1)
                  ->List.map(possibleGradeValue =>
                    <option
                      key={possibleGradeValue->string_of_int}
                      value={possibleGradeValue->string_of_int}>
                      {possibleGradeValue->string_of_int->str}
                    </option>
                  )
                  ->Array.of_list
                  ->ReasonReact.array}
                </select>
              }}
              <span
                className="inline-block tracking-wide text-sm font-semibold mx-2"
                htmlFor="pass_grades">
                {"and the passing grade is"->str}
              </span>
              {switch evaluationCriterion {
              | Some(_) =>
                <span
                  className="cursor-not-allowed inline-block appearance-none bg-white border-b-2 text-2xl font-semibold text-center border-blue px-3 py-2 leading-tight rounded-none">
                  {state.passGrade->string_of_int->str}
                </span>
              | None =>
                <select
                  onChange={event =>
                    updatePassGrade(
                      ReactEvent.Form.target(event)["value"]->int_of_string,
                      setState,
                    )}
                  id="pass_grade"
                  value={state.passGrade->string_of_int}
                  className="cursor-pointer inline-block appearance-none bg-white border-b-2 text-2xl font-semibold text-center border-blue hover:border-gray-500 px-3 py-2 rounded-none leading-tight focus:outline-none">
                  {possibleGradeValues
                  ->List.filter(g => g <= state.maxGrade)
                  ->List.map(possibleGradeValue =>
                    <option
                      key={possibleGradeValue->string_of_int}
                      value={possibleGradeValue->string_of_int}>
                      {possibleGradeValue->string_of_int->str}
                    </option>
                  )
                  ->Array.of_list
                  ->ReasonReact.array}
                </select>
              }}
            </div>
            <div className="flex justify-between">
              <div className="flex items-center">
                <label className="block tracking-wide text-xs font-semibold" htmlFor="grades">
                  {"Grade and Labels"->str}
                </label>
                <HelpIcon className="ml-2" link="https://docs.pupilfirst.com/#/evaluation_criteria">
                  {"Assign labels for each grade, to reflect in grade cards for students and coaches"->str}
                </HelpIcon>
              </div>
              <div className="flex">
                <div className="flex justify-center items-center ml-4">
                  <span className="grade-bar__pointer-legend grade-bar__pointer-legend-failed" />
                  <span className="ml-2 text-xs"> {"Fail"->str} </span>
                </div>
                <div className="flex justify-center items-center ml-4">
                  <span className="grade-bar__pointer-legend grade-bar__pointer-legend-passed" />
                  <span className="ml-2 text-xs"> {"Pass"->str} </span>
                </div>
              </div>
            </div>
            <div ariaLabel="label-editor"> {labels(state, setState)->React.array} </div>
            <div className="mt-3 mb-3 text-xs">
              <span className="leading-normal">
                <strong> {"Important:"->str} </strong>
                {" The values for maximum and passing grades cannot be modified once a criterion is created. Labels given to each grade can be edited later on."->str}
              </span>
            </div>
            <div className="flex">
              {switch evaluationCriterion {
              | Some(criterion) =>
                <button
                  disabled={saveDisabled(state)}
                  onClick={_ =>
                    updateEvaluationCriterion(state, setState, addOrUpdateCriterionCB, criterion)}
                  className="w-full btn btn-large btn-primary mt-3">
                  {"Update Criterion"->str}
                </button>

              | None =>
                <button
                  disabled={saveDisabled(state)}
                  onClick={_ =>
                    createEvaluationCriterion(state, setState, addOrUpdateCriterionCB, courseId)}
                  className="w-full btn btn-large btn-primary mt-3">
                  {"Create Criterion"->str}
                </button>
              }}
            </div>
          </div>
        </div>
      </DisablingCover>
    </div>
  </div>
}
