open CoachesSchoolIndex__Types

let str = React.string

type formVisible =
  | None
  | CoachEditor(option<Coach.t>)

type state = {
  coaches: list<Coach.t>,
  formVisible: formVisible,
}

type action =
  | UpdateFormVisible(formVisible)
  | UpdateCoaches(Coach.t)

let reducer = (state, action) =>
  switch action {
  | UpdateFormVisible(formVisible) => {...state, formVisible: formVisible}
  | UpdateCoaches(coach) =>
    let newCoachesList = coach->Coach.updateList(state.coaches)
    {...state, coaches: newCoachesList}
  }

@react.component
let make = (~coaches, ~authenticityToken) => {
  let (state, send) = React.useReducer(reducer, {coaches: coaches, formVisible: None})

  let closeFormCB = () => send(UpdateFormVisible(None))
  let updateCoachCB = coach => send(UpdateCoaches(coach))
  <div className="flex flex-1 h-full overflow-y-scroll">
    {switch state.formVisible {
    | None => React.null
    | CoachEditor(coach) =>
      <SA_Coaches_CoachEditor coach closeFormCB updateCoachCB authenticityToken />
    }}
    <div className="flex-1 flex flex-col">
      <div className="flex px-6 py-2 items-center justify-between">
        <button
          onClick={_event => {
            ReactEvent.Mouse.preventDefault(_event)
            send(UpdateFormVisible(CoachEditor(None)))
          }}
          className="max-w-2xl w-full flex mx-auto items-center justify-center relative bg-white text-primary-500 hove:bg-gray-100 hover:text-primary-600 hover:shadow-lg focus:outline-none border-2 border-gray-400 border-dashed hover:border-primary-300 p-6 rounded-lg mt-8 cursor-pointer">
          <i className="fas fa-plus-circle text-lg" />
          <h5 className="font-semibold ml-2"> {"Add New Coach"->str} </h5>
        </button>
      </div>
      <div className="px-6 pb-4 mt-5 flex flex-1">
        <div className="max-w-2xl w-full mx-auto relative">
          {state.coaches
          ->List.sort((x, y) => (x->Coach.id) - (y->Coach.id))
          ->List.map(coach =>
            <div
              key={coach->Coach.id->string_of_int}
              className="flex items-center shadow bg-white rounded-lg mb-4 overflow-hidden">
              <div className="course-faculty__list-item flex w-full">
                <div
                  className="course-faculty__list-item-details flex flex-1 items-center justify-between cursor-pointer hover:bg-gray-100 hover:text-primary-500"
                  onClick={_event => {
                    ReactEvent.Mouse.preventDefault(_event)
                    send(UpdateFormVisible(CoachEditor(Some(coach))))
                  }}>
                  <div className="flex flex-1 py-4 px-4">
                    <img
                      className="w-10 h-10 rounded-full mr-4 object-cover"
                      src={coach->Coach.imageUrl}
                      alt={"Avatar of " ++ (coach->Coach.name)}
                    />
                    <div className="text-sm">
                      <p className="font-semibold"> {coach->Coach.name->str} </p>
                      <p className="text-gray-600 font-semibold text-xs mt-px">
                        {coach->Coach.title->str}
                      </p>
                    </div>
                  </div>
                  <span
                    className="flex items-center flex-shrink-0 ml-2 py-4 px-4 font-semibold text-gray-700 hover:text-primary-500 text-sm">
                    <i className="fas fa-edit text-normal" />
                    <span className="ml-1"> {"Edit"->str} </span>
                  </span>
                </div>
              </div>
            </div>
          )
          ->Array.of_list
          ->React.array}
        </div>
      </div>
    </div>
  </div>
}
