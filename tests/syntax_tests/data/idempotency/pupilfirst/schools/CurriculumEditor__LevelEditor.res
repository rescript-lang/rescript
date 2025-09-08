open CurriculumEditor__Types

let str = ReasonReact.string

type state = {
  name: string,
  unlockOn: option<Js.Date.t>,
  hasNameError: bool,
  dirty: bool,
  saving: bool,
}

type action =
  | UpdateName(string, bool)
  | UpdateUnlockOn(option<Js.Date.t>)
  | UpdateSaving

let reducer = (state, action) =>
  switch action {
  | UpdateName(name, hasNameError) => {
      ...state,
      name: name,
      hasNameError: hasNameError,
      dirty: true,
    }
  | UpdateUnlockOn(date) => {...state, unlockOn: date, dirty: true}
  | UpdateSaving => {...state, saving: !state.saving}
  }

let updateName = (send, name) => {
  let hasError = name->String.trim->String.length < 2
  send(UpdateName(name, hasError))
}

let saveDisabled = state => state.hasNameError || (!state.dirty || state.saving)

let setPayload = (authenticityToken, state) => {
  let payload = Js.Dict.empty()

  Js.Dict.set(payload, "authenticity_token", authenticityToken->Js.Json.string)
  Js.Dict.set(payload, "name", state.name->Js.Json.string)

  switch state.unlockOn {
  | Some(date) => Js.Dict.set(payload, "unlock_on", date->Date.iso8601->Js.Json.string)
  | None => Js.Dict.set(payload, "unlock_on", ""->Js.Json.string)
  }
  payload
}
let formClasses = value =>
  value ? "drawer-right-form w-full opacity-50" : "drawer-right-form w-full"

let computeInitialState = level =>
  switch level {
  | Some(level) => {
      name: level->Level.name,
      unlockOn: level->Level.unlockOn,
      hasNameError: false,
      dirty: false,
      saving: false,
    }
  | None => {
      name: "",
      unlockOn: None,
      hasNameError: false,
      dirty: false,
      saving: false,
    }
  }

@react.component
let make = (~level, ~course, ~authenticityToken, ~hideEditorActionCB, ~updateLevelsCB) => {
  let (state, send) = React.useReducerWithMapState(reducer, level, computeInitialState)
  let handleErrorCB = () => send(UpdateSaving)
  let handleResponseCB = json => {
    let id = json->{
      open Json.Decode
      field("id", string)
    }
    let number = json->{
      open Json.Decode
      field("number", int)
    }
    let newLevel = Level.create(id, state.name, number, state.unlockOn)
    switch level {
    | Some(_) => Notification.success("Success", "Level updated successfully")
    | None => Notification.success("Success", "Level created successfully")
    }
    updateLevelsCB(newLevel)
  }

  let createLevel = (authenticityToken, course, state) => {
    send(UpdateSaving)
    let course_id = course->Course.id
    let url = "/school/courses/" ++ (course_id ++ "/levels")
    Api.create(url, setPayload(authenticityToken, state), handleResponseCB, handleErrorCB)
  }

  let updateLevel = (authenticityToken, levelId, state) => {
    send(UpdateSaving)
    let url = "/school/levels/" ++ levelId
    Api.update(url, setPayload(authenticityToken, state), handleResponseCB, handleErrorCB)
  }

  <div>
    <div className="blanket" />
    <div className="drawer-right">
      <div className="drawer-right__close absolute">
        <button
          title="close"
          onClick={_ => hideEditorActionCB()}
          className="flex items-center justify-center bg-white text-gray-600 font-bold py-3 px-5 rounded-l-full rounded-r-none hover:text-gray-700 focus:outline-none mt-4">
          <i className="fas fa-times text-xl" />
        </button>
      </div>
      <div className={formClasses(state.saving)}>
        <div className="w-full">
          <div className="mx-auto bg-white">
            <div className="max-w-2xl p-6 mx-auto">
              <h5 className="uppercase text-center border-b pb-2 mb-4">
                {"Level Details"->str}
              </h5>
              <div className="mt-5">
                <label className="inline-block tracking-wide text-xs font-semibold" htmlFor="name">
                  {"Level Name"->str}
                </label>
                <span> {"*"->str} </span>
                <input
                  className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                  id="name"
                  type_="text"
                  placeholder="Type level name here"
                  value=state.name
                  onChange={event => updateName(send, ReactEvent.Form.target(event)["value"])}
                />
                {state.hasNameError
                  ? <div className="drawer-right-form__error-msg">
                      {"not a valid name"->str}
                    </div>
                  : ReasonReact.null}
              </div>
              <div className="mt-5">
                <label
                  className="block tracking-wide text-xs font-semibold" htmlFor="unlock-on-input">
                  {"Unlock level on"->str}
                </label>
                <DatePicker
                  id="unlock-on-input"
                  selected=?state.unlockOn
                  onChange={date => send(UpdateUnlockOn(date))}
                />
              </div>
              <div className="flex mt-5">
                {switch level {
                | Some(level) =>
                  let id = level->Level.id
                  <button
                    disabled={saveDisabled(state)}
                    onClick={_event => updateLevel(authenticityToken, id, state)}
                    className="w-full btn btn-large btn-primary">
                    {"Update Level"->str}
                  </button>

                | None =>
                  <button
                    disabled={saveDisabled(state)}
                    onClick={_event => createLevel(authenticityToken, course, state)}
                    className="w-full btn btn-large btn-primary">
                    {"Create New Level"->str}
                  </button>
                }}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
}
