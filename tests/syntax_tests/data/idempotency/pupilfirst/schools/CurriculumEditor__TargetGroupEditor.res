open CurriculumEditor__Types

let str = ReasonReact.string
type state = {
  name: string,
  description: string,
  milestone: bool,
  hasNameError: bool,
  dirty: bool,
  isArchived: bool,
  saving: bool,
}

type action =
  | UpdateName(string, bool)
  | UpdateDescription(string)
  | UpdateMilestone(bool)
  | UpdateIsArchived(bool)
  | UpdateSaving

let reducer = (state, action) =>
  switch action {
  | UpdateName(name, hasNameError) => {
      ...state,
      name: name,
      hasNameError: hasNameError,
      dirty: true,
    }
  | UpdateDescription(description) => {...state, description: description, dirty: true}
  | UpdateMilestone(milestone) => {...state, milestone: milestone, dirty: true}
  | UpdateIsArchived(isArchived) => {...state, isArchived: isArchived, dirty: true}
  | UpdateSaving => {...state, saving: !state.saving}
  }

let updateName = (send, name) => {
  let hasError = name->String.length < 2
  send(UpdateName(name, hasError))
}

let saveDisabled = state => state.hasNameError || (!state.dirty || state.saving)

let setPayload = (authenticityToken, state) => {
  let payload = Js.Dict.empty()
  let milestone = state.milestone == true ? "true" : "false"
  Js.Dict.set(payload, "authenticity_token", authenticityToken->Js.Json.string)
  Js.Dict.set(payload, "archived", state.isArchived->Js.Json.boolean)
  Js.Dict.set(payload, "name", state.name->Js.Json.string)
  Js.Dict.set(payload, "description", state.description->Js.Json.string)
  Js.Dict.set(payload, "milestone", milestone->Js.Json.string)
  payload
}

let booleanButtonClasses = selected => {
  let classes = "toggle-button__button"
  classes ++ (selected ? " toggle-button__button--active" : "")
}
let formClasses = value =>
  value ? "drawer-right-form w-full opacity-50" : "drawer-right-form w-full"

let computeInitialState = targetGroup =>
  switch targetGroup {
  | Some(targetGroup) => {
      name: targetGroup->TargetGroup.name,
      description: switch targetGroup->TargetGroup.description {
      | Some(description) => description
      | None => ""
      },
      milestone: targetGroup->TargetGroup.milestone,
      hasNameError: false,
      dirty: false,
      isArchived: targetGroup->TargetGroup.archived,
      saving: false,
    }
  | None => {
      name: "",
      description: "",
      milestone: true,
      hasNameError: false,
      dirty: false,
      isArchived: false,
      saving: false,
    }
  }

@react.component
let make = (
  ~targetGroup,
  ~currentLevelId,
  ~authenticityToken,
  ~updateTargetGroupsCB,
  ~hideEditorActionCB,
) => {
  let (state, send) = React.useReducerWithMapState(reducer, targetGroup, computeInitialState)
  let handleErrorCB = () => send(UpdateSaving)
  let handleResponseCB = json => {
    let id = json->{
      open Json.Decode
      field("id", string)
    }
    let sortIndex = json->{
      open Json.Decode
      field("sortIndex", int)
    }
    let newTargetGroup = TargetGroup.create(
      id,
      state.name,
      Some(state.description),
      state.milestone,
      currentLevelId,
      sortIndex,
      state.isArchived,
    )
    switch targetGroup {
    | Some(_) => Notification.success("Success", "Target Group updated successfully")
    | None => Notification.success("Success", "Target Group created successfully")
    }
    updateTargetGroupsCB(newTargetGroup)
  }

  let createTargetGroup = () => {
    send(UpdateSaving)
    let level_id = currentLevelId
    let payload = setPayload(authenticityToken, state)
    let url = "/school/levels/" ++ (level_id ++ "/target_groups")
    Api.create(url, payload, handleResponseCB, handleErrorCB)
  }

  let updateTargetGroup = targetGroupId => {
    send(UpdateSaving)
    let payload = setPayload(authenticityToken, state)
    let url = "/school/target_groups/" ++ targetGroupId
    Api.update(url, payload, handleResponseCB, handleErrorCB)
  }
  <div>
    <div className="blanket" />
    <div className="drawer-right">
      <div className="drawer-right__close absolute">
        <button
          title="close"
          onClick={_ => hideEditorActionCB()}
          className="flex items-center justify-center bg-white text-gray-600 font-bold py-3 px-5 rounded-l-full rounded-r-none hover:text-gray-600 focus:outline-none mt-4">
          <i className="fas fa-times text-xl" />
        </button>
      </div>
      <div className={formClasses(state.saving)}>
        <div className="w-full">
          <div className="mx-auto bg-white">
            <div className="max-w-2xl pt-6 px-6 mx-auto">
              <h5 className="uppercase text-center border-b border-gray-400 pb-2">
                {"Target Group Details"->str}
              </h5>
              <div className="mt-5">
                <label className="inline-block tracking-wide text-xs font-semibold" htmlFor="name">
                  {"Title"->str}
                </label>
                <span> {"*"->str} </span>
                <input
                  className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                  id="name"
                  type_="text"
                  placeholder="Type target group name here"
                  value=state.name
                  onChange={event => updateName(send, ReactEvent.Form.target(event)["value"])}
                />
                {state.hasNameError
                  ? <div className="drawer-right-form__error-msg">
                      <span className="mr-2"> <i className="fas fa-exclamation-triangle" /> </span>
                      <span> {"not a valid Title"->str} </span>
                    </div>
                  : ReasonReact.null}
              </div>
              <div className="mt-5">
                <label className="block tracking-wide text-xs font-semibold" htmlFor="description">
                  {" Description"->str}
                </label>
                <textarea
                  className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                  id="description"
                  placeholder="Type target group description"
                  value=state.description
                  onChange={event =>
                    send(UpdateDescription(ReactEvent.Form.target(event)["value"]))}
                  rows=5
                  cols=33
                />
              </div>
              <div className="mt-5">
                <div className="flex items-center flex-shrink-0">
                  <label className="block tracking-wide text-xs font-semibold mr-3">
                    {"Is this a milestone target group?"->str}
                  </label>
                  <div
                    className="milestone flex-shrink-0 rounded-lg overflow-hidden border border-gray-400">
                    <button
                      onClick={_event => {
                        ReactEvent.Mouse.preventDefault(_event)
                        send(UpdateMilestone(true))
                      }}
                      className={booleanButtonClasses(state.milestone == true)}>
                      {"Yes"->str}
                    </button>
                    <button
                      onClick={_event => {
                        ReactEvent.Mouse.preventDefault(_event)
                        send(UpdateMilestone(false))
                      }}
                      className={booleanButtonClasses(state.milestone == false)}>
                      {"No"->str}
                    </button>
                  </div>
                </div>
              </div>
            </div>
            <div className="border-t bg-gray-100 mt-5">
              <div className="max-w-2xl p-6 mx-auto flex w-full justify-between items-center">
                {switch targetGroup {
                | Some(_) =>
                  <div className="flex items-center mr-2">
                    <label className="block tracking-wide text-xs font-semibold mr-6">
                      {"Is this target group archived?"->str}
                    </label>
                    <div
                      className="toggle-button__group archived inline-flex flex-shrink-0 rounded-lg overflow-hidden">
                      <button
                        onClick={_event => {
                          ReactEvent.Mouse.preventDefault(_event)
                          send(UpdateIsArchived(true))
                        }}
                        className={booleanButtonClasses(state.isArchived == true)}>
                        {"Yes"->str}
                      </button>
                      <button
                        onClick={_event => {
                          ReactEvent.Mouse.preventDefault(_event)
                          send(UpdateIsArchived(false))
                        }}
                        className={booleanButtonClasses(state.isArchived == false)}>
                        {"No"->str}
                      </button>
                    </div>
                  </div>
                | None => ReasonReact.null
                }}
                {switch targetGroup {
                | Some(targetGroup) =>
                  let id = targetGroup->TargetGroup.id
                  <div className="w-auto">
                    <button
                      disabled={saveDisabled(state)}
                      onClick={_e => updateTargetGroup(id)}
                      className="btn btn-primary btn-large">
                      {"Update Target Group"->str}
                    </button>
                  </div>

                | None =>
                  <div className="w-full">
                    <button
                      disabled={saveDisabled(state)}
                      onClick={_e => createTargetGroup()}
                      className="w-full btn btn-primary btn-large">
                      {"Create Target Group"->str}
                    </button>
                  </div>
                }}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
}
