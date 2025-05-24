let str = ReasonReact.string

type action =
  | UpdateName(string)
  | UpdateAbout(string)
  | UpdateSaving(bool)

type state = {
  name: string,
  about: string,
  saving: bool,
  formDirty: bool,
}
let updateButtonText = saving => saving ? "Updating..." : "Update"

module UpdateSchoolQuery = %graphql(`
  mutation UpdateSchoolMutation($name: String!, $about: String!) {
    updateSchool(about: $about, name: $name) {
      success
    }
  }
`)

let optionAbout = about => about == "" ? None : Some(about)

let updateSchoolQuery = (state, send, updateDetailsCB) => {
  send(UpdateSaving(true))

  UpdateSchoolQuery.make(~name=state.name, ~about=state.about, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    response["updateSchool"]["success"]
      ? updateDetailsCB(state.name, optionAbout(state.about))
      : send(UpdateSaving(false))
    Js.Promise.resolve()
  })
  ->ignore
}

let updateButtonDisabled = state =>
  !state.formDirty || (state.saving || state.name->String.length < 1)

let initialState = (name, about) => {
  name: name,
  about: about->OptionUtils.default(""),
  saving: false,
  formDirty: false,
}

let reducer = (state, action) =>
  switch action {
  | UpdateName(name) => {...state, name: name, formDirty: true}
  | UpdateAbout(about) => {...state, about: about, formDirty: true}
  | UpdateSaving(saving) => {...state, saving: saving}
  }

let handleInputChange = (callback, event) => {
  let value = ReactEvent.Form.target(event)["value"]
  callback(value)
}

@react.component
let make = (~name, ~about, ~updateDetailsCB) => {
  let (state, send) = React.useReducer(reducer, initialState(name, about))

  <div className="mx-8 pt-8">
    <h5 className="uppercase text-center border-b border-gray-400 pb-2">
      {"Update Details"->str}
    </h5>
    <DisablingCover disabled=state.saving>
      <div className="mt-3">
        <label
          className="inline-block tracking-wide text-xs font-semibold"
          htmlFor="details-editor__name">
          {"School Name"->str}
        </label>
        <input
          type_="text"
          maxLength=50
          placeholder="Type school name here"
          className="appearance-none block w-full bg-white text-gray-800 border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
          id="details-editor__name"
          onChange={handleInputChange(name => send(UpdateName(name)))}
          value=state.name
        />
        <School__InputGroupError
          message="name should be greater than 2 characters in length"
          active={state.name->String.length < 2}
        />
      </div>
      <div className="mt-3">
        <label
          className="inline-block tracking-wide text-xs font-semibold"
          htmlFor="details-editor__about">
          {"About"->str}
          <span className="font-normal"> {" (Maximum 500 characters)"->str} </span>
        </label>
        <textarea
          maxLength=500
          rows=7
          placeholder="Add more details about the school."
          className="appearance-none block w-full bg-white text-gray-800 border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
          id="details-editor__about"
          onChange={handleInputChange(about => send(UpdateAbout(about)))}
          value=state.about
        />
      </div>
      <button
        key="details-editor__update-button"
        onClick={_ => updateSchoolQuery(state, send, updateDetailsCB)}
        disabled={updateButtonDisabled(state)}
        className="w-full bg-indigo-600 hover:bg-blue-600 text-white font-bold py-3 px-6 rounded focus:outline-none mt-3">
        {updateButtonText(state.saving)->str}
      </button>
    </DisablingCover>
  </div>
}
