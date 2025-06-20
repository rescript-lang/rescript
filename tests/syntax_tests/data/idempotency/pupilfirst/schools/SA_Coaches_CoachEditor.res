open CoachesSchoolIndex__Types

exception UnexpectedResponse(int)

let handleApiError = x =>
  switch x {
  | UnexpectedResponse(code) => code
  }

type state = {
  name: string,
  email: string,
  title: string,
  linkedinUrl: string,
  public: bool,
  exited: bool,
  connectLink: string,
  imageFileName: string,
  dirty: bool,
  saving: bool,
  hasNameError: bool,
  hasTitleError: bool,
  hasEmailError: bool,
  hasLinkedInUrlError: bool,
  hasConnectLinkError: bool,
  affiliation: string,
}

type action =
  | UpdateName(string, bool)
  | UpdateEmail(string, bool)
  | UpdateTitle(string, bool)
  | UpdateLinkedInUrl(string, bool)
  | UpdateConnectLink(string, bool)
  | UpdatePublic(bool)
  | UpdateImageFileName(string)
  | UpdateExited(bool)
  | UpdateAffiliation(string)
  | UpdateSaving

let reducer = (state, action) =>
  switch action {
  | UpdateName(name, hasNameError) => {
      ...state,
      name: name,
      hasNameError: hasNameError,
      dirty: true,
    }
  | UpdateTitle(title, hasTitleError) => {
      ...state,
      title: title,
      hasTitleError: hasTitleError,
      dirty: true,
    }
  | UpdateEmail(email, hasEmailError) => {
      ...state,
      email: email,
      hasEmailError: hasEmailError,
      dirty: true,
    }
  | UpdateLinkedInUrl(linkedinUrl, hasLinkedInUrlError) => {
      ...state,
      linkedinUrl: linkedinUrl,
      hasLinkedInUrlError: hasLinkedInUrlError,
      dirty: true,
    }
  | UpdateConnectLink(connectLink, hasConnectLinkError) => {
      ...state,
      connectLink: connectLink,
      hasConnectLinkError: hasConnectLinkError,
      dirty: true,
    }
  | UpdatePublic(public) => {...state, public: public, dirty: true}
  | UpdateSaving => {...state, saving: !state.saving}
  | UpdateImageFileName(imageFileName) => {
      ...state,
      imageFileName: imageFileName,
      dirty: true,
    }
  | UpdateExited(exited) => {...state, exited: exited, dirty: true}
  | UpdateAffiliation(affiliation) => {...state, affiliation: affiliation, dirty: true}
  }

let str = React.string

let nameOrTitleInvalid = name => name->String.length < 2

let updateName = (send, name) => send(UpdateName(name, name->nameOrTitleInvalid))

let emailInvalid = email => email->EmailUtils.isInvalid(false)

let updateEmail = (send, email) => send(UpdateEmail(email, email->emailInvalid))

let updateTitle = (send, title) => send(UpdateTitle(title, title->nameOrTitleInvalid))

let updateLinkedInUrl = (send, linkedinUrl) => {
  let regex = /(https?)?:?(\/\/)?(([w]{3}||\w\w)\.)?linkedin.com(\w+:{0,1}\w*@)?(\S+)(:([0-9])+)?(\/|\/([\w#!:.?+=&%@!\-\/]))?/
  let hasError = linkedinUrl->String.length < 1 ? false : !Js.Re.test_(regex, linkedinUrl)
  send(UpdateLinkedInUrl(linkedinUrl, hasError))
}

let updateConnectLink = (send, connectLink) =>
  send(UpdateConnectLink(connectLink, connectLink->UrlUtils.isInvalid(true)))

let booleanButtonClasses = selected => {
  let classes = "toggle-button__button"
  classes ++ (selected ? " toggle-button__button--active" : "")
}

let saveDisabled = state =>
  state.title->nameOrTitleInvalid ||
    (state.name->nameOrTitleInvalid ||
    (state.email->emailInvalid ||
      (state.hasLinkedInUrlError ||
      (state.connectLink->UrlUtils.isInvalid(true) || (!state.dirty || state.saving)))))

let computeInitialState = coach =>
  switch coach {
  | None => {
      name: "",
      email: "",
      title: "",
      linkedinUrl: "",
      public: false,
      connectLink: "",
      exited: false,
      dirty: false,
      saving: false,
      hasNameError: false,
      hasEmailError: false,
      hasTitleError: false,
      hasLinkedInUrlError: false,
      hasConnectLinkError: false,
      imageFileName: "",
      affiliation: "",
    }
  | Some(coach) => {
      name: coach->Coach.name,
      email: coach->Coach.email,
      title: coach->Coach.title,
      linkedinUrl: switch coach->Coach.linkedinUrl {
      | Some(linkedinUrl) => linkedinUrl
      | None => ""
      },
      public: coach->Coach.public,
      connectLink: switch coach->Coach.connectLink {
      | Some(connectLink) => connectLink
      | None => ""
      },
      exited: coach->Coach.exited,
      dirty: false,
      saving: false,
      hasNameError: false,
      hasEmailError: false,
      hasTitleError: false,
      hasLinkedInUrlError: false,
      hasConnectLinkError: false,
      imageFileName: switch coach->Coach.imageFileName {
      | Some(imageFileName) => imageFileName
      | None => ""
      },
      affiliation: coach->Coach.affiliation->OptionUtils.toString,
    }
  }

@react.component
let make = (~coach, ~closeFormCB, ~updateCoachCB, ~authenticityToken) => {
  let (state, send) = React.useReducerWithMapState(reducer, coach, computeInitialState)

  let formId = "coach-create-form"
  let addCoach = json => {
    let id = json->{
      open Json.Decode
      field("id", int)
    }
    let imageUrl = json->{
      open Json.Decode
      field("image_url", string)
    }
    let newCoach = Coach.make(
      ~id,
      ~name=state.name,
      ~imageUrl,
      ~email=state.email,
      ~title=state.title,
      ~linkedinUrl=Some(state.linkedinUrl),
      ~public=state.public,
      ~connectLink=Some(state.connectLink),
      ~exited=state.exited,
      ~imageFileName=Some(state.imageFileName),
      ~affiliation=Some(state.affiliation),
    )
    switch coach {
    | Some(_) => Notification.success("Success", "Coach updated successfully")
    | None => Notification.success("Success", "Coach created successfully")
    }
    updateCoachCB(newCoach)
    closeFormCB()
  }
  let avatarUploaderText = () =>
    switch state.imageFileName {
    | "" => "Upload an avatar"
    | _ => "Replace avatar: " ++ state.imageFileName
    }
  let handleResponseJSON = json => {
    let error =
      json
      ->{
        open Json.Decode
        field("error", nullable(string))
      }
      ->Js.Null.toOption
    switch error {
    | Some(err) =>
      send(UpdateSaving)
      Notification.error("Something went wrong!", err)
    | None => addCoach(json)
    }
  }
  let sendCoach = formData => {
    let endPoint = switch coach {
    | Some(coach) => "/school/coaches/" ++ (coach->Coach.id->string_of_int)
    | None => "/school/coaches/"
    }
    let httpMethod = switch coach {
    | Some(_coach) => Fetch.Patch
    | None => Fetch.Post
    }
    open Js.Promise
    Fetch.fetchWithInit(
      endPoint,
      Fetch.RequestInit.make(
        ~method_=httpMethod,
        ~body=Fetch.BodyInit.makeWithFormData(formData),
        ~credentials=Fetch.SameOrigin,
        (),
      ),
    )
    ->then_(response =>
      if Fetch.Response.ok(response) || Fetch.Response.status(response) == 422 {
        response->Fetch.Response.json
      } else {
        Js.Promise.reject(UnexpectedResponse(response->Fetch.Response.status))
      }
    )
    ->then_(json => handleResponseJSON(json)->resolve)
    ->catch(error =>
      switch error->handleApiError {
      | Some(code) =>
        send(UpdateSaving)
        Notification.error(code->string_of_int, "Please try again")
      | None =>
        send(UpdateSaving)
        Notification.error("Something went wrong!", "Please try again")
      }->resolve
    )
    ->ignore
  }
  let submitForm = event => {
    ReactEvent.Form.preventDefault(event)
    send(UpdateSaving)
    let element = ReactDOMRe._getElementById(formId)
    switch element {
    | Some(element) => sendCoach(DomUtils.FormData.create(element))
    | None => ()
    }
  }
  <div className="blanket">
    <div className="drawer-right">
      <div className="drawer-right__close absolute">
        <button
          title="close"
          onClick={_e => closeFormCB()}
          className="flex items-center justify-center bg-white text-gray-600 font-bold py-3 px-5 rounded-l-full rounded-r-none hover:text-gray-700 focus:outline-none mt-4">
          <i className="fas fa-times text-xl" />
        </button>
      </div>
      <div className="drawer-right-form w-full">
        <div className="w-full">
          <div className="mx-auto bg-white">
            <div className="max-w-2xl px-6 pt-5 mx-auto">
              <h5 className="uppercase text-center border-b border-gray-400 pb-2">
                {switch coach {
                | Some(coach) => coach->Coach.name
                | None => "Add New Coach"
                }->str}
              </h5>
            </div>
            <form key="xxx" id=formId onSubmit={event => submitForm(event)}>
              <input name="authenticity_token" type_="hidden" value=authenticityToken />
              <div className="max-w-2xl px-6 pb-6 mx-auto">
                <div className="mt-5">
                  <label
                    className="inline-block tracking-wide text-gray-900 text-xs font-semibold"
                    htmlFor="name">
                    {"Name"->str}
                  </label>
                  <span> {"*"->str} </span>
                  <input
                    className="appearance-none block w-full bg-white text-gray-800 border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                    id="name"
                    type_="text"
                    name="faculty[name]"
                    placeholder="Coach Name"
                    value=state.name
                    onChange={event => updateName(send, ReactEvent.Form.target(event)["value"])}
                  />
                  <School__InputGroupError
                    message="is not a valid name" active=state.hasNameError
                  />
                </div>
                {switch coach {
                | Some(_coach) => React.null
                | None =>
                  <div className="mt-5">
                    <label
                      className="inline-block tracking-wide text-xs font-semibold" htmlFor="email">
                      {"Email"->str}
                    </label>
                    <span> {"*"->str} </span>
                    <input
                      className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                      id="email"
                      type_="email"
                      name="faculty[email]"
                      placeholder="Coach email address"
                      value=state.email
                      onChange={event => updateEmail(send, ReactEvent.Form.target(event)["value"])}
                    />
                    <School__InputGroupError
                      message="is not a valid email address" active=state.hasEmailError
                    />
                  </div>
                }}
                <div className="mt-5">
                  <label
                    className="inline-block tracking-wide text-xs font-semibold" htmlFor="title">
                    {"Title"->str}
                  </label>
                  <span> {"*"->str} </span>
                  <input
                    className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                    id="title"
                    type_="text"
                    name="faculty[title]"
                    placeholder="Coach Title/Expertise"
                    value=state.title
                    onChange={event => updateTitle(send, ReactEvent.Form.target(event)["value"])}
                  />
                  <School__InputGroupError
                    message="is not a valid title" active=state.hasTitleError
                  />
                </div>
                <div className="mt-5">
                  <label
                    className="inline-block tracking-wide text-xs font-semibold"
                    htmlFor="affiliation">
                    {"Affiliation"->str}
                  </label>
                  <input
                    value=state.affiliation
                    onChange={event =>
                      send(UpdateAffiliation(ReactEvent.Form.target(event)["value"]))}
                    className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                    id="affiliation"
                    name="faculty[affiliation]"
                    type_="text"
                    placeholder="Acme Inc., Acme University, etc."
                  />
                </div>
                <div className="mt-5">
                  <label
                    className="inline-block tracking-wide text-xs font-semibold" htmlFor="linkedIn">
                    {"LinkedIn"->str}
                  </label>
                  <input
                    className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                    id="linkedIn"
                    type_="text"
                    name="faculty[linkedin_url]"
                    placeholder="LinkedIn Profile URL"
                    value=state.linkedinUrl
                    onChange={event =>
                      updateLinkedInUrl(send, ReactEvent.Form.target(event)["value"])}
                  />
                  <School__InputGroupError
                    message="is not a valid LinkedIn URL" active=state.hasLinkedInUrlError
                  />
                </div>
                <div className="mt-5">
                  <label
                    className="inline-block tracking-wide text-xs font-semibold"
                    htmlFor="connectLink">
                    {"Connect Link"->str}
                  </label>
                  <input
                    className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                    id="connectLink"
                    type_="text"
                    name="faculty[connect_link]"
                    placeholder="Student connect request link for the coach"
                    value=state.connectLink
                    onChange={event =>
                      updateConnectLink(send, ReactEvent.Form.target(event)["value"])}
                  />
                  <School__InputGroupError
                    message="is not a valid connect url" active=state.hasConnectLinkError
                  />
                </div>
                <div className="mt-5">
                  <div className="flex items-center flex-shrink-0">
                    <label
                      className="block tracking-wide text-xs font-semibold mr-3"
                      htmlFor="evaluated">
                      {"Should the coach profile be public?"->str}
                    </label>
                    <div
                      id="notification"
                      className="flex flex-shrink-0 rounded-lg overflow-hidden border border-gray-400">
                      <button
                        type_="submit"
                        onClick={_event => {
                          ReactEvent.Mouse.preventDefault(_event)
                          send(UpdatePublic(true))
                        }}
                        name="faculty[public]"
                        value="true"
                        className={booleanButtonClasses(state.public)}>
                        {"Yes"->str}
                      </button>
                      <button
                        onClick={_event => {
                          ReactEvent.Mouse.preventDefault(_event)
                          send(UpdatePublic(false))
                        }}
                        className={booleanButtonClasses(!state.public)}>
                        {"No"->str}
                      </button>
                      <input
                        type_="hidden" name="faculty[public]" value={state.public->string_of_bool}
                      />
                    </div>
                  </div>
                </div>
                <div className="mt-5">
                  <label
                    className="block tracking-wide text-xs font-semibold" htmlFor="avatarUploader">
                    {"Avatar"->str}
                  </label>
                  <input
                    disabled=state.saving
                    className="hidden"
                    name="faculty[image]"
                    type_="file"
                    id="sa-coach-editor__file-input"
                    required=false
                    multiple=false
                    onChange={event =>
                      send(UpdateImageFileName(ReactEvent.Form.target(event)["files"][0]["name"]))}
                  />
                  <label className="file-input-label mt-2" htmlFor="sa-coach-editor__file-input">
                    <i className="fas fa-upload mr-2 text-gray-600 text-lg" />
                    <span className="truncate"> {avatarUploaderText()->str} </span>
                  </label>
                </div>
              </div>
              <div className="p-6 bg-gray-200">
                <div className="max-w-2xl px-6 mx-auto">
                  <div className="flex max-w-2xl w-full justify-between items-center mx-auto">
                    {switch coach {
                    | Some(_coach) =>
                      <div className="flex items-center flex-shrink-0">
                        <label
                          className="block tracking-wide  text-xs font-semibold mr-3"
                          htmlFor="evaluated">
                          {"Has the coach left the school?"->str}
                        </label>
                        <div
                          id="exited"
                          className="flex flex-shrink-0 rounded-lg overflow-hidden border border-gray-400">
                          <button
                            onClick={_event => {
                              ReactEvent.Mouse.preventDefault(_event)
                              send(UpdateExited(true))
                            }}
                            name="faculty[exited]"
                            className={booleanButtonClasses(state.exited)}>
                            {"Yes"->str}
                          </button>
                          <button
                            onClick={_event => {
                              ReactEvent.Mouse.preventDefault(_event)
                              send(UpdateExited(false))
                            }}
                            className={booleanButtonClasses(!state.exited)}>
                            {"No"->str}
                          </button>
                          <input
                            type_="hidden"
                            name="faculty[exited]"
                            value={state.exited->string_of_bool}
                          />
                        </div>
                      </div>
                    | None => React.null
                    }}
                    <button
                      disabled={saveDisabled(state)} className="w-auto btn btn-large btn-primary">
                      {switch coach {
                      | Some(_coach) => "Update Coach"
                      | None => "Add Coach"
                      }->str}
                    </button>
                  </div>
                </div>
              </div>
            </form>
          </div>
        </div>
      </div>
    </div>
  </div>
}
