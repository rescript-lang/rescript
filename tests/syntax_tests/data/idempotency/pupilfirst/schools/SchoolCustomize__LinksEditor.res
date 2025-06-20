open SchoolCustomize__Types

%raw(`require("./SchoolCustomize__LinksEditor.css")`)

let str = ReasonReact.string

type kind =
  | HeaderLink
  | FooterLink
  | SocialLink

type state = {
  kind: kind,
  title: string,
  url: string,
  titleInvalid: bool,
  urlInvalid: bool,
  formDirty: bool,
  adding: bool,
  deleting: list<Customizations.linkId>,
}

type action =
  | UpdateKind(kind)
  | UpdateTitle(string, bool)
  | UpdateUrl(string, bool)
  | DisableForm
  | EnableForm
  | ClearForm
  | DisableDelete(Customizations.linkId)

let handleKindChange = (send, kind, event) => {
  event->ReactEvent.Mouse.preventDefault
  send(UpdateKind(kind))
}

let isTitleInvalid = title => title->String.trim->String.length == 0

let handleTitleChange = (send, event) => {
  let title = ReactEvent.Form.target(event)["value"]
  send(UpdateTitle(title, isTitleInvalid(title)))
}

let handleUrlChange = (send, event) => {
  let url = ReactEvent.Form.target(event)["value"]
  send(UpdateUrl(url, url->UrlUtils.isInvalid(false)))
}

module DestroySchoolLinkQuery = %graphql(`
  mutation DestroySchoolLinkMutation($id: ID!) {
    destroySchoolLink(id: $id) {
      success
    }
  }
  `)

let handleDelete = (state, send, removeLinkCB, id, event) => {
  event->ReactEvent.Mouse.preventDefault

  if state.deleting->List.mem(id) {
    ()
  } else {
    send(DisableDelete(id))

    DestroySchoolLinkQuery.make(~id, ())
    ->GraphqlQuery.sendQuery
    ->Js.Promise.then_(_response => {
      removeLinkCB(id)
      Js.Promise.resolve()
    })
    ->ignore
  }
}

let deleteIconClasses = deleting => deleting ? "fas fa-spinner fa-pulse" : "far fa-trash-alt"

let showLinks = (state, send, removeLinkCB, kind, links) =>
  switch links {
  | list{} =>
    <div
      className="border border-gray-400 rounded italic text-gray-600 text-xs cursor-default mt-2 p-3">
      {"There are no custom links here. Add some?"->str}
    </div>
  | links =>
    links
    ->List.map(((id, title, url)) =>
      <div
        className="flex items-center justify-between bg-gray-100 text-xs text-gray-900 border rounded pl-3 mt-2"
        key=id>
        <div className="flex items-center">
          {switch kind {
          | HeaderLink
          | FooterLink =>
            [
              <span key="link-editor-entry__title"> {title->str} </span>,
              <i key="link-editor-entry__icon" className="fas fa-link mx-2" />,
              <code key="link-editor-entry__url"> {url->str} </code>,
            ]->ReasonReact.array
          | SocialLink => <code> {url->str} </code>
          }}
        </div>
        <button
          title={"Delete " ++ url}
          onClick={handleDelete(state, send, removeLinkCB, id)}
          className="p-3">
          <FaIcon classes={deleteIconClasses(state.deleting->List.mem(id))} />
        </button>
      </div>
    )
    ->Array.of_list
    ->ReasonReact.array
  }

let titleInputVisible = state =>
  switch state.kind {
  | HeaderLink
  | FooterLink => true
  | SocialLink => false
  }

let kindClasses = selected => {
  let classes = "nav-tab-item border-t cursor-pointer w-1/3 appearance-none flex justify-center items-center w-full text-sm text-center text-gray-800 bg-white hover:bg-gray-200 hover:text-gray-900 py-3 px-4 font-semibold leading-tight focus:outline-none"
  classes ++ (
    selected
      ? " nav-tab-item--selected text-primary-500 bg-white hover:bg-white hover:text-primary-500"
      : " text-gray-600"
  )
}

let addLinkText = adding => adding ? "Adding new link..." : "Add a New Link"

let addLinkDisabled = state =>
  if state.adding {
    true
  } else if state.formDirty {
    switch state.kind {
    | HeaderLink
    | FooterLink =>
      isTitleInvalid(state.title) || state.url->UrlUtils.isInvalid(false)
    | SocialLink => state.url->UrlUtils.isInvalid(false)
    }
  } else {
    true
  }

module CreateSchoolLinkQuery = %graphql(`
  mutation CreateSchoolLinkMutation($kind: String!, $title: String, $url: String!) {
    createSchoolLink(kind: $kind, title: $title, url: $url) @bsVariant {
      schoolLink {
        id
      }
      errors
    }
  }
`)

let displayNewLink = (state, addLinkCB, id) =>
  switch state.kind {
  | HeaderLink => Customizations.HeaderLink(id, state.title, state.url)
  | FooterLink => Customizations.FooterLink(id, state.title, state.url)
  | SocialLink => Customizations.SocialLink(id, state.url)
  }->addLinkCB

module CreateLinkError = {
  type t = [#InvalidUrl | #InvalidLengthTitle | #InvalidKind | #BlankTitle]

  let notification = error =>
    switch error {
    | #InvalidUrl => (
        "Invalid URL",
        "It looks like the URL you've entered isn't valid. Please check, and try again.",
      )
    | #InvalidKind => ("InvalidKind", "")
    | #InvalidLengthTitle => ("InvalidLengthTitle", "")
    | #BlankTitle => ("BlankTitle", "")
    }
}

module CreateLinkErrorHandler = GraphqlErrorHandler.Make(CreateLinkError)

let handleAddLink = (state, send, addLinkCB, event) => {
  event->ReactEvent.Mouse.preventDefault

  if addLinkDisabled(state) {
    ()
  } else {
    send(DisableForm)
    switch state.kind {
    | HeaderLink =>
      CreateSchoolLinkQuery.make(~kind="header", ~title=state.title, ~url=state.url, ())
    | FooterLink =>
      CreateSchoolLinkQuery.make(~kind="footer", ~title=state.title, ~url=state.url, ())
    | SocialLink => CreateSchoolLinkQuery.make(~kind="social", ~url=state.url, ())
    }
    ->GraphqlQuery.sendQuery
    ->Js.Promise.then_(response =>
      switch response["createSchoolLink"] {
      | #SchoolLink(schoolLink) =>
        schoolLink["id"]->displayNewLink(state, addLinkCB)
        send(ClearForm)
        Notification.success("Done!", "A custom link has been added.")
        Js.Promise.resolve()
      | #Errors(errors) => Js.Promise.reject(CreateLinkErrorHandler.Errors(errors))
      }
    )
    ->CreateLinkErrorHandler.catch(() => send(EnableForm))
    ->ignore
  }
}

let linksTitle = kind =>
  switch kind {
  | HeaderLink => "Current Header Links"
  | FooterLink => "Current Sitemap Links"
  | SocialLink => "Current Social Media Links"
  }->str

let unpackLinks = (kind, customizations) =>
  customizations
  ->switch kind {
  | HeaderLink => Customizations.headerLinks
  | FooterLink => Customizations.footerLinks
  | SocialLink => Customizations.socialLinks
  }
  ->Customizations.unpackLinks

let initialState = kind => {
  kind: kind,
  title: "",
  url: "",
  titleInvalid: false,
  urlInvalid: false,
  formDirty: false,
  adding: false,
  deleting: list{},
}
let reducer = (state, action) =>
  switch action {
  | UpdateKind(kind) => {...state, kind: kind, formDirty: true}
  | UpdateTitle(title, invalid) => {
      ...state,
      title: title,
      titleInvalid: invalid,
      formDirty: true,
    }
  | UpdateUrl(url, invalid) => {
      ...state,
      url: url,
      urlInvalid: invalid,
      formDirty: true,
    }
  | DisableForm => {...state, adding: true}
  | EnableForm => {...state, adding: false}
  | ClearForm => {...state, adding: false, title: "", url: ""}
  | DisableDelete(linkId) => {
      ...state,
      deleting: list{linkId, ...state.deleting},
    }
  }

@react.component
let make = (~kind, ~customizations, ~addLinkCB, ~removeLinkCB) => {
  let (state, send) = React.useReducer(reducer, initialState(kind))

  <div className="mt-8 mx-8 pb-6">
    <h5 className="uppercase text-center border-b border-gray-400 pb-2">
      {"Manage custom links"->str}
    </h5>
    <div className="mt-3">
      <label className="inline-block tracking-wide text-xs font-semibold">
        {"Location of Link"->str}
      </label>
      <div className="flex bg-white border border-t-0 rounded-t mt-2">
        <div
          title="Show header links"
          className={kindClasses(state.kind == HeaderLink)}
          onClick={handleKindChange(send, HeaderLink)}>
          {"Header"->str}
        </div>
        <div
          title="Show footer links"
          className={kindClasses(state.kind == FooterLink) ++ " border-l"}
          onClick={handleKindChange(send, FooterLink)}>
          {"Footer Sitemap"->str}
        </div>
        <div
          title="Show social media links"
          className={kindClasses(state.kind == SocialLink) ++ " border-l"}
          onClick={handleKindChange(send, SocialLink)}>
          {"Social"->str}
        </div>
      </div>
    </div>
    <div className="p-5 border border-t-0 rounded-b">
      <label className="inline-block tracking-wide text-xs font-semibold mt-4">
        {linksTitle(state.kind)}
      </label>
      {showLinks(state, send, removeLinkCB, state.kind, unpackLinks(state.kind, customizations))}
      <DisablingCover disabled=state.adding>
        <div className="flex mt-3" key="sc-links-editor__form-body">
          {if state->titleInputVisible {
            <div className="flex-grow mr-4">
              <label
                className="inline-block tracking-wide text-xs font-semibold" htmlFor="link-title">
                {"Title"->str}
              </label>
              <input
                className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                id="link-title"
                type_="text"
                placeholder="A short title for a new link"
                onChange={handleTitleChange(send)}
                value=state.title
                maxLength=24
              />
              <School__InputGroupError message="can't be empty" active=state.titleInvalid />
            </div>
          } else {
            ReasonReact.null
          }}
          <div className="flex-grow">
            <label
              className="inline-block tracking-wide text-xs font-semibold" htmlFor="link-full-url">
              {"Full URL"->str}
            </label>
            <input
              className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
              id="link-full-url"
              type_="text"
              placeholder="Full URL, staring with https://"
              onChange={handleUrlChange(send)}
              value=state.url
            />
            <School__InputGroupError message="is not a valid URL" active=state.urlInvalid />
          </div>
        </div>
        <div className="flex justify-end">
          <button
            key="sc-links-editor__form-button"
            disabled={addLinkDisabled(state)}
            onClick={handleAddLink(state, send, addLinkCB)}
            className="btn btn-primary btn-large mt-6">
            {state.adding->addLinkText->str}
          </button>
        </div>
      </DisablingCover>
    </div>
  </div>
}
