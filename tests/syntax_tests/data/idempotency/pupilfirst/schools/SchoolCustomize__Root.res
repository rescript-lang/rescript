open SchoolCustomize__Types

%raw(`require("./SchoolCustomize__Root.css")`)

let str = ReasonReact.string

type editor =
  | LinksEditor(SchoolCustomize__LinksEditor.kind)
  | DetailsEditor
  | ImagesEditor
  | ContactsEditor
  | AgreementsEditor(SchoolCustomize__AgreementsEditor.kind)

type state = {
  visibleEditor: option<editor>,
  customizations: Customizations.t,
  schoolName: string,
  schoolAbout: option<string>,
}

type rec action =
  | ShowEditor(editor)
  | CloseEditor
  | AddLink(Customizations.link)
  | RemoveLink(Customizations.linkId)
  | UpdateTermsOfUse(string)
  | UpdatePrivacyPolicy(string)
  | UpdateAddress(string)
  | UpdateEmailAddress(string)
  | UpdateSchoolDetails(name, about)
  | UpdateImages(Js.Json.t)
and name = string
and about = option<string>

let headerLogo = (schoolName, logoOnLightBg) =>
  switch logoOnLightBg {
  | Some(logo) =>
    <div className="max-w-xs"> <img className="h-12" src={logo->Customizations.url} /> </div>
  | None => <span className="text-2xl font-bold"> {schoolName->str} </span>
  }

let headerLink = ((id, title, _)) =>
  <div className="ml-6 text-sm font-semibold cursor-default" key=id>
    <span> {title->str} </span>
  </div>

let headerLinks = links => {
  let (visibleLinks, dropdownLinks) = switch links {
  | list{l1, l2, l3, l4, l5, ...rest} => (list{l1, l2, l3}, list{l4, l5, ...rest})
  | fourOrLessLinks => (fourOrLessLinks, list{})
  }

  switch visibleLinks {
  | list{} =>
    <div
      className="border border-gray-400 rounded-lg italic text-gray-600 cursor-default text-sm py-2 px-4">
      {"You can customize links on the header."->str}
    </div>
  | visibleLinks =>
    (visibleLinks->List.map(l => headerLink(l)))
      ->List.append(list{<SchoolCustomize__MoreLinks links=dropdownLinks key="more-links" />})
    ->Array.of_list
    ->ReasonReact.array
  }
}

let sitemap = links =>
  switch links {
  | list{} =>
    <div
      className="border border-gray-500 rounded-lg italic text-gray-400 cursor-default text-sm max-w-fc mt-3 py-2 px-4">
      {"You can customize links in the footer."->str}
    </div>
  | links =>
    <div className="flex flex-wrap">
      {links
      ->List.map(((id, title, _)) =>
        <div className="w-1/3 pr-4 mt-3 text-xs font-semibold" key=id> {title->str} </div>
      )
      ->Array.of_list
      ->ReasonReact.array}
    </div>
  }

let socialLinks = links =>
  switch links {
  | list{} =>
    <div
      className="border border-gray-500 rounded-lg italic text-gray-400 cursor-default text-sm max-w-fc mt-3 py-2 px-4">
      {"Add social media links?"->str}
    </div>
  | links =>
    <div className="flex flex-wrap">
      {links
      ->List.map(((id, _title, url)) => <SchoolCustomize__SocialLink url key=id />)
      ->Array.of_list
      ->ReasonReact.array}
    </div>
  }

let address = a =>
  switch a {
  | Some(a) =>
    <div
      className="text-xs font-semibold mt-3 leading-normal"
      dangerouslySetInnerHTML={
        "__html": a->Markdown.parse(Markdown.Permissive),
      }
    />
  | None =>
    <div
      className="border border-gray-500 rounded-lg italic text-gray-400 cursor-default text-sm max-w-fc mt-3 py-2 px-4">
      {"Add an address?"->str}
    </div>
  }

let emailAddress = email =>
  switch email {
  | Some(email) =>
    <div className="text-xs font-semibold mt-4">
      {"React us at "->str} <span className="font-bold"> {email->str} </span>
    </div>
  | None =>
    <div
      className="border border-gray-500 rounded-lg italic text-gray-400 cursor-default text-sm max-w-fc mt-4 py-2 px-4">
      {"Add a contact email?"->str}
    </div>
  }

let footerLogo = (schoolName, logoOnDarkBg) =>
  switch logoOnDarkBg {
  | Some(logo) => <img className="h-8" src={logo->Customizations.url} />
  | None => <span className="text-lg font-bold"> {schoolName->str} </span>
  }

let editIcon = (additionalClasses, clickHandler, title) =>
  <div
    className={"cursor-pointer bg-primary-100 border border-primary-400 text-primary-500 hover:bg-primary-200 hover:border-primary-500 hover:text-primary-600 px-2 py-1 rounded flex items-center " ++
    additionalClasses}
    title
    onClick=clickHandler>
    <i className="fas fa-pencil-alt text-xs" />
    <span className="text-xs font-semibold ml-2"> {"Edit"->str} </span>
  </div>

let showEditor = (editor, send, event) => {
  event->ReactEvent.Mouse.preventDefault
  send(ShowEditor(editor))
}

let editor = (state, send, authenticityToken) =>
  switch state.visibleEditor {
  | Some(editor) =>
    <SchoolAdmin__EditorDrawer closeDrawerCB={() => send(CloseEditor)}>
      {switch editor {
      | LinksEditor(kind) =>
        <SchoolCustomize__LinksEditor
          key="sc-drawer__links-editor"
          kind
          customizations=state.customizations
          addLinkCB={link => send(AddLink(link))}
          removeLinkCB={linkId => send(RemoveLink(linkId))}
        />
      | AgreementsEditor(kind) =>
        <SchoolCustomize__AgreementsEditor
          key="sc-drawer__agreements-editor"
          kind
          customizations=state.customizations
          updatePrivacyPolicyCB={agreement => send(UpdatePrivacyPolicy(agreement))}
          updateTermsOfUseCB={agreement => send(UpdateTermsOfUse(agreement))}
        />
      | ContactsEditor =>
        <SchoolCustomize__ContactsEditor
          key="sc-drawer__contacts-editor"
          customizations=state.customizations
          updateAddressCB={address => send(UpdateAddress(address))}
          updateEmailAddressCB={emailAddress => send(UpdateEmailAddress(emailAddress))}
        />
      | ImagesEditor =>
        <SchoolCustomize__ImagesEditor
          key="sc-drawer__images-editor"
          customizations=state.customizations
          updateImagesCB={json => send(UpdateImages(json))}
          authenticityToken
        />
      | DetailsEditor =>
        <SchoolCustomize__DetailsEditor
          name=state.schoolName
          about=state.schoolAbout
          updateDetailsCB={(name, about) => send(UpdateSchoolDetails(name, about))}
        />
      }}
    </SchoolAdmin__EditorDrawer>

  | None => ReasonReact.null
  }

let initialState = (customizations, schoolName, schoolAbout) => {
  visibleEditor: None,
  customizations: customizations,
  schoolName: schoolName,
  schoolAbout: schoolAbout,
}

let reducer = (state, action) =>
  switch action {
  | ShowEditor(editor) => {...state, visibleEditor: Some(editor)}
  | CloseEditor => {...state, visibleEditor: None}
  | AddLink(link) => {
      ...state,
      customizations: state.customizations->Customizations.addLink(link),
    }
  | RemoveLink(linkId) => {
      ...state,
      customizations: state.customizations->Customizations.removeLink(linkId),
    }
  | UpdatePrivacyPolicy(agreement) => {
      ...state,
      customizations: state.customizations->Customizations.updatePrivacyPolicy(agreement),
    }
  | UpdateTermsOfUse(agreement) => {
      ...state,
      customizations: state.customizations->Customizations.updateTermsOfUse(agreement),
    }
  | UpdateAddress(address) => {
      ...state,
      customizations: state.customizations->Customizations.updateAddress(address),
    }
  | UpdateEmailAddress(emailAddress) => {
      ...state,
      customizations: state.customizations->Customizations.updateEmailAddress(emailAddress),
    }
  | UpdateImages(json) => {
      ...state,
      customizations: state.customizations->Customizations.updateImages(json),
      visibleEditor: None,
    }
  | UpdateSchoolDetails(schoolName, schoolAbout) => {
      ...state,
      schoolName: schoolName,
      schoolAbout: schoolAbout,
      visibleEditor: None,
    }
  }

let about = state =>
  switch state.schoolAbout {
  | Some(about) => about
  | None => "Add more details about the school."
  }

@react.component
let make = (~authenticityToken, ~customizations, ~schoolName, ~schoolAbout) => {
  let (state, send) = React.useReducer(
    reducer,
    initialState(customizations, schoolName, schoolAbout),
  )
  <div>
    <div className="px-6 py-6 w-full xl:max-w-6xl mx-auto">
      <div className="font-bold"> {"Home Page"->str} </div>
      <div className="border rounded-t-lg px-5 py-4 flex justify-between mt-3">
        <div className="flex items-center bg-gray-200 rounded p-2">
          {headerLogo(schoolName, state.customizations->Customizations.logoOnLightBg)}
          {editIcon("ml-6", showEditor(ImagesEditor, send), "Edit logo (on light backgrounds)")}
        </div>
        <div className="flex items-center">
          <div
            className="school-customize__header-links flex items-center bg-gray-200 rounded px-3 py-2 h-full">
            {headerLinks(
              state.customizations->Customizations.headerLinks->Customizations.unpackLinks,
            )}
            {editIcon(
              "ml-3",
              showEditor(LinksEditor(SchoolCustomize__LinksEditor.HeaderLink), send),
              "Edit header links",
            )}
          </div>
        </div>
      </div>
      <div className="relative bg-gray-300">
        <div className="absolute right-0 z-10 pt-3 pr-3">
          <button
            className="flex items-center text-xs bg-primary-100 text-primary-500 border border-primary-400 hover:bg-primary-200 hover:border-primary-500 hover:text-primary-600 px-2 py-1 cursor-pointer rounded"
            onClick={showEditor(ImagesEditor, send)}>
            <i className="fas fa-pencil-alt" />
            <span className="font-semibold ml-2"> {"Change cover"->str} </span>
          </button>
        </div>
        <div className="relative pb-1/2 md:pb-1/4 rounded-b-lg overflow-hidden">
          {switch state.customizations->Customizations.coverImage {
          | Some(image) =>
            <img
              className="absolute h-full w-full object-cover" src={image->Customizations.url}
            />
          | None =>
            <div
              className="school-customize__cover-default absolute h-full w-full svg-bg-pattern-6"
            />
          }}
        </div>
      </div>
      <div className="max-w-3xl relative mx-auto bg-primary-900 shadow-xl rounded-lg -mt-7">
        <div
          className="relative mx-auto flex flex-col justify-center items-center text-white p-10 text-center">
          <p> {"Hello, welcome to"->str} </p>
          <div onClick={showEditor(DetailsEditor, send)}>
            <h1
              className="flex items-center border border-dashed border-gray-800 hover:border-primary-300 hover:text-primary-200 cursor-text rounded px-2 py-1 text-3xl mt-1">
              <span> {state.schoolName->str} </span>
              <button
                className="flex items-center text-xs bg-primary-100 text-primary-500 border border-primary-400 hover:bg-primary-200 hover:border-primary-500 hover:text-primary-600 p-1 ml-1 cursor-pointer rounded"
                onClick={showEditor(DetailsEditor, send)}>
                <i className="fas fa-pencil-alt" />
              </button>
            </h1>
          </div>
          <div
            ariaLabel="Edit school details"
            onClick={showEditor(DetailsEditor, send)}
            className="w-full max-w-2xl mt-2 relative flex items-center justify-center border border-dashed border-gray-800 rounded px-8 py-5 hover:border-primary-300 hover:text-primary-200 cursor-text">
            <div className="absolute right-0 top-0 z-10 pt-2 pr-2">
              <button
                className="flex items-center text-xs bg-primary-100 text-primary-500 border border-primary-400 hover:bg-primary-200 hover:border-primary-500 hover:text-primary-600 p-1 cursor-pointer rounded">
                <i className="fas fa-pencil-alt" />
              </button>
            </div>
            <div className="text-sm">
              <MarkdownBlock profile=Markdown.AreaOfText markdown={about(state)} />
            </div>
          </div>
        </div>
      </div>
      <div className="mx-auto text-center pt-8 mt-8">
        <h2 className="school-customize__featured-courses-header relative text-2xl font-bold">
          {"Featured Courses"->str}
        </h2>
        <div className="text-sm"> {"Featured courses will be listed here"->str} </div>
        <div className="max-w-2xl bg-gray-100 rounded-lg mx-auto p-3 mt-4">
          <div className="school-customize__featured-courses-empty-placeholder" />
        </div>
      </div>
      <div className="mt-8 w-full">
        <div className="school-customize__footer-top-container rounded-t-lg p-6 flex">
          <div className="w-2/5">
            <div
              className="p-3 bg-gray-300 border border-dashed border-gray-500 rounded h-full mr-2">
              <div className="flex items-center">
                <span className="uppercase font-bold text-sm"> {"Sitemap"->str} </span>
                {editIcon(
                  "ml-3",
                  showEditor(LinksEditor(SchoolCustomize__LinksEditor.FooterLink), send),
                  "Edit footer links",
                )}
              </div>
              {sitemap(
                state.customizations->Customizations.footerLinks->Customizations.unpackLinks,
              )}
            </div>
          </div>
          <div className="w-3/5">
            <div className="flex">
              <div className="w-3/5">
                <div
                  className="p-3 bg-gray-300 border border-dashed border-gray-500 rounded h-full mr-2">
                  <div className="flex items-center">
                    <span className="uppercase font-bold text-sm"> {"Social"->str} </span>
                    {editIcon(
                      "ml-3",
                      showEditor(LinksEditor(SchoolCustomize__LinksEditor.SocialLink), send),
                      "Edit social media links",
                    )}
                  </div>
                  {socialLinks(
                    state.customizations
                    ->Customizations.socialLinks
                    ->Customizations.unpackLinks,
                  )}
                </div>
              </div>
              <div className="w-2/5">
                <div
                  className="p-3 bg-gray-300 border border-dashed border-gray-500 rounded h-full">
                  <div className="flex items-center">
                    <span className="uppercase font-bold text-sm"> {"Contact"->str} </span>
                    {editIcon("ml-3", showEditor(ContactsEditor, send), "Edit contact details")}
                  </div>
                  {address(state.customizations->Customizations.address)}
                  {emailAddress(state.customizations->Customizations.emailAddress)}
                </div>
              </div>
            </div>
          </div>
        </div>
        <div
          className="school-customize__footer-bottom-container rounded-b-lg p-6 flex justify-between">
          <div
            className="flex items-center bg-gray-300 border border-dashed border-gray-500 rounded p-2">
            {footerLogo(schoolName, state.customizations->Customizations.logoOnLightBg)}
            {editIcon("ml-3", showEditor(ImagesEditor, send), "Edit logo (on dark backgrounds)")}
          </div>
          <div className="flex items-center text-sm">
            <div
              className="flex items-center bg-gray-300 border border-dashed border-gray-500 rounded p-2 text-xs">
              <div> {"Privacy Policy"->str} </div>
              {editIcon(
                "ml-3",
                showEditor(AgreementsEditor(SchoolCustomize__AgreementsEditor.PrivacyPolicy), send),
                "Edit privacy policy",
              )}
            </div>
            <div
              className="flex items-center bg-gray-300 border border-dashed border-gray-500 rounded p-2 ml-6 text-xs">
              <div> {"Terms of Use"->str} </div>
              {editIcon(
                "ml-3",
                showEditor(AgreementsEditor(SchoolCustomize__AgreementsEditor.TermsOfUse), send),
                "Edit terms of use",
              )}
            </div>
            <div className="ml-6 flex items-center text-xs text-gray-600">
              <i className="far fa-copyright" />
              <span className="ml-1">
                {(Js.Date.make()->Js.Date.getFullYear->int_of_float->string_of_int) ++
                  (" " ++
                  schoolName)->str}
              </span>
            </div>
          </div>
        </div>
      </div>
      <div className="mt-6 font-bold"> {"Icon"->str} </div>
      <div className="mt-3 w-2/4 max-w-sm">
        <div className="bg-gray-400 rounded-t-lg h-12 flex items-end">
          <div className="w-full flex items-center pr-3">
            <div className="h-3 w-3 rounded-full bg-gray-500 ml-4" />
            <div className="h-3 w-3 rounded-full bg-gray-500 ml-2" />
            <div className="h-3 w-3 rounded-full bg-gray-500 ml-2" />
            <div className="p-3 ml-4 bg-gray-100 rounded-t-lg flex items-center">
              <img
                src={state.customizations->Customizations.icon->Customizations.url}
                className="h-5 w-5"
              />
              <span className="ml-1 text-xs font-semibold max-w-xs truncate">
                {schoolName->str}
              </span>
            </div>
            {editIcon("ml-2", showEditor(ImagesEditor, send), "Edit icon")}
          </div>
        </div>
        <div className="bg-gray-100 border border-t-0 h-16 rounded-b-lg" />
      </div>
    </div>
    {editor(state, send, authenticityToken)}
  </div>
}
