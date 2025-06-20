open SchoolCustomize__Types

let str = ReasonReact.string

type action =
  | SelectLogoOnLightBgFile(string, bool)
  | SelectCoverImageFile(string, bool)
  | SelectIconFile(string, bool)
  | BeginUpdate
  | ErrorOccured
  | DoneUpdating

type state = {
  logoOnLightBgFilename: option<string>,
  logoOnLightBgInvalid: bool,
  coverImageFilename: option<string>,
  coverImageInvalid: bool,
  iconFilename: option<string>,
  iconInvalid: bool,
  updating: bool,
  formDirty: bool,
}

let updateButtonText = updating => updating ? "Updating..." : "Update Images"

let formId = "sc-images-editor__form"

let handleUpdateImages = (send, updateImagesCB, event) => {
  event->ReactEvent.Form.preventDefault
  send(BeginUpdate)

  let element = ReactDOMRe._getElementById(formId)
  switch element {
  | Some(element) =>
    Api.sendFormData(
      "/school/images",
      DomUtils.FormData.create(element),
      json => {
        Notification.success("Done!", "Images have been updated successfully.")
        updateImagesCB(json)
        send(DoneUpdating)
      },
      () => send(ErrorOccured),
    )
  | None => ()
  }
}

let updateButtonDisabled = state =>
  if state.updating {
    true
  } else {
    !state.formDirty || (state.logoOnLightBgInvalid || state.iconInvalid)
  }

let maxAllowedSize = 2 * 1024 * 1024

let isInvalidImageFile = image =>
  switch image["_type"] {
  | "image/jpeg"
  | "image/png" => false
  | _ => true
  } ||
  image["size"] > maxAllowedSize

let updateLogoOnLightBg = (send, event) => {
  let imageFile = ReactEvent.Form.target(event)["files"][0]
  send(SelectLogoOnLightBgFile(imageFile["name"], imageFile->isInvalidImageFile))
}

let updateCoverImage = (send, event) => {
  let imageFile = ReactEvent.Form.target(event)["files"][0]
  send(SelectCoverImageFile(imageFile["name"], imageFile->isInvalidImageFile))
}

let updateIcon = (send, event) => {
  let imageFile = ReactEvent.Form.target(event)["files"][0]
  send(SelectIconFile(imageFile["name"], imageFile->isInvalidImageFile))
}

let imageUploader = (
  ~id,
  ~disabled,
  ~name,
  ~onChange,
  ~labelText,
  ~optionalImageLabel,
  ~errorState,
  ~errorMessage,
) =>
  <div key=id className="mt-4">
    <label className="block tracking-wide text-gray-800 text-xs font-semibold" htmlFor=id>
      {labelText->str}
    </label>
    <input
      disabled
      className="hidden"
      name
      type_="file"
      accept=".jpg,.jpeg,.png,.gif,image/x-png,image/gif,image/jpeg"
      id
      required=false
      multiple=false
      onChange
    />
    <label className="file-input-label mt-2" htmlFor=id>
      <i className="fas fa-upload" /> <span className="ml-2 truncate"> optionalImageLabel </span>
    </label>
    <School__InputGroupError message=errorMessage active=errorState />
  </div>

let initialState = () => {
  logoOnLightBgFilename: None,
  logoOnLightBgInvalid: false,
  coverImageFilename: None,
  coverImageInvalid: false,
  iconFilename: None,
  iconInvalid: false,
  updating: false,
  formDirty: false,
}

let reducer = (state, action) =>
  switch action {
  | SelectLogoOnLightBgFile(name, invalid) => {
      ...state,
      logoOnLightBgFilename: Some(name),
      logoOnLightBgInvalid: invalid,
      formDirty: true,
    }
  | SelectIconFile(name, invalid) => {
      ...state,
      iconFilename: Some(name),
      iconInvalid: invalid,
      formDirty: true,
    }
  | SelectCoverImageFile(name, invalid) => {
      ...state,
      coverImageFilename: Some(name),
      coverImageInvalid: invalid,
      formDirty: true,
    }
  | BeginUpdate => {...state, updating: true}
  | ErrorOccured => {...state, updating: false}
  | DoneUpdating => initialState()
  }

@react.component
let make = (~customizations, ~updateImagesCB, ~authenticityToken) => {
  let (state, send) = React.useReducer(reducer, initialState())
  let logoOnLightBg = customizations->Customizations.logoOnLightBg
  let coverImage = customizations->Customizations.coverImage
  let icon = customizations->Customizations.icon

  <form
    className="mx-8 pt-8"
    id=formId
    key="sc-images-editor__form"
    onSubmit={handleUpdateImages(send, updateImagesCB)}>
    <input name="authenticity_token" type_="hidden" value=authenticityToken />
    <h5 className="uppercase text-center border-b border-gray-400 pb-2">
      {"Manage Images"->str}
    </h5>
    <DisablingCover disabled=state.updating>
      <SchoolCustomize__ImageFileInput
        id="sc-images-editor__logo-on-400-bg-input"
        disabled=state.updating
        name="logo_on_light_bg"
        onChange={updateLogoOnLightBg(send)}
        labelText="Logo on a light background"
        imageName={logoOnLightBg->OptionUtils.map(Customizations.filename)}
        selectedImageName=state.logoOnLightBgFilename
        errorState=state.logoOnLightBgInvalid
      />
      <SchoolCustomize__ImageFileInput
        id="sc-images-editor__icon-input"
        disabled=state.updating
        name="icon"
        onChange={updateIcon(send)}
        labelText="Icon"
        imageName=Some(icon->Customizations.filename)
        selectedImageName=state.iconFilename
        errorState=state.iconInvalid
      />
      <SchoolCustomize__ImageFileInput
        id="sc-images-editor__cover-image-input"
        disabled=state.updating
        name="cover_image"
        onChange={updateCoverImage(send)}
        labelText="Cover image"
        imageName={coverImage->OptionUtils.map(Customizations.filename)}
        selectedImageName=state.coverImageFilename
        errorState=state.coverImageInvalid
      />
      <div className="flex justify-end">
        <button
          type_="submit"
          key="sc-images-editor__update-button"
          disabled={updateButtonDisabled(state)}
          className="btn btn-primary btn-large mt-6">
          {updateButtonText(state.updating)->str}
        </button>
      </div>
    </DisablingCover>
  </form>
}
