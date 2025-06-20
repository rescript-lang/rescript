open CourseEditor__Types

let str = ReasonReact.string

type action =
  | SelectCover(string, bool)
  | SelectThumb(string, bool)
  | BeginUpdate
  | ErrorOccured
  | DoneUpdating

type state = {
  filenameThumb: option<string>,
  filenameCover: option<string>,
  invalidThumb: bool,
  invalidCover: bool,
  updating: bool,
  formDirty: bool,
}

let updateButtonText = updating => updating ? "Updating..." : "Update Images"

let formId = "course-editor-form-image-form"

let filename = optionalFilename => optionalFilename->OptionUtils.default("unknown")

let handleUpdateCB = (json, state, course, updateCourseCB) => {
  let coverUrl = json->{
    open Json.Decode
    field("cover_url", optional(string))
  }
  let thumbnailUrl = json->{
    open Json.Decode
    field("thumbnail_url", optional(string))
  }

  let newCourse =
    course->Course.addImages(
      ~coverUrl,
      ~thumbnailUrl,
      ~coverFilename=filename(state.filenameCover),
      ~thumbnailFilename=filename(state.filenameThumb),
    )

  updateCourseCB(newCourse)
}

let handleUpdateImages = (send, state, course, updateCourseCB, event) => {
  event->ReactEvent.Form.preventDefault
  send(BeginUpdate)

  let element = ReactDOMRe._getElementById(formId)
  switch element {
  | Some(element) =>
    Api.sendFormData(
      "courses/" ++ ((course->Course.id) ++ "/attach_images"),
      DomUtils.FormData.create(element),
      json => {
        Notification.success("Done!", "Images have been updated successfully.")
        handleUpdateCB(json, state, course, updateCourseCB)
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
    !state.formDirty || (state.invalidThumb || state.invalidCover)
  }

let optionalImageLabelText = (image, selectedFilename) =>
  switch selectedFilename {
  | Some(name) =>
    <span>
      {"You have selected "->str}
      <code className="mr-1"> {name->str} </code>
      {". Click to replace the current image."->str}
    </span>
  | None =>
    switch image {
    | Some(existingImage) =>
      <span>
        {"Please pick a file to replace "->str}
        <code> {existingImage->Course.filename->str} </code>
      </span>
    | None => "Please choose an image file."->str
    }
  }

let maxAllowedSize = 2 * 1024 * 1024

let isInvalidImageFile = image =>
  switch image["_type"] {
  | "image/jpeg"
  | "image/png" => false
  | _ => true
  } ||
  image["size"] > maxAllowedSize

let updateImage = (send, isCover, event) => {
  let imageFile = ReactEvent.Form.target(event)["files"][0]
  isCover
    ? send(SelectCover(imageFile["name"], imageFile->isInvalidImageFile))
    : send(SelectThumb(imageFile["name"], imageFile->isInvalidImageFile))
}

let initialState = () => {
  filenameThumb: None,
  filenameCover: None,
  invalidThumb: false,
  invalidCover: false,
  updating: false,
  formDirty: false,
}

let reducer = (state, action) =>
  switch action {
  | SelectThumb(name, invalid) => {
      ...state,
      filenameThumb: Some(name),
      invalidThumb: invalid,
      formDirty: true,
    }
  | SelectCover(name, invalid) => {
      ...state,
      filenameCover: Some(name),
      invalidCover: invalid,
      formDirty: true,
    }
  | BeginUpdate => {...state, updating: true}
  | DoneUpdating => {...state, updating: false, formDirty: false}
  | ErrorOccured => {...state, updating: false}
  }

@react.component
let make = (~course, ~updateCourseCB, ~closeDrawerCB) => {
  let (state, send) = React.useReducer(reducer, initialState())

  let thumbnail = course->Course.thumbnail
  let cover = course->Course.cover
  <SchoolAdmin__EditorDrawer closeDrawerCB>
    <form
      id=formId
      className="mx-8 pt-8"
      key="sc-images-editor__form"
      onSubmit={handleUpdateImages(send, state, course, updateCourseCB)}>
      <input name="authenticity_token" type_="hidden" value={AuthenticityToken.fromHead()} />
      <h5 className="uppercase text-center border-b border-gray-400 pb-2">
        {"Course Images"->str}
      </h5>
      <DisablingCover disabled=state.updating>
        <div key="course-images-editor__thumbnail" className="mt-4">
          <label
            className="tracking-wide text-gray-800 text-xs font-semibold"
            htmlFor="sc-images-editor__logo-on-400-bg-input">
            {"Thumbnail"->str}
          </label>
          <HelpIcon
            className="text-xs ml-1"
            responsiveAlignment=HelpIcon.NonResponsive(AlignLeft)
            link="https://docs.pupilfirst.com/#/courses?id=course-images">
            {"The thumbnail will be displayed on the homepage, and here in the admin courses list."->str}
          </HelpIcon>
          <input
            disabled=state.updating
            className="hidden"
            name="course_thumbnail"
            type_="file"
            accept=".jpg,.jpeg,.png,.gif,image/x-png,image/gif,image/jpeg"
            id="course-images-editor__thumbnail"
            required=false
            multiple=false
            onChange={updateImage(send, false)}
          />
          <label className="file-input-label mt-2" htmlFor="course-images-editor__thumbnail">
            <i className="fas fa-upload" />
            <span className="ml-2 truncate">
              {optionalImageLabelText(thumbnail, state.filenameThumb)}
            </span>
          </label>
          <School__InputGroupError
            message="must be a JPEG / PNG under 2 MB in size" active=state.invalidThumb
          />
        </div>
        <div key="course-images-editor__cover" className="mt-4">
          <label
            className="tracking-wide text-gray-800 text-xs font-semibold"
            htmlFor="sc-images-editor__logo-on-400-bg-input">
            {"Cover Image"->str}
          </label>
          <HelpIcon
            className="text-xs ml-1"
            responsiveAlignment=HelpIcon.NonResponsive(AlignLeft)
            link="https://docs.pupilfirst.com/#/courses?id=course-images">
            {"The cover image for a course will be displayed at the top of all student pages within the course."->str}
          </HelpIcon>
          <input
            disabled=state.updating
            className="hidden"
            name="course_cover"
            type_="file"
            accept=".jpg,.jpeg,.png,.gif,image/x-png,image/gif,image/jpeg"
            id="course-images-editor__cover"
            required=false
            multiple=false
            onChange={updateImage(send, true)}
          />
          <label className="file-input-label mt-2" htmlFor="course-images-editor__cover">
            <i className="fas fa-upload" />
            <span className="ml-2 truncate">
              {optionalImageLabelText(cover, state.filenameCover)}
            </span>
          </label>
          <School__InputGroupError
            message="must be a JPEG / PNG under 2 MB in size" active=state.invalidCover
          />
        </div>
        <button
          type_="submit"
          key="sc-images-editor__update-button"
          disabled={updateButtonDisabled(state)}
          className="btn btn-primary btn-large mt-6">
          {updateButtonText(state.updating)->str}
        </button>
      </DisablingCover>
    </form>
  </SchoolAdmin__EditorDrawer>
}
