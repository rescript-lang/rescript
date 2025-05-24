exception FormNotFound(string)

%raw(`require("./CurriculumEditor__ContentBlockCreator.css")`)

open CurriculumEditor__Types

let str = React.string

module CreateMarkdownContentBlock = %graphql(`
    mutation CreateMarkdownContentBlockMutation($targetId: ID!, $aboveContentBlockId: ID) {
      createMarkdownContentBlock(targetId: $targetId, aboveContentBlockId: $aboveContentBlockId) {
        contentBlock {
          ...ContentBlock.Fragments.AllFields
        }
      }
    }
  `)

module CreateEmbedContentBlock = %graphql(`
    mutation CreateEmbedContentBlockMutation($targetId: ID!, $aboveContentBlockId: ID, $url: String!) {
      createEmbedContentBlock(targetId: $targetId, aboveContentBlockId: $aboveContentBlockId, url: $url) {
        contentBlock {
          ...ContentBlock.Fragments.AllFields
        }
      }
    }
  `)

type ui =
  | Hidden
  | BlockSelector
  | EmbedForm(string)

type state = {
  ui: ui,
  saving: bool,
  error: option<string>,
}

type action =
  | ToggleVisibility
  | ToggleSaving
  | FinishSaving(bool)
  | SetError(string)
  | FailedToCreate
  | FailToUpload
  | ShowEmbedForm
  | HideEmbedForm
  | UpdateEmbedUrl(string)

let computeInitialState = isAboveTarget => {
  ui: isAboveTarget ? Hidden : BlockSelector,
  saving: false,
  error: None,
}

let reducer = (state, action) =>
  switch action {
  | ToggleVisibility =>
    let ui = switch state.ui {
    | Hidden => BlockSelector
    | BlockSelector
    | EmbedForm(_) =>
      Hidden
    }

    {...state, ui: ui}
  | ToggleSaving => {...state, saving: !state.saving, error: None}
  | FinishSaving(isAboveTarget) => computeInitialState(isAboveTarget)
  | SetError(error) => {...state, error: Some(error)}
  | FailedToCreate => {
      ...state,
      saving: false,
      error: Some("An unexpected error occured. Please reload the page and try again."),
    }
  | FailToUpload => {
      ...state,
      saving: false,
      error: Some("Failed to upload file. Please check message in notification, and try again."),
    }
  | ShowEmbedForm => {...state, ui: EmbedForm("")}
  | HideEmbedForm => {...state, ui: BlockSelector}
  | UpdateEmbedUrl(url) => {...state, ui: EmbedForm(url)}
  }

let containerClasses = (visible, isAboveTarget) => {
  let classes = "content-block-creator py-3"
  classes ++ (visible || !isAboveTarget ? " content-block-creator--open" : "")
}

let handleGraphqlCreateResponse = (aboveContentBlock, send, addContentBlockCB, contentBlock) => {
  switch contentBlock {
  | Some(contentBlock) =>
    contentBlock->ContentBlock.makeFromJs->addContentBlockCB
    send(FinishSaving(aboveContentBlock != None))
  | None => send(ToggleSaving)
  }

  Js.Promise.resolve()
}

let createMarkdownContentBlock = (target, aboveContentBlock, send, addContentBlockCB) => {
  send(ToggleSaving)
  let aboveContentBlockId = aboveContentBlock->OptionUtils.map(ContentBlock.id)
  let targetId = target->Target.id
  CreateMarkdownContentBlock.make(~targetId, ~aboveContentBlockId?, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(result =>
    handleGraphqlCreateResponse(
      aboveContentBlock,
      send,
      addContentBlockCB,
      result["createMarkdownContentBlock"]["contentBlock"],
    )
  )
  ->Js.Promise.catch(_ => {
    send(FailedToCreate)
    Js.Promise.resolve()
  })
  ->ignore
}

let elementId = (prefix, aboveContentBlock) =>
  prefix ++
  switch aboveContentBlock {
  | Some(contentBlock) => contentBlock->ContentBlock.id
  | None => "bottom"
  }

let fileInputId = aboveContentBlock => aboveContentBlock->elementId("markdown-block-file-input-")
let imageInputId = aboveContentBlock =>
  aboveContentBlock->elementId("markdown-block-image-input-")
let fileFormId = aboveContentBlock => aboveContentBlock->elementId("markdown-block-file-form-")
let imageFormId = aboveContentBlock => aboveContentBlock->elementId("markdown-block-image-form-")

let onBlockTypeSelect = (target, aboveContentBlock, send, addContentBlockCB, blockType, _event) =>
  switch blockType {
  | #Markdown => createMarkdownContentBlock(target, aboveContentBlock, send, addContentBlockCB)
  | #File => ()
  | #Image => ()
  | #Embed => send(ShowEmbedForm)
  }

let button = (target, aboveContentBlock, send, addContentBlockCB, blockType) => {
  let fileId = aboveContentBlock->fileInputId
  let imageId = aboveContentBlock->imageInputId

  let (faIcon, buttonText, htmlFor) = switch blockType {
  | #Markdown => ("fab fa-markdown", "Markdown", None)
  | #File => ("far fa-file-alt", "File", Some(fileId))
  | #Image => ("far fa-image", "Image", Some(imageId))
  | #Embed => ("fas fa-code", "Embed", None)
  }

  <label
    ?htmlFor
    key=buttonText
    className="content-block-creator__block-content-type-picker px-3 pt-4 pb-3 flex-1 text-center text-primary-200"
    onClick={onBlockTypeSelect(target, aboveContentBlock, send, addContentBlockCB, blockType)}>
    <i className={faIcon ++ " text-2xl"} /> <p className="font-semibold"> {buttonText->str} </p>
  </label>
}

let maxAllowedFileSize = 5 * 1024 * 1024
let isInvalidFile = file => file["size"] > maxAllowedFileSize

let isInvalidImageFile = image =>
  switch image["_type"] {
  | "image/jpeg"
  | "image/gif"
  | "image/png" => false
  | _ => true
  } ||
  image->isInvalidFile

let uploadFile = (target, send, addContentBlockCB, isAboveContentBlock, formData) =>
  Api.sendFormData(
    "/school/targets/" ++ ((target->Target.id) ++ "/content_block"),
    formData,
    json => {
      Notification.success("Done!", "File uploaded successfully.")
      let contentBlock = json->ContentBlock.decode
      addContentBlockCB(contentBlock)
      send(FinishSaving(isAboveContentBlock))
    },
    () => send(FailToUpload),
  )

let submitForm = (target, aboveContentBlock, send, addContentBlockCB, blockType) => {
  let formId = switch blockType {
  | #File => fileFormId(aboveContentBlock)
  | #Image => imageFormId(aboveContentBlock)
  }

  let element = ReactDOMRe._getElementById(formId)

  switch element {
  | Some(element) =>
    DomUtils.FormData.create(element)->uploadFile(
      target,
      send,
      addContentBlockCB,
      aboveContentBlock != None,
    )
  | None =>
    Rollbar.error("Could not find form to upload file for content block: " ++ formId)
    raise(FormNotFound(formId))
  }
}

let handleFileInputChange = (
  target,
  aboveContentBlock,
  send,
  addContentBlockCB,
  blockType,
  event,
) => {
  event->ReactEvent.Form.preventDefault

  switch ReactEvent.Form.target(event)["files"] {
  | [] => ()
  | files =>
    let file = files[0]

    let error = switch blockType {
    | #File =>
      file->isInvalidFile ? Some("Please select a file with a size less than 5 MB.") : None
    | #Image =>
      file->isInvalidImageFile
        ? Some(
            "Please select an image (PNG, JPEG, GIF) with a size less than 5 MB, and less than 4096px wide or high.",
          )
        : None
    }

    switch error {
    | Some(error) => send(SetError(error))
    | None =>
      // let filename = file##name;
      send(ToggleSaving)
      submitForm(target, aboveContentBlock, send, addContentBlockCB, blockType)
    }
  }
}

let uploadForm = (target, aboveContentBlock, send, addContentBlockCB, blockType) => {
  let fileSelectionHandler = handleFileInputChange(
    target,
    aboveContentBlock,
    send,
    addContentBlockCB,
  )

  let (fileId, formId, onChange, fileType) = switch blockType {
  | #File => (
      fileInputId(aboveContentBlock),
      fileFormId(aboveContentBlock),
      fileSelectionHandler(#File),
      "file",
    )
  | #Image => (
      imageInputId(aboveContentBlock),
      imageFormId(aboveContentBlock),
      fileSelectionHandler(#Image),
      "image",
    )
  }

  <form className="hidden" id=formId>
    <input name="authenticity_token" type_="hidden" value={AuthenticityToken.fromHead()} />
    <input type_="hidden" name="block_type" value=fileType />
    <input type_="file" name="file" id=fileId onChange required=true multiple=false />
    {switch aboveContentBlock {
    | Some(contentBlock) =>
      <input type_="hidden" name="above_content_block_id" value={contentBlock->ContentBlock.id} />
    | None => React.null
    }}
  </form>
}

let visible = state =>
  switch state.ui {
  | Hidden => false
  | BlockSelector
  | EmbedForm(_) => true
  }

let updateEmbedUrl = (send, event) => {
  let value = ReactEvent.Form.target(event)["value"]
  send(UpdateEmbedUrl(value))
}

let embedUrlRegexes = [
  /https:\/\/.*slideshare\.net/,
  /https:\/\/.*vimeo\.com/,
  /https:\/\/.*youtube\.com/,
  /https:\/\/.*youtu\.be/,
]

let validEmbedUrl = url => Belt.Array.some(embedUrlRegexes, regex => regex->Js.Re.test_(url))

let onEmbedFormSave = (target, aboveContentBlock, url, send, addContentBlockCB, event) => {
  event->ReactEvent.Mouse.preventDefault

  if url->validEmbedUrl {
    send(ToggleSaving)

    let aboveContentBlockId = aboveContentBlock->OptionUtils.map(ContentBlock.id)

    let targetId = target->Target.id

    CreateEmbedContentBlock.make(~targetId, ~aboveContentBlockId?, ~url, ())
    ->GraphqlQuery.sendQuery
    ->Js.Promise.then_(result =>
      handleGraphqlCreateResponse(
        aboveContentBlock,
        send,
        addContentBlockCB,
        result["createEmbedContentBlock"]["contentBlock"],
      )
    )
    ->Js.Promise.catch(_ => {
      send(FailedToCreate)
      Js.Promise.resolve()
    })
    ->ignore
  } else {
    send(
      SetError(
        "The URL doesn't look valid. Please make sure that it starts with 'https://' and that it's one of the accepted websites.",
      ),
    )
  }
}

let topButton = (handler, id, title, icon) =>
  <div
    className="content-block-creator__top-button-container relative cursor-pointer" onClick=handler>
    <div
      id={"top-button-" ++ id}
      title
      className="content-block-creator__top-button bg-gray-200 hover:bg-gray-300 relative rounded-lg border border-gray-500 w-10 h-10 flex justify-center items-center mx-auto z-20">
      <FaIcon classes={"text-base fas " ++ icon} />
    </div>
  </div>

let closeEmbedFormButton = (send, aboveContentBlock) => {
  let id = aboveContentBlock->OptionUtils.map(ContentBlock.id)->OptionUtils.default("bottom")

  topButton(_e => send(HideEmbedForm), id, "Close Embed Form", "fa-level-up-alt")
}

let toggleVisibilityButton = (send, contentBlock) =>
  topButton(
    _e => send(ToggleVisibility),
    contentBlock->ContentBlock.id,
    "Toggle Content Block Form",
    "fa-plus content-block-creator__plus-button-icon",
  )

let buttonAboveContentBlock = (state, send, aboveContentBlock) =>
  switch (state.ui, aboveContentBlock) {
  | (EmbedForm(_), Some(_) | None) => closeEmbedFormButton(send, aboveContentBlock)
  | (Hidden, None)
  | (BlockSelector, None) =>
    <div className="h-10" /> // Spacer.
  | (Hidden | BlockSelector, Some(contentBlock)) => toggleVisibilityButton(send, contentBlock)
  }

@react.component
let make = (~target, ~aboveContentBlock=?, ~addContentBlockCB) => {
  let (embedInputId, isAboveContentBlock) = switch aboveContentBlock {
  | Some(contentBlock) =>
    let id = "embed-" ++ (contentBlock->ContentBlock.id)
    (id, true)
  | None => ("embed-bottom", false)
  }

  let (state, send) = React.useReducerWithMapState(
    reducer,
    isAboveContentBlock,
    computeInitialState,
  )

  <DisablingCover disabled=state.saving message="Creating...">
    {uploadForm(target, aboveContentBlock, send, addContentBlockCB, #File)}
    {uploadForm(target, aboveContentBlock, send, addContentBlockCB, #Image)}
    <div className={containerClasses(state->visible, isAboveContentBlock)}>
      {buttonAboveContentBlock(state, send, aboveContentBlock)}
      <div className="content-block-creator__inner-container">
        {switch state.ui {
        | Hidden => React.null
        | BlockSelector =>
          <div
            className="content-block-creator__block-content-type text-sm hidden shadow-lg mx-auto relative bg-primary-900 rounded-lg -mt-4 z-10">
            {[#Markdown, #Image, #Embed, #File]
            ->Array.map(button(target, aboveContentBlock, send, addContentBlockCB))
            ->React.array}
          </div>
        | EmbedForm(url) =>
          <div
            className="clearfix border-2 border-gray-400 bg-gray-200 border-dashed rounded-lg px-3 pb-3 pt-2 -mt-4 z-10">
            <label htmlFor=embedInputId className="text-xs font-semibold">
              {"URL to Embed"->str}
            </label>
            <HelpIcon
              className="ml-2 text-xs"
              link="https://docs.pupilfirst.com/#/curriculum_editor?id=content-block-types">
              {"We support YouTube, Vimeo, and Slideshare URLs. Just copy & paste the full URL to the page that contains the resource that you'd like to embed."->str}
            </HelpIcon>
            <div className="flex mt-1">
              <input
                id=embedInputId
                placeholder="https://www.youtube.com/watch?v="
                className="w-full py-1 px-2 border rounded"
                type_="text"
                value=url
                onChange={updateEmbedUrl(send)}
              />
              <button
                className="ml-2 btn btn-success"
                onClick={onEmbedFormSave(target, aboveContentBlock, url, send, addContentBlockCB)}>
                {"Save"->str}
              </button>
            </div>
          </div>
        }}
      </div>
      {switch state.error {
      | Some(error) => <School__InputGroupError message=error active={state->visible} />
      | None => React.null
      }}
    </div>
  </DisablingCover>
}
