exception InvalidBlockTypeForUpdate

let str = React.string

type state = {
  dirty: bool,
  saving: option<string>,
  contentBlock: ContentBlock.t,
}

let computeInitialState = contentBlock => {
  saving: None,
  contentBlock: contentBlock,
  dirty: false,
}

type action =
  | StartSaving(string)
  | FinishSaving
  | UpdateContentBlock(ContentBlock.t, bool)
  | FailSaving

let reducer = (state, action) =>
  switch action {
  | StartSaving(message) => {...state, saving: Some(message)}
  | FinishSaving => {...state, saving: None, dirty: false}
  | UpdateContentBlock(contentBlock, dirty) => {
      ...state,
      contentBlock: contentBlock,
      dirty: dirty,
    }
  | FailSaving => {...state, saving: None}
  }

module DeleteContentBlockMutation = %graphql(`
    mutation DeleteContentBlockMutation($id: ID!) {
      deleteContentBlock(id: $id) {
        success
      }
    }
  `)

module MoveContentBlockMutation = %graphql(`
    mutation MoveContentBlockMutation($id: ID!, $direction: MoveDirection!) {
      moveContentBlock(id: $id, direction: $direction) {
        success
      }
    }
  `)

module UpdateFileBlockMutation = %graphql(`
    mutation UpdateFileBlockMutation($id: ID!, $title: String!) {
      updateFileBlock(id: $id, title: $title) {
        contentBlock {
          ...ContentBlock.Fragments.AllFields
        }
      }
    }
  `)

module UpdateMarkdownBlockMutation = %graphql(`
    mutation UpdateMarkdownBlockMutation($id: ID!, $markdown: String!) {
      updateMarkdownBlock(id: $id, markdown: $markdown) {
        contentBlock {
          ...ContentBlock.Fragments.AllFields
        }
      }
    }
  `)

module UpdateImageBlockMutation = %graphql(`
    mutation UpdateImageBlockMutation($id: ID!, $caption: String!) {
      updateImageBlock(id: $id, caption: $caption) {
        contentBlock {
          ...ContentBlock.Fragments.AllFields
        }
      }
    }
  `)

let controlIcon = (~icon, ~title, ~color, ~handler) => {
  let buttonClasses = switch color {
  | #Grey => "hover:bg-gray-200"
  | #Green => "bg-green-600 hover:bg-green-700 text-white rounded-b"
  }

  handler == None
    ? React.null
    : <button
        title
        disabled={handler == None}
        className={"p-2 focus:outline-none " ++ buttonClasses}
        onClick=?handler>
        <i className={"fas fa-fw " ++ icon} />
      </button>
}

let onMove = (contentBlock, cb, direction, _event) => {
  // We don't actually handle the response for this query.
  MoveContentBlockMutation.make(~id=contentBlock->ContentBlock.id, ~direction, ())
  ->GraphqlQuery.sendQuery
  ->ignore

  cb(contentBlock)
}

let onDelete = (contentBlock, removeContentBlockCB, send, _event) =>
  WindowUtils.confirm("Are you sure you want to delete this block?", () => {
    send(StartSaving("Deleting..."))
    let id = contentBlock->ContentBlock.id

    DeleteContentBlockMutation.make(~id, ())
    ->GraphqlQuery.sendQuery
    ->Js.Promise.then_(result => {
      if result["deleteContentBlock"]["success"] {
        removeContentBlockCB(id)
      } else {
        send(FinishSaving)
      }

      Js.Promise.resolve()
    })
    ->Js.Promise.catch(_error => {
      send(FinishSaving)
      Js.Promise.resolve()
    })
    ->ignore
  })

let onUndo = (originalContentBlock, setDirtyCB, send, event) => {
  event->ReactEvent.Mouse.preventDefault

  WindowUtils.confirm("Are you sure you want to undo your changes to this block?", () => {
    setDirtyCB(false)
    send(UpdateContentBlock(originalContentBlock, false))
  })
}

let handleUpdateResult = (updateContentBlockCB, setDirtyCB, send, contentBlock) => {
  switch contentBlock {
  | Some(contentBlock) =>
    contentBlock->ContentBlock.makeFromJs->updateContentBlockCB
    send(FinishSaving)
    setDirtyCB(false)
  | None => send(FailSaving)
  }
  Js.Promise.resolve()
}

let updateContentBlockBlock = (
  mutation,
  contentBlockExtractor,
  updateContentBlockCB,
  setDirtyCB,
  send,
) => {
  send(StartSaving("Updating..."))

  mutation
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(result =>
    result->contentBlockExtractor->handleUpdateResult(updateContentBlockCB, setDirtyCB, send)
  )
  ->Js.Promise.catch(_error => {
    send(FinishSaving)
    Js.Promise.resolve()
  })
  ->ignore
}

let onSave = (contentBlock, updateContentBlockCB, setDirtyCB, send, event) => {
  event->ReactEvent.Mouse.preventDefault
  let id = contentBlock->ContentBlock.id

  switch contentBlock->ContentBlock.blockType {
  | ContentBlock.File(_url, title, _filename) =>
    let mutation = UpdateFileBlockMutation.make(~id, ~title, ())
    let extractor = result => result["updateFileBlock"]["contentBlock"]
    updateContentBlockBlock(mutation, extractor, updateContentBlockCB, setDirtyCB, send)
  | Markdown(markdown) =>
    let mutation = UpdateMarkdownBlockMutation.make(~id, ~markdown, ())
    let extractor = result => result["updateMarkdownBlock"]["contentBlock"]
    updateContentBlockBlock(mutation, extractor, updateContentBlockCB, setDirtyCB, send)
  | Image(_url, caption) =>
    let mutation = UpdateImageBlockMutation.make(~id, ~caption, ())
    let extractor = result => result["updateImageBlock"]["contentBlock"]
    updateContentBlockBlock(mutation, extractor, updateContentBlockCB, setDirtyCB, send)
  | Embed(_) => raise(InvalidBlockTypeForUpdate)
  }
}

let updateContentBlockCB = (originalContentBlock, setDirtyCB, state, send, newContentBlock) => {
  let dirty = newContentBlock != originalContentBlock

  if state.dirty != dirty {
    setDirtyCB(dirty)
  }

  send(UpdateContentBlock(newContentBlock, dirty))
}

let innerEditor = (originalContentBlock, contentBlock, setDirtyCB, state, send) => {
  let updateContentBlockCB = updateContentBlockCB(originalContentBlock, setDirtyCB, state, send)

  switch contentBlock->ContentBlock.blockType {
  | ContentBlock.Embed(_url, embedCode) => TargetContentView.embedContentBlock("", embedCode)
  | Markdown(markdown) =>
    <CurriculumEditor__MarkdownBlockEditor markdown contentBlock updateContentBlockCB />
  | File(url, title, filename) =>
    <CurriculumEditor__FileBlockEditor url title filename contentBlock updateContentBlockCB />
  | Image(url, caption) =>
    <CurriculumEditor__ImageBlockEditor url caption contentBlock updateContentBlockCB />
  }
}

@react.component
let make = (
  ~contentBlock,
  ~setDirtyCB,
  ~removeContentBlockCB=?,
  ~moveContentBlockUpCB=?,
  ~moveContentBlockDownCB=?,
  ~updateContentBlockCB=?,
) => {
  let (state, send) = React.useReducerWithMapState(reducer, contentBlock, computeInitialState)

  <DisablingCover disabled={state.saving != None} message=?state.saving>
    <div
      className="flex items-start"
      ariaLabel={"Editor for content block " ++ (contentBlock->ContentBlock.id)}>
      <div className="flex-grow self-stretch">
        {innerEditor(contentBlock, state.contentBlock, setDirtyCB, state, send)}
      </div>
      <div
        className="ml-2 flex-shrink-0 border-transparent bg-gray-100 border rounded flex flex-col text-xs">
        {controlIcon(
          ~icon="fa-arrow-up",
          ~title="Move Up",
          ~color=#Grey,
          ~handler=moveContentBlockUpCB->OptionUtils.map(cb => onMove(contentBlock, cb, #Up)),
        )}
        {controlIcon(
          ~icon="fa-arrow-down",
          ~title="Move Down",
          ~color=#Grey,
          ~handler=moveContentBlockDownCB->OptionUtils.map(cb => onMove(contentBlock, cb, #Down)),
        )}
        {controlIcon(
          ~icon="fa-trash-alt",
          ~title="Delete",
          ~color=#Grey,
          ~handler=removeContentBlockCB->OptionUtils.map(cb => onDelete(contentBlock, cb, send)),
        )}
        {controlIcon(
          ~icon="fa-undo-alt",
          ~title="Undo Changes",
          ~color=#Grey,
          ~handler=updateContentBlockCB->OptionUtils.map(_cb =>
            onUndo(contentBlock, setDirtyCB, send)
          ),
        )}
        {controlIcon(
          ~icon="fa-check",
          ~title="Save Changes",
          ~color=#Green,
          ~handler=updateContentBlockCB->OptionUtils.map(cb =>
            onSave(state.contentBlock, cb, setDirtyCB, send)
          ),
        )}
      </div>
    </div>
  </DisablingCover>
}
