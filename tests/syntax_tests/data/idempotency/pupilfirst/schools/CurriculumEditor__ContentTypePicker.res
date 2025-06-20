let str = React.string

type kindOfButton =
  | MarkdownButton
  | FileButton
  | ImageButton
  | EmbedButton

let buttonClasses = (visibility, staticMode) => {
  let classes = "add-content-block py-3 cursor-pointer"
  classes ++ (visibility || staticMode ? " add-content-block--open" : " ")
}

let button = (sortIndex, newContentBlockCB, kindOfButton) => {
  let (faIcon, buttonText, newBlock) = switch kindOfButton {
  | MarkdownButton => ("fab fa-markdown", "Markdown", () => ContentBlock.makeMarkdownBlock(""))
  | FileButton => ("far fa-file-alt", "File", () => ContentBlock.makeFileBlock("", "", ""))
  | ImageButton => ("far fa-image", "Image", () => ContentBlock.makeImageBlock("", ""))
  | EmbedButton => ("fas fa-code", "Embed", () => ContentBlock.makeEmbedBlock("", ""))
  }

  <div
    key=buttonText
    className="add-content-block__block-content-type-picker px-3 pt-4 pb-3 flex-1 text-center text-primary-200"
    onClick={event => {
      event->ReactEvent.Mouse.preventDefault
      newContentBlockCB(sortIndex, newBlock())
    }}>
    <i className={faIcon ++ " text-2xl"} /> <p className="font-semibold"> {buttonText->str} </p>
  </div>
}

@react.component
let make = (~sortIndex, ~staticMode, ~newContentBlockCB) => {
  let (visibility, setVisibility) = React.useState(() => false)
  <div className={buttonClasses(visibility, staticMode)}>
    {staticMode
    /* Spacer for add-content-block section */
      ? <div className="h-10" />
      : <div
          className="add-content-block__plus-button-container relative"
          onClick={_event => setVisibility(_ => !visibility)}>
          <div
            id={"add-block-" ++ (sortIndex->string_of_int)}
            title="Add block"
            className="add-content-block__plus-button bg-gray-200 hover:bg-gray-300 relative rounded-lg border border-gray-500 w-10 h-10 flex justify-center items-center mx-auto z-20">
            <i className="fas fa-plus text-base add-content-block__plus-button-icon" />
          </div>
        </div>}
    <div
      className="add-content-block__block-content-type text-sm hidden shadow-lg mx-auto relative bg-primary-900 rounded-lg -mt-4 z-10"
      id={"content-type-picker-" ++ (sortIndex->string_of_int)}>
      {[MarkdownButton, ImageButton, EmbedButton, FileButton]
      ->Array.map(button(sortIndex, newContentBlockCB))
      ->React.array}
    </div>
  </div>
}
