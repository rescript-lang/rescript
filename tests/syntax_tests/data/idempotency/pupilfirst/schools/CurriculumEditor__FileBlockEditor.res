let str = React.string

let onChange = (contentBlock, updateContentBlockCB, event) => {
  event->ReactEvent.Form.preventDefault
  let newTitle = ReactEvent.Form.target(event)["value"]
  let newContentBlock = contentBlock->ContentBlock.updateFile(newTitle)
  updateContentBlockCB(newContentBlock)
}

@react.component
let make = (~url, ~title, ~filename, ~contentBlock, ~updateContentBlockCB) => {
  let titleInputId = "title-" ++ (contentBlock->ContentBlock.id)

  <div className="relative border border-gray-400 rounded-lg">
    <div
      className="content-block__content text-base bg-gray-200 flex justify-center items-center rounded-t-lg">
      <div className="w-full">
        <a
          className="flex justify-between items-center bg-white rounded-t-lg px-6 py-4 hover:bg-gray-100 hover:text-primary-500"
          target="_blank"
          href=url>
          <div className="flex items-center">
            <FaIcon classes="text-4xl text-gray-800 far fa-file-alt" />
            <div className="pl-4 leading-tight h-12 flex flex-col justify-center">
              <div className="text-lg font-semibold"> {title->str} </div>
              <div className="text-sm italic text-gray-600"> {filename->str} </div>
            </div>
          </div>
        </a>
      </div>
    </div>
    <div className="flex border-t justify-end">
      <div className="flex-1 content-block__action-bar-input p-3">
        <label htmlFor=titleInputId className="text-sm font-semibold"> {"Title"->str} </label>
        <span className="text-sm ml-1"> {"(optional)"->str} </span>
        <input
          id=titleInputId
          className="mt-1 appearance-none block w-full h-10 bg-white text-gray-800 border rounded py-3 px-3 focus:border-gray-400 leading-tight focus:outline-none focus:bg-white focus:border-gray"
          onChange={onChange(contentBlock, updateContentBlockCB)}
          maxLength=60
          type_="text"
          value=title
          placeholder="A title for this file"
        />
      </div>
    </div>
  </div>
}
