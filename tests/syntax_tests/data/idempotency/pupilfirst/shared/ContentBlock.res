exception UnexpectedBlockType(string)

type markdown = string
type url = string
type title = string
type caption = string
type embedCode = string
type filename = string

type blockType =
  | Markdown(markdown)
  | File(url, title, filename)
  | Image(url, caption)
  | Embed(url, embedCode)

type rec t = {
  id: id,
  blockType: blockType,
  sortIndex: int,
}
and id = string

let decodeMarkdownContent = json => {
  open Json.Decode
  json->field("markdown", string)
}
let decodeFileContent = json => {
  open Json.Decode
  json->field("title", string)
}
let decodeImageContent = json => {
  open Json.Decode
  json->field("caption", string)
}
let decodeEmbedContent = json => {
  open Json.Decode
  (json->field("url", string), json->field("embedCode", string))
}

let decode = json => {
  open Json.Decode

  let blockType = switch json->field("blockType", string) {
  | "markdown" => Markdown(json->field("content", decodeMarkdownContent))
  | "file" =>
    let title = json->field("content", decodeFileContent)
    let url = json->field("fileUrl", string)
    let filename = json->field("filename", string)
    File(url, title, filename)
  | "image" =>
    let caption = json->field("content", decodeImageContent)
    let url = json->field("fileUrl", string)
    Image(url, caption)
  | "embed" =>
    let (url, embedCode) = json->field("content", decodeEmbedContent)
    Embed(url, embedCode)
  | unknownBlockType => raise(UnexpectedBlockType(unknownBlockType))
  }

  {
    id: json->field("id", string),
    blockType: blockType,
    sortIndex: json->field("sortIndex", int),
  }
}

let sort = blocks => blocks->ArrayUtils.copyAndSort((x, y) => x.sortIndex - y.sortIndex)

let id = t => t.id
let blockType = t => t.blockType
let sortIndex = t => t.sortIndex

let makeMarkdownBlock = markdown => Markdown(markdown)
let makeImageBlock = (fileUrl, caption) => Image(fileUrl, caption)
let makeFileBlock = (fileUrl, title, fileName) => File(fileUrl, title, fileName)
let makeEmbedBlock = (url, embedCode) => Embed(url, embedCode)

let make = (id, blockType, sortIndex) => {id: id, blockType: blockType, sortIndex: sortIndex}

let makeFromJs = js => {
  let id = js["id"]
  let sortIndex = js["sortIndex"]
  let blockType = switch js["content"] {
  | #MarkdownBlock(content) => Markdown(content["markdown"])
  | #FileBlock(content) => File(content["url"], content["title"], content["filename"])
  | #ImageBlock(content) => Image(content["url"], content["caption"])
  | #EmbedBlock(content) => Embed(content["url"], content["embedCode"])
  }

  make(id, blockType, sortIndex)
}

let blockTypeAsString = blockType =>
  switch blockType {
  | Markdown(_markdown) => "markdown"
  | File(_url, _title, _filename) => "file"
  | Image(_url, _caption) => "image"
  | Embed(_url, _embedCode) => "embed"
  }

let incrementSortIndex = t => {...t, sortIndex: t.sortIndex + 1}

let reindex = ts => ts->List.mapi((sortIndex, t) => {...t, sortIndex: sortIndex})

let moveUp = (t, ts) =>
  ts->sort->Array.to_list->ListUtils.swapUp(t)->reindex->Array.of_list

let moveDown = (t, ts) =>
  ts->sort->Array.to_list->ListUtils.swapDown(t)->reindex->Array.of_list

let updateFile = (title, t) =>
  switch t.blockType {
  | File(url, _, filename) => {...t, blockType: File(url, title, filename)}
  | Markdown(_)
  | Image(_)
  | Embed(_) => t
  }

let updateImage = (caption, t) =>
  switch t.blockType {
  | Image(url, _) => {...t, blockType: Image(url, caption)}
  | Markdown(_)
  | File(_)
  | Embed(_) => t
  }

let updateMarkdown = (markdown, t) =>
  switch t.blockType {
  | Markdown(_) => {...t, blockType: Markdown(markdown)}
  | File(_)
  | Image(_)
  | Embed(_) => t
  }

module Fragments = %graphql(`
  fragment allFields on ContentBlock {
    id
    blockType
    sortIndex
    content {
      ... on ImageBlock {
        caption
        url
        filename
      }
      ... on FileBlock {
        title
        url
        filename
      }
      ... on MarkdownBlock {
        markdown
      }
      ... on EmbedBlock {
        url
        embedCode
      }
    }
  }
`)

module Query = %graphql(`
    query ContentBlocksWithVersionsQuery($targetId: ID!, $targetVersionId: ID) {
      contentBlocks(targetId: $targetId, targetVersionId: $targetVersionId) {
        id
        blockType
        sortIndex
        content {
          ... on ImageBlock {
            caption
            url
            filename
          }
          ... on FileBlock {
            title
            url
            filename
          }
          ... on MarkdownBlock {
            markdown
          }
          ... on EmbedBlock {
            url
            embedCode
          }
        }
      }
      versions(targetId: $targetId){
        id
        createdAt
        updatedAt
      }
  }
`)
