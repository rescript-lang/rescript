open Belt

let apiBaseUrl = "https://serverless-api.hackernewsmobile.com"

let topStoriesUrl = page => `$apiBaseUrl/topstories-25-$page.json`

let storyUrl = id => `$apiBaseUrl/stories/$id.json`

type story = {
  by: string,
  descendants: int,
  id: int,
  score: int,
  time: int,
  title: string,
  url: option<string>,
}

type comment_deleted = {id: int}

type comment_present = {
  by: string,
  id: int,
  kids: option<array<int>>,
  parent: int,
  text: option<string>,
  time: int,
}

type comment =
  | CommentPresent(comment_present)
  | CommentDeleted(comment_deleted)

type comments_map = Map.Int.t<comment>

type story_with_comments = {
  by: string,
  descendants: int,
  id: int,
  kids: option<array<int>>,
  score: int,
  time: int,
  title: string,
  url: option<string>,
  descendentIds: array<int>,
  comments: comments_map,
}

type topstories = array<story>

module Decode = {
  let idsArray = (json): array<int> => {
    open Json.Decode
    json->array(int)
  }
  let getCommentId = comment =>
    switch comment {
    | CommentDeleted(c) => c.id
    | CommentPresent(c) => c.id
    }
  let comment = (json): comment => {
    let deletedMaybe = {
      open Json.Decode
      json->optional(field("deleted", bool))
    }
    let deleted = switch deletedMaybe {
    | Some(v) => v == true
    | None => false
    }
    if deleted {
      CommentDeleted({
        open Json.Decode
        {id: json->field("id", int)}
      })
    } else {
      CommentPresent({
        open Json.Decode
        {
          by: json->field("by", string),
          id: json->field("id", int),
          parent: json->field("parent", int),
          kids: json->optional(field("kids", idsArray)),
          text: json->optional(field("text", string)),
          time: json->field("time", int),
        }
      })
    }
  }
  let commentsArray = (json): comments_map =>
    Json.Decode.array(comment, json)
    ->Array.map(comment => (getCommentId(comment), comment))
    ->Map.Int.fromArray
  let storyWithComments = (json): story_with_comments => {
    open Json.Decode
    {
      by: json->field("by", string),
      descendants: json->field("descendants", int),
      descendentIds: json->field("descendentIds", idsArray),
      comments: json->field("comments", commentsArray),
      id: json->field("id", int),
      kids: json->optional(field("kids", idsArray)),
      score: json->field("score", int),
      time: json->field("time", int),
      title: json->field("title", string),
      url: json->optional(field("url", string)),
    }
  }
  let story = (json): story => {
    open Json.Decode
    {
      by: json->field("by", string),
      descendants: json->field("descendants", int),
      id: json->field("id", int),
      score: json->field("score", int),
      time: json->field("time", int),
      title: json->field("title", string),
      url: json->optional(field("url", string)),
    }
  }
  let stories = (json): array<story> => {
    open Json.Decode
    json->array(story)
  }
}

let fetchTopStories = (page, callback) => {
  open Js.Promise
  Fetch.fetch(topStoriesUrl(page))
  ->then_(Fetch.Response.json)
  ->then_(json =>
    json
    ->Decode.stories
    ->(
      stories => {
        callback((page, stories))
        resolve()
      }
    )
  )
  ->ignore
} /* TODO: error handling */

let fetchStoryWithComments = (id, callback) => {
  open Js.Promise
  Fetch.fetch(storyUrl(id))
  ->then_(Fetch.Response.json)
  ->then_(json =>
    json
    ->Decode.storyWithComments
    ->(
      stories => {
        callback(stories)
        resolve()
      }
    )
  )
  ->ignore
} /* TODO: error handling */
