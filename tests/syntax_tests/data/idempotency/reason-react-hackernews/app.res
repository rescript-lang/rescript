@react.component
let make = () => {
  let url = ReasonReact.Router.useUrl()

  switch url.path {
  | list{} => <TopStoriesPage />
  | list{"comments", id} => <CommentsPage id={int_of_string(id)} />
  | _ => <NotFound />
  }
}
