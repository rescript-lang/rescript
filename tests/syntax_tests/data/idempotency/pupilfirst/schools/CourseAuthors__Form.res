let str = React.string

open CourseAuthors__Types

module CreateCourseAuthorQuery = %graphql(`
  mutation CreateCourseAuthorMutation($courseId: ID!, $name: String!, $email: String!) {
    createCourseAuthor(courseId: $courseId, name: $name, email: $email){
      courseAuthor{
        id,
        avatarUrl
      }
    }
  }
`)

module UpdateCourseAuthorQuery = %graphql(`
  mutation UpdateCourseAuthorMutation($id: ID!, $name: String!) {
    updateCourseAuthor(id: $id, name: $name) {
      success
    }
  }
`)

let createCourseAuthorQuery = (courseId, rootPath, email, name, setSaving, addAuthorCB) => {
  setSaving(_ => true)
  CreateCourseAuthorQuery.make(~courseId, ~email, ~name, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    switch response["createCourseAuthor"]["courseAuthor"] {
    | Some(courseAuthor) =>
      addAuthorCB(
        Author.create(~id=courseAuthor["id"], ~name, ~email, ~avatarUrl=courseAuthor["avatarUrl"]),
      )

      ReasonReactRouter.push(rootPath)
    | None => setSaving(_ => false)
    }
    Js.Promise.resolve()
  })
  ->Js.Promise.catch(_ => {
    setSaving(_ => false)
    Js.Promise.resolve()
  })
  ->ignore
}

let updateCourseAuthorQuery = (rootPath, author, name, setSaving, updateAuthorCB) => {
  setSaving(_ => true)
  let id = author->Author.id
  UpdateCourseAuthorQuery.make(~id, ~name, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    if response["updateCourseAuthor"]["success"] {
      updateAuthorCB(author->Author.updateName(name))
      ReasonReactRouter.push(rootPath)
    } else {
      setSaving(_ => false)
    }

    Js.Promise.resolve()
  })
  ->Js.Promise.catch(_ => {
    setSaving(_ => false)
    Js.Promise.resolve()
  })
  ->ignore
}

let handleButtonClick = (
  courseId,
  rootPath,
  author,
  setSaving,
  name,
  email,
  addAuthorCB,
  updateAuthorCB,
  event,
) => {
  event->ReactEvent.Mouse.preventDefault
  switch author {
  | Some(author) => updateCourseAuthorQuery(rootPath, author, name, setSaving, updateAuthorCB)
  | None => createCourseAuthorQuery(courseId, rootPath, email, name, setSaving, addAuthorCB)
  }
}

let isInvalidEmail = email => email->EmailUtils.isInvalid(false)

let showInvalidEmailError = (email, author) =>
  switch author {
  | Some(_) => isInvalidEmail(email)
  | None => email == "" ? false : isInvalidEmail(email)
  }

let showInvalidNameError = (name, author) =>
  switch author {
  | Some(_) => name == ""
  | None => false
  }
let saveDisabled = (email, name, saving, author) =>
  isInvalidEmail(email) ||
  (saving ||
  (name == "" ||
    switch author {
    | Some(author) => author->Author.name == name && author->Author.email == email
    | None => false
    }))

let buttonText = (saving, author) =>
  switch (saving, author) {
  | (true, _) => "Saving"
  | (false, Some(_)) => "Update Author"
  | (false, None) => "Create Author"
  }

let emailInputDisabled = author =>
  switch author {
  | Some(_) => true
  | None => false
  }

@react.component
let make = (~courseId, ~rootPath, ~author, ~addAuthorCB, ~updateAuthorCB) => {
  let (saving, setSaving) = React.useState(() => false)

  let (name, setName) = React.useState(() =>
    switch author {
    | Some(author) => author->Author.name
    | None => ""
    }
  )

  let (email, setEmail) = React.useState(() =>
    switch author {
    | Some(author) => author->Author.email
    | None => ""
    }
  )

  <div className="w-full">
    <DisablingCover disabled=saving>
      <div className="mx-auto bg-white">
        <div className="max-w-2xl p-6 mx-auto">
          <h5 className="uppercase text-center border-b border-gray-400 pb-2 mb-4">
            {switch author {
            | Some(author) => author->Author.name
            | None => "Add new author"
            }->str}
          </h5>
          <div>
            <label
              className="inline-block tracking-wide text-xs font-semibold mb-2 leading-tight"
              htmlFor="email">
              {"Email"->str}
            </label>
            <input
              value=email
              onChange={event => setEmail(ReactEvent.Form.target(event)["value"])}
              className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 leading-snug focus:outline-none focus:bg-white focus:border-gray-500"
              id="email"
              type_="email"
              placeholder="Add email here"
              disabled={emailInputDisabled(author)}
            />
            <School__InputGroupError
              message="Enter a valid Email" active={showInvalidEmailError(email, author)}
            />
          </div>
          <div className="mt-5">
            <label
              className="inline-block tracking-wide text-xs font-semibold mb-2 leading-tight"
              htmlFor="name">
              {"Name"->str}
            </label>
            <input
              value=name
              onChange={event => setName(ReactEvent.Form.target(event)["value"])}
              className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 leading-snug focus:outline-none focus:bg-white focus:border-gray-500"
              id="name"
              type_="text"
              placeholder="Add name here"
            />
            <School__InputGroupError
              message="Enter a valid name" active={showInvalidNameError(name, author)}
            />
          </div>
          <div className="w-auto mt-8">
            <button
              disabled={saveDisabled(email, name, saving, author)}
              onClick={handleButtonClick(
                courseId,
                rootPath,
                author,
                setSaving,
                name,
                email,
                addAuthorCB,
                updateAuthorCB,
              )}
              className="w-full btn btn-large btn-primary">
              {buttonText(saving, author)->str}
            </button>
          </div>
        </div>
      </div>
    </DisablingCover>
  </div>
}
