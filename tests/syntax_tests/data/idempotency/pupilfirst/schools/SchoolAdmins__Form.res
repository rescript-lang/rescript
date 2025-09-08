let str = React.string

module CreateSchoolAdminQuery = %graphql(`
  mutation CreateSchoolAdminMutation($name: String!, $email: String!) {
    createSchoolAdmin(name: $name, email: $email){
      schoolAdmin{
        id,
        avatarUrl
      }
    }
  }
`)

module UpdateSchoolAdminQuery = %graphql(`
  mutation UpdateSchoolAdminMutation($id: ID!, $name: String!) {
    updateSchoolAdmin(id: $id, name: $name) {
      success
    }
  }
`)

let createSchoolAdminQuery = (email, name, setSaving, updateCB) => {
  setSaving(_ => true)
  CreateSchoolAdminQuery.make(~email, ~name, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    switch response["createSchoolAdmin"]["schoolAdmin"] {
    | Some(schoolAdmin) =>
      updateCB(
        SchoolAdmin.create(
          ~id=schoolAdmin["id"],
          ~name,
          ~email,
          ~avatarUrl=schoolAdmin["avatarUrl"],
        ),
      )
      Notification.success("Success", "School Admin created successfully.")
    | None => setSaving(_ => false)
    }
    Js.Promise.resolve()
  })
  ->ignore
  ()
}

let updateSchoolAdminQuery = (admin, name, setSaving, updateCB) => {
  setSaving(_ => true)
  let id = admin->SchoolAdmin.id
  UpdateSchoolAdminQuery.make(~id, ~name, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    response["updateSchoolAdmin"]["success"]
      ? {
          updateCB(admin->SchoolAdmin.updateName(name))
          Notification.success("Success", "School Admin updated successfully.")
        }
      : setSaving(_ => false)
    Js.Promise.resolve()
  })
  ->ignore
}

let handleButtonClick = (admin, setSaving, name, email, updateCB, event) => {
  event->ReactEvent.Mouse.preventDefault
  switch admin {
  | Some(admin) => updateSchoolAdminQuery(admin, name, setSaving, updateCB)
  | None => createSchoolAdminQuery(email, name, setSaving, updateCB)
  }
}

let isInvalidEmail = email => email->EmailUtils.isInvalid(false)

let showInvalidEmailError = (email, admin) =>
  switch admin {
  | Some(_) => isInvalidEmail(email)
  | None => email == "" ? false : isInvalidEmail(email)
  }

let showInvalidNameError = (name, admin) =>
  switch admin {
  | Some(_) => name == ""
  | None => false
  }
let saveDisabled = (email, name, saving, admin) =>
  isInvalidEmail(email) ||
  (saving ||
  (name == "" ||
    switch admin {
    | Some(admin) => admin->SchoolAdmin.name == name && admin->SchoolAdmin.email == email
    | None => false
    }))

let buttonText = (saving, admin) =>
  switch (saving, admin) {
  | (true, _) => "Saving"
  | (false, Some(_)) => "Update School Admin"
  | (false, None) => "Create School Admin"
  }

let emailInputDisabled = admin =>
  switch admin {
  | Some(_) => true
  | None => false
  }

@react.component
let make = (~admin, ~updateCB) => {
  let (saving, setSaving) = React.useState(() => false)

  let (name, setName) = React.useState(() =>
    switch admin {
    | Some(admin) => admin->SchoolAdmin.name
    | None => ""
    }
  )

  let (email, setEmail) = React.useState(() =>
    switch admin {
    | Some(admin) => admin->SchoolAdmin.email
    | None => ""
    }
  )

  <div className="w-full">
    <DisablingCover disabled=saving>
      <div className="mx-auto bg-white">
        <div className="max-w-2xl p-6 mx-auto">
          <h5 className="uppercase text-center border-b border-gray-400 pb-2 mb-4">
            {switch admin {
            | Some(admin) => admin->SchoolAdmin.name
            | None => "Add new school admin"
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
              disabled={emailInputDisabled(admin)}
            />
            <School__InputGroupError
              message="Enter a valid Email" active={showInvalidEmailError(email, admin)}
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
              message="Enter a valid name" active={showInvalidNameError(name, admin)}
            />
          </div>
          <div className="w-auto mt-8">
            <button
              disabled={saveDisabled(email, name, saving, admin)}
              onClick={handleButtonClick(admin, setSaving, name, email, updateCB)}
              className="w-full btn btn-large btn-primary">
              {buttonText(saving, admin)->str}
            </button>
          </div>
        </div>
      </div>
    </DisablingCover>
  </div>
}
