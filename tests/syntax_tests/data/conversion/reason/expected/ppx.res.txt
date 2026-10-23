%graphql(
  `
  query Site {
    site {
      siteMetadata {
        title
        description
        siteUrl
      }
    }
  }
`
  {taggedTemplate: false}
)

module Form = %form(
  type input = {
    name: string,
    email: string,
    message: string,
    @as("form-name")
    formName: string,
  }
  type output = input
  let validators = {
    name: {
      strategy: OnFirstBlur,
      validate: ({name, _}) =>
        switch name {
        | "" => Error("Name is required.")
        | name => Ok(name)
        },
    },
    email: {
      strategy: OnFirstBlur,
      validate: ({email, _}) =>
        switch email {
        | "" => Error("Email is required.")
        | email => Ok(email)
        },
    },
    message: {
      strategy: OnFirstBlur,
      validate: ({message, _}) =>
        switch message {
        | "" => Error("Message is required.")
        | message => Ok(message)
        },
    },
    formName: {
      strategy: OnSubmit,
      validate: ({formName, _}) => Ok(formName),
    },
  }
)
