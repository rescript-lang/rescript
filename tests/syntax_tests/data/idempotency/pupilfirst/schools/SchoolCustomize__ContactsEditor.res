open SchoolCustomize__Types

let str = ReasonReact.string

type action =
  | UpdateAddress(string)
  | UpdateEmailAddress(string, bool)
  | BeginUpdate
  | ErrorOccured
  | DoneUpdating

type state = {
  address: string,
  emailAddress: string,
  emailAddressInvalid: bool,
  updating: bool,
  formDirty: bool,
}

let handleInputChange = (callback, event) => {
  let value = ReactEvent.Form.target(event)["value"]
  callback(value)
}

let updateContactDetailsButtonText = updating => updating ? "Updating..." : "Update Contact Details"

module UpdateContactDetailsQuery = %graphql(`
   mutation UpdateAddressAndEmailMutation($address: String!, $emailAddress: String!) {
     updateAddress: updateSchoolString(key: "address", value: $address) {
       errors
     }

     updateEmailAddress: updateSchoolString(key: "email_address", value: $emailAddress) {
       errors
     }
   }
  `)

module UpdateSchoolStringErrorHandler = GraphqlErrorHandler.Make(
  SchoolCustomize__UpdateSchoolStringError,
)

let handleUpdateContactDetails = (state, send, updateAddressCB, updateEmailAddressCB, event) => {
  event->ReactEvent.Mouse.preventDefault
  send(BeginUpdate)

  UpdateContactDetailsQuery.make(~address=state.address, ~emailAddress=state.emailAddress, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(result =>
    switch (result["updateAddress"]["errors"], result["updateEmailAddress"]["errors"]) {
    | ([], []) =>
      Notification.success("Done!", "Contact details have been updated.")
      updateAddressCB(state.address)
      updateEmailAddressCB(state.emailAddress)
      send(DoneUpdating)
      Js.Promise.resolve()
    | ([], errors) =>
      Notification.notice("Partial success!", "We were only able to update the address.")
      Js.Promise.reject(UpdateSchoolStringErrorHandler.Errors(errors))
    | (errors, []) =>
      Notification.notice("Partial success!", "We were only able to update the email address.")
      Js.Promise.reject(UpdateSchoolStringErrorHandler.Errors(errors))
    | (addressErrors, emailAddressErrors) =>
      let errors = addressErrors->Array.append(emailAddressErrors)
      Js.Promise.reject(UpdateSchoolStringErrorHandler.Errors(errors))
    }
  )
  ->UpdateSchoolStringErrorHandler.catch(() => send(ErrorOccured))
  ->ignore
  ()
}

let updateButtonDisabled = state =>
  if state.updating {
    true
  } else {
    !state.formDirty || state.emailAddressInvalid
  }

let initialState = customizations => {
  address: customizations->Customizations.address->OptionUtils.default(""),
  emailAddress: customizations->Customizations.emailAddress->OptionUtils.default(""),
  emailAddressInvalid: false,
  updating: false,
  formDirty: false,
}

let reducer = (state, action) =>
  switch action {
  | UpdateAddress(address) => {...state, address: address, formDirty: true}
  | UpdateEmailAddress(emailAddress, invalid) => {
      ...state,
      emailAddress: emailAddress,
      emailAddressInvalid: invalid,
      formDirty: true,
    }
  | BeginUpdate => {...state, updating: true}
  | ErrorOccured => {...state, updating: false}
  | DoneUpdating => {...state, updating: false, formDirty: false}
  }

@react.component
let make = (~customizations, ~updateAddressCB, ~updateEmailAddressCB) => {
  let (state, send) = React.useReducer(reducer, initialState(customizations))

  <div className="mx-8 pt-8">
    <h5 className="uppercase text-center border-b border-gray-400 pb-2">
      {"Manage Contact Details"->str}
    </h5>
    <DisablingCover disabled=state.updating>
      <div key="contacts-editor__address-input-group" className="mt-3">
        <label
          className="inline-block tracking-wide text-xs font-semibold"
          htmlFor="contacts-editor__address">
          {"Contact Address "->str} <i className="fab fa-markdown text-base" />
        </label>
        <textarea
          maxLength=1000
          className="appearance-none block w-full bg-white text-gray-800 border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
          id="contacts-editor__address"
          placeholder="Leave the address empty to hide the footer section."
          onChange={handleInputChange(address => send(UpdateAddress(address)))}
          value=state.address
        />
      </div>
      <div key="contacts-editor__email-address-input-group" className="mt-3">
        <label
          className="inline-block tracking-wide text-xs font-semibold"
          htmlFor="contacts-editor__email-address">
          {"Email Address"->str}
        </label>
        <input
          type_="text"
          maxLength=250
          className="appearance-none block w-full bg-white text-gray-800 border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
          id="contacts-editor__email-address"
          placeholder="Leave the email address empty to hide the footer link."
          onChange={handleInputChange(emailAddress =>
            send(UpdateEmailAddress(emailAddress, emailAddress->EmailUtils.isInvalid(true)))
          )}
          value=state.emailAddress
        />
        <School__InputGroupError
          message="is not a valid email address" active=state.emailAddressInvalid
        />
      </div>
      <button
        key="contacts-editor__update-button"
        disabled={updateButtonDisabled(state)}
        onClick={handleUpdateContactDetails(state, send, updateAddressCB, updateEmailAddressCB)}
        className="w-full bg-indigo-600 hover:bg-blue-600 text-white font-bold py-3 px-6 rounded focus:outline-none mt-3">
        {updateContactDetailsButtonText(state.updating)->str}
      </button>
    </DisablingCover>
  </div>
}
