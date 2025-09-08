%raw(`require("../shared/background_patterns.css")`)
%raw(`require("./CoursesApply__Root.css")`)

@module("./images/email-sent-icon.svg")
external emailSentIcon: string = "default"

let str = React.string

type views =
  | Apply
  | EmailSent

let setViewEmailSent = (setView, ()) => setView(_ => EmailSent)

let emailSentMessage = () =>
  <div>
    <img className="mx-auto w-44 sm:w-48" src=emailSentIcon />
    <div className="text-xl font-bold text-center mt-4">
      {"We've sent you a verification mail."->str}
    </div>
    <p className="mt-4 text-center">
      {"It should reach you in less than a minute. Click the link in the email to sign up, and get started."->str}
    </p>
  </div>

@react.component
let make = (~courseName, ~courseId, ~thumbnailUrl, ~email, ~name, ~privacyPolicy, ~termsOfUse) => {
  let (view, setView) = React.useState(() => Apply)

  <div className="flex min-h-screen bg-gray-100 items-center justify-center">
    <div className="py-8 w-full">
      <div className="container mx-auto px-3 max-w-lg">
        <div
          className="relative flex flex-col shadow-xl rounded-lg overflow-hidden bg-white border">
          <div className="flex flex-col text-gray-900 bg-gray-200 text-white">
            <div className="relative pb-1/2 bg-primary-900">
              {switch thumbnailUrl {
              | Some(src) => <img className="absolute h-full w-full object-cover" src />
              | None =>
                <div
                  className="course-apply__cover-default absolute h-full w-full svg-bg-pattern-1"
                />
              }}
            </div>
          </div>
          <div className="">
            <div className="p-4 pt-5 md:px-12 md:py-12 md:pt-10">
              {switch view {
              | Apply =>
                <CoursesApply__Form
                  courseName courseId setViewEmailSent={setViewEmailSent(setView)} email name
                />
              | EmailSent => emailSentMessage()
              }}
            </div>
          </div>
        </div>
        <div className="text-center mt-4 text-gray-700">
          {termsOfUse
            ? <a
                href="/agreements/terms-of-use"
                className="text-xs cursor-pointer hover:text-primary-500">
                {"Terms of Use"->str}
              </a>
            : React.null}
          {termsOfUse && privacyPolicy
            ? <span className="px-4 text-gray-500" dangerouslySetInnerHTML={"__html": "&vert;"} />
            : React.null}
          {privacyPolicy
            ? <a
                href="/agreements/privacy-policy"
                className="text-xs cursor-pointer hover:text-primary-500">
                {"Privacy Policy"->str}
              </a>
            : React.null}
        </div>
      </div>
    </div>
  </div>
}
