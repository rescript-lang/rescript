%raw(`require("./DisablingCover.css")`)

@react.component
let make = (~disabled, ~message="Loading...", ~containerClasses="", ~children) =>
  <div className={"relative " ++ containerClasses}>
    {if disabled {
      [
        <div
          key="school-admin-disabling-cover__blanket"
          className="absolute w-full h-full bg-white opacity-75 z-20 flex items-center justify-center"
        />,
        <div
          key="school-admin-disabling-cover__body"
          className="absolute w-full h-full z-20 flex items-center justify-center">
          <div className="disabling-cover__loading-container bg-white rounded-lg shadow-xl p-4">
            <div className="disabling-cover__loading-animation-box mx-auto">
              <div className="disabling-cover__loading-box-1" />
              <div className="disabling-cover__loading-box-2" />
              <div className="disabling-cover__loading-box-3" />
            </div>
            <span className="block pt-2 font-semibold max-w-sm text-center text-sm">
              {message->React.string}
            </span>
          </div>
        </div>,
      ]->React.array
    } else {
      React.null
    }}
    children
  </div>
