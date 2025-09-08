%raw(`require("./SkeletonLoading.css")`)

let card = (~className="", ()) =>
  <div className={"skeleton-body-container pt-4 w-full mx-auto" ++ className}>
    <div className="skeleton-body-wrapper px-3 lg:px-0">
      <div className="skeleton-placeholder__card px-5 py-6 bg-white rounded-lg shadow">
        <div className="flex items-center">
          <div className="flex-1">
            <div className="skeleton-placeholder__line-sm w-5/6 skeleton-animate" />
            <div className="skeleton-placeholder__line-sm mt-4 w-4/6 skeleton-animate" />
          </div>
          <div className="skeleton-placeholder__line-sm w-1/6 skeleton-animate" />
        </div>
      </div>
    </div>
  </div>

let userCard = () =>
  <div className="skeleton-body-container pt-4 w-full mx-auto">
    <div className="skeleton-body-wrapper px-2 lg:px-0">
      <div className="skeleton-placeholder__card px-5 py-6 bg-white rounded-lg shadow">
        <div className="flex items-center">
          <div className="w-14 h-14 bg-gray-100 rounded-full mr-4 skeleton-animate" />
          <div className="flex-1">
            <div className="skeleton-placeholder__line-sm w-2/6 skeleton-animate" />
            <div className="skeleton-placeholder__line-sm mt-4 w-3/6 skeleton-animate" />
          </div>
          <div className="skeleton-placeholder__line-sm w-1/6 skeleton-animate" />
        </div>
      </div>
    </div>
  </div>

let heading = () =>
  <div className="skeleton-body-container w-full pb-4 mx-auto">
    <div className="skeleton-body-wrapper mt-8 px-3 lg:px-0">
      <div className="skeleton-placeholder__line-md mt-4 w-3/6 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 w-4/6 skeleton-animate" />
    </div>
  </div>

@live
let codeBlock = () =>
  <div className="skeleton-body-container w-full pb-4 mx-auto">
    <div className="skeleton-body-wrapper max-w-xs mt-8 px-3 lg:px-0">
      <div className="flex">
        <div className="skeleton-placeholder__line-sm mr-3 w-2/6 skeleton-animate" />
        <div className="skeleton-placeholder__line-sm mr-3 w-2/6 skeleton-animate" />
        <div className="skeleton-placeholder__line-sm mr-3 w-4 skeleton-animate" />
      </div>
      <div className="flex ml-6">
        <div className="skeleton-placeholder__line-sm mt-4 mr-3 w-3/6 skeleton-animate" />
        <div className="skeleton-placeholder__line-sm mt-4 mr-3 w-3/6 skeleton-animate" />
      </div>
      <div className="flex ml-6">
        <div className="skeleton-placeholder__line-sm mt-4 mr-3 w-2/6 skeleton-animate" />
        <div className="skeleton-placeholder__line-sm mt-4 mr-3 w-1/6 skeleton-animate" />
        <div className="skeleton-placeholder__line-sm mt-4 mr-3 w-2/6 skeleton-animate" />
      </div>
      <div className="flex">
        <div className="skeleton-placeholder__line-sm mt-4 mr-3 w-16 skeleton-animate" />
        <div className="skeleton-placeholder__line-sm mt-4 mr-3 w-6 skeleton-animate" />
      </div>
    </div>
  </div>

let profileCard = () =>
  <div className="skeleton-body-container w-full pb-4 mx-auto">
    <div className="skeleton-body-wrapper max-w-sm mt-8 px-3 lg:px-0">
      <div className="flex items-center">
        <div className="w-14 h-14 bg-gray-100 rounded-full mr-4 skeleton-animate" />
        <div className="flex-1">
          <div className="skeleton-placeholder__line-sm w-3/6 skeleton-animate" />
          <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
        </div>
      </div>
    </div>
  </div>

let image = () =>
  <div className="skeleton-body-container w-full pb-4 mx-auto">
    <div className="skeleton-body-wrapper mt-8 px-3 lg:px-0">
      <div className="skeleton-placeholder__image mt-5 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mx-auto mt-4 w-3/6 skeleton-animate" />
    </div>
  </div>

let paragraph = () =>
  <div className="skeleton-body-container w-full pb-4 mx-auto">
    <div className="skeleton-body-wrapper mt-8 px-3 lg:px-0">
      <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 w-4/6 skeleton-animate" />
    </div>
  </div>

let contents = () =>
  <div className="skeleton-body-container w-full pb-4 mx-auto">
    <div className="skeleton-body-wrapper mt-8 px-3 lg:px-0">
      <div className="skeleton-placeholder__line-md mt-4 w-3/6 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 w-4/6 skeleton-animate" />
      <div className="skeleton-placeholder__image mt-5 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mx-auto mt-4 w-3/6 skeleton-animate" />
    </div>
    <div className="skeleton-body-wrapper mt-8 px-3 lg:px-0">
      <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 skeleton-animate" />
      <div className="skeleton-placeholder__line-sm mt-4 w-4/6 skeleton-animate" />
    </div>
  </div>

let multiple = (~count, ~element) =>
  Array.make(count, element)
  ->Array.mapi((key, element) => <div key={key->string_of_int}> element </div>)
  ->React.array
