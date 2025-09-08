open SchoolAdminNavbar__Types

let str = React.string

let handleClick = (setShowDropdown, event) => {
  event->ReactEvent.Mouse.preventDefault
  setShowDropdown(showDropdown => !showDropdown)
}

@react.component
let make = (~courses, ~currentCourseId) => {
  let (showDropdown, setShowDropdown) = React.useState(() => false)
  let currentCourse =
    courses->ListUtils.unsafeFind(
      course => course->Course.id == currentCourseId,
      "Could not find currentCourse with ID " ++ currentCourseId,
    )

  <div className="flex-1 flex flex-col bg-transparent mb-3">
    <div className="inline-block relative border-b border-gray-400 rounded">
      <button
        onClick={handleClick(setShowDropdown)}
        title={currentCourse->Course.name}
        className="appearance-none flex items-center justify-between hover:bg-primary-100 hover:text-primary-500 focus:outline-none focus:bg-white focus:text-primary-500 font-semibold relative px-2 py-2 rounded w-full">
        <span className="w-5/6 flex items-center">
          <i className="fas fa-book" />
          <span className="truncate ml-2 text-left"> {currentCourse->Course.name->str} </span>
        </span>
        <span className="w-1/6 text-right"> <i className="fas fa-chevron-down text-sm" /> </span>
      </button>
      {showDropdown
        ? <div
            className="bg-white shadow-lg rounded-b-lg border absolute overflow-hidden min-w-full w-auto z-50">
            {courses
            ->Course.sort
            ->List.filter(course => course->Course.id != (currentCourse->Course.id))
            ->List.map(course =>
              <a
                className="block px-4 py-3 text-xs font-semibold text-gray-900 border-b border-gray-200 bg-white hover:text-primary-500 hover:bg-gray-200 whitespace-no-wrap"
                key={course->Course.id}
                href={"/school/courses/" ++ ((course->Course.id) ++ "/curriculum")}>
                {course->Course.name->str}
              </a>
            )
            ->Array.of_list
            ->React.array}
          </div>
        : React.null}
    </div>
  </div>
}
