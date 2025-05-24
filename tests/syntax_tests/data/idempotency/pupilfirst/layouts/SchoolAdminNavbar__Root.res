exception UnknownPathEncountered(list<string>)

%raw(`require("./SchoolAdminNavbar__Root.css")`)

open SchoolAdminNavbar__Types

type courseSelection =
  | Students
  | CourseCoaches
  | Curriculum
  | EvaluationCriteria
  | CourseExports
  | Authors

type settingsSelection =
  | Customization
  | Admins

type userRole =
  | SchoolAdmin
  | CourseAuthor

type selection =
  | Overview
  | SchoolCoaches
  | Settings(settingsSelection)
  | Courses
  | SelectedCourse(Course.id, courseSelection)
  | Communities

let str = React.string

let containerClasses = shrunk => {
  let defaultClasses = "bg-gradient-primary-600-to-primary-800-to-bottom school-admin-navbar__primary-nav flex flex-col justify-between "

  defaultClasses ++ (shrunk ? "school-admin-navbar__primary-nav--shrunk" : "overflow-y-auto")
}

let headerclasses = shrunk => {
  let defaultClasses = "school-admin-navbar__header "
  defaultClasses ++ (
    shrunk
      ? "mx-auto"
      : "px-5 py-2 relative z-20 border-r border-b border-gray-400 bg-white flex h-16 items-center"
  )
}

let imageContainerClasses = shrunk => {
  let defaultClasses = "school-admin-navbar__school-logo-container flex items-center "
  defaultClasses ++ (shrunk ? "justify-center w-16 h-16" : "bg-white h-8 w-3/5 rounded")
}

let bottomLinkClasses = shrunk => {
  let defaultClasses = "flex text-white text-sm py-4 px-5 hover:bg-primary-900 font-semibold items-center "
  defaultClasses ++ (shrunk ? "justify-center" : "")
}

let bottomLink = (path, shrunk, iconClasses, text) => {
  let title = shrunk ? Some(text) : None

  <li>
    <a ?title href=path className={bottomLinkClasses(shrunk)}>
      <i className={iconClasses ++ " fa-fw"} />
      {shrunk ? React.null : <span className="ml-2"> {text->str} </span>}
    </a>
  </li>
}

let topLink = (selectedOption, currentOption, path, shrunk, iconClasses, text) => {
  let defaultClasses = "school-admin-navbar__primary-nav-link py-4 px-5"
  let classes =
    defaultClasses ++ (
      selectedOption == currentOption ? " school-admin-navbar__primary-nav-link--active" : ""
    )
  let title = shrunk ? Some(text) : None
  <a href=path className=classes ?title>
    <i className={iconClasses ++ " fa-fw text-lg"} />
    {shrunk ? React.null : <span className="ml-2"> {text->str} </span>}
  </a>
}

let secondaryNavOption = (path, currentSelection, inspectedSelection, text) => {
  let defaultClasses = "flex text-indigo-800 text-sm py-3 px-4 hover:bg-gray-400 focus:bg-gray-400 font-semibold rounded items-center my-1"
  let classes = defaultClasses ++ (currentSelection == inspectedSelection ? " bg-gray-400" : "")

  <li key=text> <a href=path className=classes> {text->str} </a> </li>
}

let secondaryNav = (courses, userRole, selectedOption) =>
  switch selectedOption {
  | Settings(settingsSelection) =>
    <div
      key="secondary-nav"
      className="bg-gray-200 school-admin-navbar__secondary-nav w-full border-r border-gray-400 pb-6 overflow-y-auto">
      <ul className="p-4">
        {secondaryNavOption("/school/customize", settingsSelection, Customization, "Customization")}
        {secondaryNavOption("/school/admins", settingsSelection, Admins, "Admins")}
      </ul>
    </div>
  | SelectedCourse(courseId, courseSelection) =>
    <div
      key="secondary-nav"
      className="bg-gray-200 school-admin-navbar__secondary-nav w-full border-r border-gray-400 pb-6 overflow-y-auto">
      <ul className="p-4">
        <li> <SchoolAdminNavbar__CourseDropdown courses currentCourseId=courseId /> </li>
        {secondaryNavOption(
          "/school/courses/" ++ (courseId ++ "/curriculum"),
          courseSelection,
          Curriculum,
          "Curriculum",
        )}
        {switch userRole {
        | SchoolAdmin =>
          [
            secondaryNavOption(
              "/school/courses/" ++ (courseId ++ "/students"),
              courseSelection,
              Students,
              "Students",
            ),
            secondaryNavOption(
              "/school/courses/" ++ (courseId ++ "/coaches"),
              courseSelection,
              CourseCoaches,
              "Coaches",
            ),
            secondaryNavOption(
              "/school/courses/" ++ (courseId ++ "/exports"),
              courseSelection,
              CourseExports,
              "Exports",
            ),
            secondaryNavOption(
              "/school/courses/" ++ (courseId ++ "/authors"),
              courseSelection,
              Authors,
              "Authors",
            ),
          ]->React.array
        | CourseAuthor => React.null
        }}
        {secondaryNavOption(
          "/school/courses/" ++ (courseId ++ "/evaluation_criteria"),
          courseSelection,
          EvaluationCriteria,
          "Evaluation Criteria",
        )}
      </ul>
    </div>
  | _ => React.null
  }

@react.component
let make = (~schoolName, ~schoolLogoPath, ~schoolIconPath, ~courses, ~isCourseAuthor) => {
  let url = ReasonReactRouter.useUrl()

  let userRole = isCourseAuthor ? CourseAuthor : SchoolAdmin

  let (selectedOption, shrunk) = switch url.path {
  | list{"school"} => (Overview, false)
  | list{"school", "coaches"} => (SchoolCoaches, false)
  | list{"school", "customize"} => (Settings(Customization), true)
  | list{"school", "courses"} => (Courses, false)
  | list{"school", "courses", courseId, "students"}
  | list{"school", "courses", courseId, "inactive_students"} => (
      SelectedCourse(courseId, Students),
      true,
    )
  | list{"school", "courses", courseId, "coaches"} => (
      SelectedCourse(courseId, CourseCoaches),
      true,
    )
  | list{"school", "courses", courseId, "curriculum"} => (
      SelectedCourse(courseId, Curriculum),
      true,
    )
  | list{
      "school",
      "courses",
      courseId,
      "targets",
      _targetId,
      "content" | "versions" | "details",
    } => (SelectedCourse(courseId, Curriculum), true)
  | list{"school", "courses", courseId, "exports"} => (
      SelectedCourse(courseId, CourseExports),
      true,
    )
  | list{"school", "courses", courseId, "authors"} => (SelectedCourse(courseId, Authors), true)
  | list{"school", "courses", courseId, "authors", _authorId} => (
      SelectedCourse(courseId, Authors),
      true,
    )
  | list{"school", "courses", courseId, "evaluation_criteria"} => (
      SelectedCourse(courseId, EvaluationCriteria),
      true,
    )
  | list{"school", "communities"} => (Communities, false)
  | list{"school", "admins"} => (Settings(Admins), true)
  | _ =>
    Rollbar.critical(
      "Unknown path encountered by SA navbar: " ++
      (url.path->Array.of_list->Js.Array.joinWith("/")),
    )
    raise(UnknownPathEncountered(url.path))
  }

  [
    <div key="main-nav" className={containerClasses(shrunk)}>
      <div>
        <div className={headerclasses(shrunk)}>
          <div className={imageContainerClasses(shrunk)}>
            {shrunk
              ? <div className="p-2 bg-white flex items-center justify-center p-2 m-2 rounded">
                  {isCourseAuthor
                    ? <img src=schoolIconPath alt=schoolName />
                    : <a className="text-xs" href="/school">
                        <img src=schoolIconPath alt=schoolName />
                      </a>}
                </div>
              : <img className="h-full object-contain" src=schoolLogoPath alt=schoolName />}
          </div>
        </div>
        {/* <div
             className="flex school-admin-navbar__school-search rounded justify-end w-1/2">
             <div
               className="school-admin-navbar__school-search__icon-box flex items-center justify-center border rounded w-16">
               <i className="fas fa-search" />
             </div>
           </div> */
        switch userRole {
        | SchoolAdmin =>
          <ul>
            <li>
              {topLink(selectedOption, Overview, "/school", shrunk, "fas fa-eye", "Overview")}
            </li>
            <li>
              {topLink(
                selectedOption,
                SchoolCoaches,
                "/school/coaches",
                shrunk,
                "fas fa-chalkboard-teacher",
                "Coaches",
              )}
            </li>
            <li>
              {topLink(
                selectedOption,
                Settings(Customization),
                "/school/customize",
                shrunk,
                "fas fa-cog",
                "Settings",
              )}
            </li>
            <li>
              {topLink(
                selectedOption,
                Courses,
                "/school/courses",
                shrunk,
                "fas fa-book",
                "Courses",
              )}
              {shrunk
                ? React.null
                : <ul className="pr-4 pb-4 ml-10 mt-1">
                    {courses
                    ->List.map(course =>
                      <li key={course->Course.id}>
                        <a
                          href={"/school/courses/" ++ ((course->Course.id) ++ "/curriculum")}
                          className="block text-white py-3 px-4 hover:bg-primary-800 rounded font-semibold text-xs">
                          {course->Course.name->str}
                        </a>
                      </li>
                    )
                    ->Array.of_list
                    ->React.array}
                  </ul>}
            </li>
            <li>
              {topLink(
                selectedOption,
                Communities,
                "/school/communities",
                shrunk,
                "fas fa-users",
                "Communities",
              )}
            </li>
          </ul>
        | CourseAuthor => React.null
        }}
      </div>
      <ul>
        {bottomLink("/home", shrunk, "fas fa-home", "Home")}
        <li>
          <a
            title=?{shrunk ? Some("Sign Out") : None}
            className={bottomLinkClasses(shrunk)}
            rel="nofollow"
            href="/users/sign_out">
            <i className="fas fa-sign-out-alt fa-fw" />
            {shrunk ? React.null : <span className="ml-2"> {"Sign Out"->str} </span>}
          </a>
        </li>
      </ul>
    </div>,
    selectedOption->secondaryNav(courses, userRole),
  ]->React.array
}
