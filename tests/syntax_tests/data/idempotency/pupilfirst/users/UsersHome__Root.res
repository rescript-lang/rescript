%raw(`require("courses/shared/background_patterns.css")`)
%raw(`require("./UserHome__Root.css")`)

open UsersHome__Types

let str = React.string

type view =
  | ShowCourses
  | ShowCommunities

let headerSectiom = (userName, userTitle, avatarUrl, showUserEdit) =>
  <div className="max-w-4xl mx-auto pt-12 flex items-center justify-between px-3 lg:px-0">
    <div className="flex">
      {switch avatarUrl {
      | Some(src) =>
        <img
          className="w-16 h-16 rounded-full border object-cover border-gray-400 rounded-full overflow-hidden flex-shrink-0 mr-4"
          src
        />
      | None =>
        <Avatar
          name=userName
          className="w-16 h-16 mr-4 border border-gray-400 rounded-full overflow-hidden flex-shrink-0"
        />
      }}
      <div className="text-sm flex flex-col justify-center">
        <div className="text-black font-bold inline-block"> {userName->str} </div>
        <div className="text-gray-600 inline-block"> {userTitle->str} </div>
      </div>
    </div>
    {showUserEdit
      ? <a className="btn" href="/user/edit">
          <i className="fas fa-edit text-xs md:text-sm mr-2" />
          <span> {"Edit Profile"->str} </span>
        </a>
      : React.null}
  </div>

let navButtonClasses = selected =>
  "font-semibold border-b-2 border-transparent text-sm py-4 mr-6 focus:outline-none " ++ (
    selected ? "text-primary-500 border-primary-500" : ""
  )

let navSection = (view, setView, communities) =>
  <div className="border-b mt-6">
    <div className="flex max-w-4xl mx-auto px-3 lg:px-0">
      <button
        className={navButtonClasses(view == ShowCourses)} onClick={_ => setView(_ => ShowCourses)}>
        <i className="fas fa-book text-xs md:text-sm mr-2" /> <span> {"My Courses"->str} </span>
      </button>
      {communities->ArrayUtils.isNotEmpty
        ? <button
            className={navButtonClasses(view == ShowCommunities)}
            onClick={_ => setView(_ => ShowCommunities)}>
            <i className="fas fa-users text-xs md:text-sm mr-2" />
            <span> {"Communities"->str} </span>
          </button>
        : React.null}
    </div>
  </div>

let courseLink = (href, title, icon) =>
  <a
    key=href
    href
    className="px-2 py-1 mr-2 mt-2 rounded text-sm bg-gray-100 text-gray-800 hover:bg-gray-200 hover:text-primary-500">
    <i className=icon /> <span className="font-semibold ml-2"> {title->str} </span>
  </a>

let ctaButton = (title, href) =>
  <a
    href
    className="w-full bg-gray-200 mt-4 px-6 py-4 flex text-sm font-semibold justify-between items-center cursor-pointer text-primary-500 hover:bg-gray-300">
    <span> <i className="fas fa-book" /> <span className="ml-2"> {title->str} </span> </span>
    <i className="fas fa-arrow-right" />
  </a>

let ctaText = (message, icon) =>
  <div
    className="w-full bg-red-100 text-red-600 mt-4 px-6 py-4 flex text-sm font-semibold justify-center items-center ">
    <span> <i className=icon /> <span className="ml-2"> {message->str} </span> </span>
  </div>

let studentLink = (courseId, suffix) => "/courses/" ++ (courseId ++ ("/" ++ suffix))

let callToAction = (course, currentSchoolAdmin) =>
  if currentSchoolAdmin {
    #ViewCourse
  } else if course->Course.author {
    #EditCourse
  } else if course->Course.review {
    #ReviewSubmissions
  } else if course->Course.exited {
    #DroppedOut
  } else if course->Course.ended {
    #CourseEnded
  } else {
    #ViewCourse
  }

let ctaFooter = (course, currentSchoolAdmin) => {
  let courseId = course->Course.id

  switch callToAction(course, currentSchoolAdmin) {
  | #ViewCourse => ctaButton("View Course", studentLink(courseId, "curriculum"))
  | #EditCourse => ctaButton("Edit Curriculum", "/school/courses/" ++ (courseId ++ "/curriculum"))
  | #ReviewSubmissions => ctaButton("Review Submissions", studentLink(courseId, "review"))
  | #DroppedOut => ctaText("Dropped out", "fas fa-user-slash")
  | #CourseEnded => ctaText("Course Ended", "fas fa-history")
  }
}

let communityLinks = (communityIds, communities) =>
  communityIds
  ->Array.map(id => {
    let community = communities->Js.Array.find(c => c->Community.id == id)
    switch community {
    | Some(c) =>
      <a
        key={c->Community.id}
        href={"/communities/" ++ (c->Community.id)}
        className="px-2 py-1 mr-2 mt-2 rounded text-sm bg-gray-100 text-gray-800 hover:bg-gray-200 hover:text-primary-500">
        <i className="fas fa-users" />
        <span className="font-semibold ml-2"> {c->Community.name->str} </span>
      </a>
    | None => React.null
    }
  })
  ->React.array

let courseLinks = (course, currentSchoolAdmin, communities) => {
  let courseId = course->Course.id
  let cta = callToAction(course, currentSchoolAdmin)

  <div className="flex flex-wrap px-4 mt-2">
    {course->Course.author && cta != #EditCourse
      ? courseLink(
          "/school/courses/" ++ (courseId ++ "/curriculum"),
          "Edit Curriculum",
          "fas fa-check-square",
        )
      : React.null}
    {cta != #ViewCourse
      ? courseLink(studentLink(courseId, "curriculum"), "View Curriculum", "fas fa-book")
      : React.null}
    {course->Course.enableLeaderboard
      ? courseLink(studentLink(courseId, "leaderboard"), "Leaderboard", "fas fa-calendar-alt")
      : React.null}
    {course->Course.review && cta != #ReviewSubmissions
      ? courseLink(studentLink(courseId, "review"), "Review Submissions", "fas fa-check-square")
      : React.null}
    {course->Course.review
      ? courseLink(studentLink(courseId, "students"), "My Students", "fas fa-user-friends")
      : React.null}
    {communityLinks(course->Course.linkedCommunities, communities)}
  </div>
}

let coursesSection = (courses, communities, currentSchoolAdmin) =>
  <div className="w-full max-w-4xl mx-auto">
    <div className="flex flex-wrap flex-1 lg:-mx-5">
      {courses
      ->Array.map(course =>
        <div
          key={course->Course.id}
          ariaLabel={course->Course.name}
          className="w-full px-3 lg:px-5 md:w-1/2 mt-6 md:mt-10">
          <div
            key={course->Course.id}
            className="flex overflow-hidden shadow bg-white rounded-lg flex flex-col justify-between h-full">
            <div>
              <div className="relative">
                <div className="relative pb-1/2 bg-gray-800">
                  {switch course->Course.thumbnailUrl {
                  | Some(url) => <img className="absolute h-full w-full object-cover" src=url />
                  | None =>
                    <div
                      className="user-home-course__cover absolute h-full w-full svg-bg-pattern-1"
                    />
                  }}
                </div>
                <div
                  className="user-home-course__title-container absolute w-full flex items-center h-16 bottom-0 z-50"
                  key={course->Course.id}>
                  <h4
                    className="user-home-course__title text-white font-semibold leading-tight pl-6 pr-4 text-lg md:text-xl">
                    {course->Course.name->str}
                  </h4>
                </div>
              </div>
              <div
                className="user-home-course__description text-sm px-6 pt-4 w-full leading-relaxed">
                {course->Course.description->str}
              </div>
              {if course->Course.exited {
                <div className="text-sm py-4 bg-red-100 rounded mt-2 px-6">
                  {"Your student profile for this course is locked, and cannot be updated."->str}
                </div>
              } else {
                <div> {courseLinks(course, currentSchoolAdmin, communities)} </div>
              }}
            </div>
            <div> {ctaFooter(course, currentSchoolAdmin)} </div>
          </div>
        </div>
      )
      ->React.array}
    </div>
  </div>

let communitiesSection = communities =>
  <div className="w-full max-w-4xl mx-auto">
    <div className="flex flex-wrap flex-1 lg:-mx-5">
      {communities
      ->Array.map(community =>
        <div
          key={community->Community.id}
          className="flex w-full px-3 lg:px-5 md:w-1/2 mt-6 md:mt-10">
          <a
            className="w-full h-full shadow rounded-lg hover:shadow-lg"
            href={"communities/" ++ (community->Community.id)}>
            <div
              className="user-home-community__cover flex w-full bg-gray-600 h-40 svg-bg-pattern-5 items-center justify-center p-4 shadow rounded-t-lg"
            />
            <div className="w-full flex justify-between items-center flex-wrap px-4 pt-2 pb-4">
              <h4 className="font-bold text-sm pt-2 leading-tight">
                {community->Community.name->str}
              </h4>
              <div className="btn btn-small btn-primary-ghost mt-2">
                {"Visit Community"->str}
              </div>
            </div>
          </a>
        </div>
      )
      ->React.array}
    </div>
  </div>

@react.component
let make = (
  ~currentSchoolAdmin,
  ~courses,
  ~communities,
  ~showUserEdit,
  ~userName,
  ~userTitle,
  ~avatarUrl,
) => {
  let (view, setView) = React.useState(() => ShowCourses)
  <div className="bg-gray-100">
    <div className="bg-white">
      {headerSectiom(userName, userTitle, avatarUrl, showUserEdit)}
      {navSection(view, setView, communities)}
    </div>
    <div className="pb-8">
      {switch view {
      | ShowCourses => coursesSection(courses, communities, currentSchoolAdmin)
      | ShowCommunities => communitiesSection(communities)
      }}
    </div>
  </div>
}
