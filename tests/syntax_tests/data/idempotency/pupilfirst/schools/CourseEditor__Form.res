open CourseEditor__Types

let str = ReasonReact.string

type state = {
  name: string,
  description: string,
  endsAt: option<Js.Date.t>,
  hasNameError: bool,
  hasDescriptionError: bool,
  hasDateError: bool,
  about: string,
  publicSignup: bool,
  dirty: bool,
  saving: bool,
  featured: bool,
}

type action =
  | UpdateName(string, bool)
  | UpdateDescription(string, bool)
  | UpdateEndsAt(option<Js.Date.t>)
  | UpdateSaving
  | UpdateAbout(string)
  | UpdatePublicSignup(bool)
  | UpdateFeatured(bool)

let reducer = (state, action) =>
  switch action {
  | UpdateSaving => {...state, saving: !state.saving}
  | UpdateName(name, hasNameError) => {
      ...state,
      name: name,
      hasNameError: hasNameError,
      dirty: true,
    }
  | UpdateDescription(description, hasDescriptionError) => {
      ...state,
      description: description,
      hasDescriptionError: hasDescriptionError,
      dirty: true,
    }
  | UpdateEndsAt(date) => {...state, endsAt: date, dirty: true}
  | UpdatePublicSignup(publicSignup) => {...state, publicSignup: publicSignup, dirty: true}
  | UpdateAbout(about) => {...state, about: about, dirty: true}
  | UpdateFeatured(featured) => {...state, featured: featured, dirty: true}
  }

module CreateCourseQuery = %graphql(`
   mutation CreateCourseMutation($name: String!, $description: String!, $endsAt: Date, $about: String!,$publicSignup: Boolean!,$featured: Boolean!) {
     createCourse(name: $name, description: $description, endsAt: $endsAt, about: $about,publicSignup: $publicSignup,featured: $featured) {
       course {
         id
       }
     }
   }
   `)

module UpdateCourseQuery = %graphql(`
   mutation UpdateCourseMutation($id: ID!, $description: String!, $name: String!, $endsAt: Date, $about: String!, $publicSignup: Boolean!, $featured: Boolean!) {
    updateCourse(id: $id, name: $name, description: $description, endsAt: $endsAt, about: $about, publicSignup: $publicSignup, featured: $featured){
       course {
         id
       }
      }
   }
   `)

let updateName = (send, name) => {
  let hasError = name->String.trim->String.length < 2
  send(UpdateName(name, hasError))
}

let updateDescription = (send, description) => {
  let lengthOfDescription = description->String.trim->String.length
  let hasError = lengthOfDescription < 2 || lengthOfDescription >= 150
  send(UpdateDescription(description, hasError))
}

let saveDisabled = state =>
  state.hasDateError ||
  (state.hasDescriptionError ||
  (state.description == "" ||
    (state.hasNameError ||
    (state.name == "" || (!state.dirty || state.saving)))))

let formClasses = value =>
  value ? "drawer-right-form w-full opacity-50" : "drawer-right-form w-full"

let handleResponseCB = (id, state, updateCourseCB, course) => {
  let (thumbnail, cover) = switch course {
  | Some(c) => (c->Course.thumbnail, c->Course.cover)
  | None => (None, None)
  }

  let course = Course.create(
    ~id,
    ~name=state.name,
    ~description=state.description,
    ~endsAt=state.endsAt,
    ~about=Some(state.about),
    ~publicSignup=state.publicSignup,
    ~thumbnail,
    ~cover,
    ~featured=state.featured,
  )

  updateCourseCB(course)
}

let createCourse = (state, send, updateCourseCB) => {
  send(UpdateSaving)

  let createCourseQuery = CreateCourseQuery.make(
    ~name=state.name,
    ~description=state.description,
    ~endsAt=?state.endsAt->OptionUtils.map(Date.iso8601)->OptionUtils.map(Js.Json.string),
    ~about=state.about,
    ~publicSignup=state.publicSignup,
    ~featured=state.featured,
    (),
  )

  createCourseQuery
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(result => {
    handleResponseCB(result["createCourse"]["course"]["id"], state, updateCourseCB, None)
    Js.Promise.resolve()
  })
  ->ignore
}

let updateCourse = (state, send, updateCourseCB, course) => {
  send(UpdateSaving)

  let updateCourseQuery = UpdateCourseQuery.make(
    ~id=course->Course.id,
    ~name=state.name,
    ~description=state.description,
    ~endsAt=?state.endsAt->OptionUtils.map(Date.iso8601)->OptionUtils.map(Js.Json.string),
    ~about=state.about,
    ~publicSignup=state.publicSignup,
    ~featured=state.featured,
    (),
  )

  updateCourseQuery
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(result => {
    handleResponseCB(result["updateCourse"]["course"]["id"], state, updateCourseCB, Some(course))
    Js.Promise.resolve()
  })
  ->ignore
}

let booleanButtonClasses = bool => {
  let classes = "toggle-button__button"
  classes ++ (bool ? " toggle-button__button--active" : "")
}

let enablePublicSignupButton = (publicSignup, send) =>
  <div className="flex items-center mt-5">
    <label className="block tracking-wide text-xs font-semibold mr-6" htmlFor="public-signup">
      {"Enable public signup for this course?"->str}
    </label>
    <div
      id="public-signup"
      className="flex toggle-button__group flex-shrink-0 rounded-lg overflow-hidden">
      <button
        className={booleanButtonClasses(publicSignup)}
        onClick={_ => send(UpdatePublicSignup(true))}>
        {"Yes"->str}
      </button>
      <button
        className={booleanButtonClasses(!publicSignup)}
        onClick={_ => send(UpdatePublicSignup(false))}>
        {"No"->str}
      </button>
    </div>
  </div>

let featuredButton = (featured, send) =>
  <div className="flex items-center mt-5">
    <label className="block tracking-wide text-xs font-semibold mr-6" htmlFor="featured">
      {"Feature course in school homepage?"->str}
    </label>
    <div
      id="featured" className="flex toggle-button__group flex-shrink-0 rounded-lg overflow-hidden">
      <button className={booleanButtonClasses(featured)} onClick={_ => send(UpdateFeatured(true))}>
        {"Yes"->str}
      </button>
      <button
        className={booleanButtonClasses(!featured)} onClick={_ => send(UpdateFeatured(false))}>
        {"No"->str}
      </button>
    </div>
  </div>

let about = course =>
  switch course->Course.about {
  | Some(about) => about
  | None => ""
  }

let updateAboutCB = (send, about) => send(UpdateAbout(about))

let computeInitialState = course =>
  switch course {
  | Some(course) => {
      name: course->Course.name,
      description: course->Course.description,
      endsAt: course->Course.endsAt,
      hasNameError: false,
      hasDateError: false,
      hasDescriptionError: false,
      dirty: false,
      saving: false,
      about: about(course),
      publicSignup: course->Course.publicSignup,
      featured: course->Course.featured,
    }
  | None => {
      name: "",
      description: "",
      endsAt: None,
      hasNameError: false,
      hasDateError: false,
      hasDescriptionError: false,
      dirty: false,
      saving: false,
      about: "",
      publicSignup: false,
      featured: true,
    }
  }

@react.component
let make = (~course, ~hideEditorActionCB, ~updateCourseCB) => {
  let (state, send) = React.useReducerWithMapState(reducer, course, computeInitialState)
  <div>
    <div className="blanket" />
    <div className="drawer-right">
      <div className="drawer-right__close absolute">
        <button
          title="close"
          onClick={_ => hideEditorActionCB()}
          className="flex items-center justify-center bg-white text-gray-600 font-bold py-3 px-5 rounded-l-full rounded-r-none hover:text-gray-700 focus:outline-none mt-4">
          <i className="fas fa-times text-xl" />
        </button>
      </div>
      <div className={formClasses(state.saving)}>
        <div className="w-full">
          <div className="mx-auto bg-white">
            <div className="max-w-2xl p-6 mx-auto">
              <h5 className="uppercase text-center border-b border-gray-400 pb-2">
                {(course == None ? "Add New Course" : "Edit Course Details")->str}
              </h5>
              <div className="mt-5">
                <label className="inline-block tracking-wide text-xs font-semibold " htmlFor="name">
                  {"Course name"->str}
                </label>
                <input
                  className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                  id="name"
                  type_="text"
                  placeholder="Type course name here"
                  maxLength=50
                  value=state.name
                  onChange={event => updateName(send, ReactEvent.Form.target(event)["value"])}
                />
                <School__InputGroupError
                  message="A name is required (2-50 characters)" active=state.hasNameError
                />
              </div>
              <div className="mt-5">
                <label
                  className="inline-block tracking-wide text-xs font-semibold"
                  htmlFor="description">
                  {"Course description"->str}
                </label>
                <input
                  className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 mt-2 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                  id="description"
                  type_="text"
                  placeholder="Short description for this course"
                  value=state.description
                  maxLength=150
                  onChange={event =>
                    updateDescription(send, ReactEvent.Form.target(event)["value"])}
                />
              </div>
              <School__InputGroupError
                message="A description is required (2-150 characters)"
                active=state.hasDescriptionError
              />
              <div className="mt-5">
                <label
                  className="tracking-wide text-xs font-semibold" htmlFor="course-ends-at-input">
                  {"Course end date"->str}
                </label>
                <span className="ml-1 text-xs"> {"(optional)"->str} </span>
                <HelpIcon className="ml-2" link="https://docs.pupilfirst.com/#/courses">
                  {"If specified, course will appear as closed to students on this date. Students will not be able to make any more submissions."->str}
                </HelpIcon>
                <DatePicker
                  onChange={date => send(UpdateEndsAt(date))}
                  selected=?state.endsAt
                  id="course-ends-at-input"
                />
              </div>
              <School__InputGroupError message="Enter a valid date" active=state.hasDateError />
              <div id="About" className="mt-5">
                <MarkdownEditor
                  onChange={updateAboutCB(send)}
                  value=state.about
                  placeholder="Add more details about the course."
                  profile=Markdown.Permissive
                  maxLength=10000
                />
              </div>
              {featuredButton(state.featured, send)}
              {enablePublicSignupButton(state.publicSignup, send)}
            </div>
          </div>
          <div className="mx-auto">
            <div className="max-w-2xl p-6 mx-auto">
              <div className="flex">
                {switch course {
                | Some(course) =>
                  <button
                    disabled={saveDisabled(state)}
                    onClick={_ => updateCourse(state, send, updateCourseCB, course)}
                    className="w-full btn btn-large btn-primary mt-3">
                    {"Update Course"->str}
                  </button>

                | None =>
                  <button
                    disabled={saveDisabled(state)}
                    onClick={_ => createCourse(state, send, updateCourseCB)}
                    className="w-full btn btn-large btn-primary mt-3">
                    {"Create Course"->str}
                  </button>
                }}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
}
