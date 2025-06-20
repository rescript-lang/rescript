module Image = {
  type t = {
    url: string,
    filename: string,
  }

  let url = t => t.url
  let filename = t => t.filename

  let make = (url, filename) => {url: url, filename: filename}
}

type t = {
  id: string,
  name: string,
  description: string,
  endsAt: option<Js.Date.t>,
  about: option<string>,
  publicSignup: bool,
  thumbnail: option<Image.t>,
  cover: option<Image.t>,
  featured: bool,
}

let name = t => t.name

let id = t => t.id

let endsAt = t => t.endsAt

let about = t => t.about

let publicSignup = t => t.publicSignup

let description = t => t.description

let featured = t => t.featured

let cover = t => t.cover

let thumbnail = t => t.thumbnail

let imageUrl = image => image->Image.url

let filename = image => image->Image.filename

let sort = courses =>
  courses->List.sort((x, y) => (x.id->int_of_string) - (y.id->int_of_string))

let updateList = (courses, course) => {
  let oldCourses = courses->List.filter(c => c.id !== course.id)
  oldCourses->List.rev->List.append(list{course})->List.rev
}

let makeImageFromJs = data =>
  switch data {
  | Some(image) => Some(Image.make(image["url"], image["filename"]))
  | None => None
  }

let addImages = (~coverUrl, ~thumbnailUrl, ~coverFilename, ~thumbnailFilename, t) => {
  ...t,
  cover: switch coverUrl {
  | Some(coverUrl) => Some(Image.make(coverUrl, coverFilename))
  | None => None
  },
  thumbnail: switch thumbnailUrl {
  | Some(thumbnailUrl) => Some(Image.make(thumbnailUrl, thumbnailFilename))
  | None => None
  },
}

let create = (
  ~id,
  ~name,
  ~description,
  ~endsAt,
  ~about,
  ~publicSignup,
  ~cover,
  ~thumbnail,
  ~featured,
) => {
  id: id,
  name: name,
  description: description,
  endsAt: endsAt,
  about: about,
  publicSignup: publicSignup,
  cover: cover,
  thumbnail: thumbnail,
  featured: featured,
}

let replaceImages = (cover, thumbnail, t) => {...t, cover: cover, thumbnail: thumbnail}

let makeFromJs = rawCourse => {
  let endsAt = switch rawCourse["endsAt"] {
  | Some(endsAt) => Some(endsAt->Json.Decode.string)->OptionUtils.map(DateFns.parseString)
  | None => None
  }
  create(
    ~id=rawCourse["id"],
    ~name=rawCourse["name"],
    ~description=rawCourse["description"],
    ~endsAt,
    ~about=rawCourse["about"],
    ~publicSignup=rawCourse["publicSignup"],
    ~thumbnail=makeImageFromJs(rawCourse["thumbnail"]),
    ~cover=makeImageFromJs(rawCourse["cover"]),
    ~featured=rawCourse["featured"],
  )
}
