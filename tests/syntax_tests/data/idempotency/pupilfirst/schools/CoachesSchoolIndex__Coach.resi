type t

let decode: Js.Json.t => t

let name: t => string

let id: t => int

let imageUrl: t => string

let email: t => string

let title: t => string

let linkedinUrl: t => option<string>

let public: t => bool

let connectLink: t => option<string>

let exited: t => bool

let updateList: (list<t>, t) => list<t>

let affiliation: t => option<string>

let imageFileName: t => option<string>

let make: (
  ~id: int,
  ~name: string,
  ~imageUrl: string,
  ~email: string,
  ~title: string,
  ~linkedinUrl: option<string>,
  ~public: bool,
  ~connectLink: option<string>,
  ~exited: bool,
  ~imageFileName: option<string>,
  ~affiliation: option<string>,
) => t
