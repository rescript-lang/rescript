let test = (value, url) => {
  let tester = Js.Re.fromString(value)
  url->Js.Re.test_(tester)
}

let iconClass = url =>
  switch url {
  | url if url->test("twitter") => "fab fa-twitter"
  | url if url->test("facebook") => "fab fa-facebook-f"
  | url if url->test("instagram") => "fab fa-instagram"
  | url if url->test("youtube") => "fab fa-youtube"
  | url if url->test("linkedin") => "fab fa-linkedin"
  | url if url->test("snapchat") => "fab fa-snapchat"
  | url if url->test("tumblr") => "fab fa-tumblr"
  | url if url->test("pinterest") => "fab fa-pinterest"
  | url if url->test("reddit") => "fab fa-reddit"
  | url if url->test("flickr") => "fab fa-flickr"
  | _unknownUrl => "fas fa-users"
  }

@react.component
let make = (~url) =>
  <div className="h-8 w-8 mr-2 mt-2 flex items-center justify-center">
    <i className={"text-xl " ++ iconClass(url)} />
  </div>
