let str = React.string

let reasonCode = `
\`\`\`reason
/* A sample fizzbuzz implementation. */
let fizzbuzz = (num) =>
  switch (num mod 3, num mod 5) {
  | (0, 0) => "FizzBuzz"
  | (0, _) => "Fizz"
  | (_, 0) => "Buzz"
  | _ => string_of_int(num)
  };

for (i in 1 to 100) {
  Js.log(fizzbuzz(i))
};
\`\`\``

let rubyCode = `\`\`\`ruby
# A sample fizzbuzz implementation.
def fizzbuzz(num)
  a = String.new
  a << "Fizz" if num%3 == 0
  a << "Buzz" if num%5 == 0
  a << num.to_s if a.empty?
  a
end

(1..100).each do |i|
  puts fizzbuzz(i)
end
\`\`\``

let jsCode = `\`\`\`js
// A sample fizzbuzz implementation.
const fizzbuzz = (num) => {
  if (i % (15) === 0){
    return('fizzbuzz');
  } else if (i % 3 === 0){
    return('fizz');
  } else if (i % 5 === 0){
    return('buzz');
  } else {
    return(i);
  }
};

for(var i = 1; i <= 100; i++) {
  console.log(fizzbuzz(i));
}
\`\`\``

let htmlCode = `\`\`\`html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Fizzbuzz Demo</title>
  </head>

  <body>
    <!-- Display for a JS implementation -->
  </body>
</html>
`

let scssCode = `\`\`\`scss
/* Fizzbuzz with SCSS! */

@import "compass/css3";

ul {
  list-style-type:none;
}

li:nth-child(3n), li:nth-child(5n){
  font-size:0px;
}

li:nth-child(3n):before{
  font-size:16px;
  content:"Fizz";
}

li:nth-child(5n):after{
  font-size:16px;
  content:"Buzz";
}
`

let cssCode = `\`\`\`css
/* Fizzbuzz with CSS! */

.fizz-buzz {
  counter-reset: fizzbuzz;
}

.fizz-buzz > div::before {
  content: counter(fizzbuzz);
  counter-increment: fizzbuzz;
}

.fizz-buzz > div:nth-of-type(3n+3)::before {
  content: "Fizz";
}

.fizz-buzz > div:nth-of-type(5n+5)::before {
  content: "Buzz";
}

.fizz-buzz > div:nth-of-type(3n+3):nth-of-type(5n+5)::before {
  content: "FizzBuzz";
}
`

type language =
  | ReasonML
  | Ruby
  | Javascript
  | Html
  | Css
  | Scss

let renderedMarkdown = language => {
  let (title, markdown) = switch language {
  | ReasonML => ("ReasonML", reasonCode)
  | Ruby => ("Ruby", rubyCode)
  | Javascript => ("Javascript", jsCode)
  | Html => ("HTML", htmlCode)
  | Css => ("CSS", cssCode)
  | Scss => ("SCSS", scssCode)
  }

  <div className="mt-4">
    <p className="text-xs font-semibold"> {title->str} </p>
    <MarkdownBlock markdown className="mt-2" profile=Markdown.Permissive />
  </div>
}

let handleChange = (setLanguage, event) => {
  let language = switch ReactEvent.Form.target(event)["value"] {
  | "reasonml" => ReasonML
  | "ruby" => Ruby
  | "javascript" => Javascript
  | "html" => Html
  | "css" => Css
  | "scss" => Scss
  | _ => ReasonML
  }

  setLanguage(_ => language)
}

@react.component
let make = () => {
  let (language, setLanguage) = React.useState(() => ReasonML)

  <div>
    <span> {"Select a language to preview:"->str} </span>
    <select className="ml-2" onChange={handleChange(setLanguage)}>
      <option value="reasonml"> {"ReasonML"->str} </option>
      <option value="ruby"> {"Ruby"->str} </option>
      <option value="javascript"> {"Javascript"->str} </option>
      <option value="html"> {"HTML"->str} </option>
      <option value="css"> {"CSS"->str} </option>
      <option value="scss"> {"SCSS"->str} </option>
    </select>
    {renderedMarkdown(language)}
  </div>
}
