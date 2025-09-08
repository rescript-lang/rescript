let y = `In winter the sky is never blue`
let x = `firstname: ${user.firstName} lastname: ${user.lastName}.`
let z = `${user.name} is his name`
let o = `His name is ${user.name}`

let s = `multi
  line

string
`

let s = `a \ b`
let s = `a \\ b`
let s = `a \\\ b`

let s = `${foo}`

let s = `before${foo}`
let s = `before ${foo}`
let s = `before  ${foo}`

let s = `${foo}after`
let s = `${foo} after`
let s = `${foo}  after`

let s = `${foo}${bar}`
let s = `${foo}${bar}${baz}`

let s = `${foo} ${bar}`
let s = `${foo} ${bar} ${baz}`

let s = ` before ${foo} ${bar} after `
let s = `before ${foo} middle ${bar} ${baz} wow `

let s = `
  multiline

  es6

  template

  expression

  so convenient

  :)
          :D
`

let s = `$dollar without $braces $interpolation`

let s = `${(s: string)}`

// don't sugar to one single template literal, keep the concatenation
`the sky` ++ `is blue`

`my ${language.name} is ` ++ `Bond, ${jamesbond.firstName}.`
`my ${language.name} is ` ++ `${jamesbond.lastName}. James Bond.`
(`my ${language.name} is ` ++ `Bond, ${jamesbond.firstName}.`)->Js.log
(`my ${name} is ` ++ `Bond`)->Js.log
`my ${kitchen.quality} kitched` ++ ` is ${language.big} ` ++ ` of the ${kitchen.things}.` 
json`null`->Js.log
a ++ ` x ` ++ b
a ++ (` x ` ++ b)

let x = json`null`

let x =  sql`select ${column} from ${table}`

module X = %graphql("
  query 123456789 {
    x {
      a
      b
      c
      d
      e
    }
  }
")

let cn = css`
  display: block;
  color: ${Color.text};
  background-color: ${Color.bg};
  border: 6px solid ${Color.Border.bg};
  margin: ${1}px;
  margin: ${1 + 2}px;
  margin: ${1 * 2}px;
  margin: ${1 + 2 / 3 + 4}px;
  margin: ${1 * 2 / 3 + 4}px;
  margin: ${1 - 2 / 3 * 4}px;
  margin: ${1 / 2 + 3 * 4}px;
  padding: ${Size.md}px;
  padding: ${1 + Size.md}px;
  padding: ${1.2 / Size.md}px;
  padding: ${Size.md + 1 - 2.3 * pad / 4}px;
`

let box = css`
  margin: ${ten()}px;
  padding: ${pad}px;
  border: 6px solid ${Color.Border.bg->Polished.lighten(0.3)};
  background-color: ${Color.bg};
  border-radius: ${Size.md / 2}px;
`

%graphql(`
  fragment ActivityBefore_ActivityCategory on ActivityCategory
  @argumentDefinitions(pixelRatio: {type: "Float!"}) {
    id
    title
    description
    icon(scaleFactor: $pixelRatio, width: 105, height: 100) {
      url
    }
    iconDarkMode(scaleFactor: $pixelRatio, width: 105, height: 100) {
      url
    }
  }
`)
