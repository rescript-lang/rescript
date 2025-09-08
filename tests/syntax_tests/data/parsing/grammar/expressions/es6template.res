let s = `foo`

let s = `multi
  line

string
`

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
`

let s = `$dollar without $braces $interpolation`

let s = json`null`

let x = `foo\`bar\$\\foo`
let x = `foo\`bar\$\\foo${a} \` ${b} \` xx`

let thisIsFine = `$something`
let thisIsAlsoFine = `fine\$`
let isThisFine = `shouldBeFine$`

`$${dollarAmountInt}`
`\$${dollarAmountInt}`
