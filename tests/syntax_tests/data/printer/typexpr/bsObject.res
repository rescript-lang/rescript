type user = {"age": int}
type user = {"age": int, "name": string}
type user = {"age": int, "name": string, "moreFields": veryVeryVeryLongtype, "superLongNameThat": whyDoesATypeWithThisAmountOfCharactersMakeItIntoThisFile, "because": testingLineBreaks}


type magic = {..}
type t = {.. "age": int}
type magicallyLong = {.."age": int, "name": string, "moreFields": veryVeryVeryLongtype, "superLongNameThat": whyDoesATypeWithThisAmountOfCharactersMakeItIntoThisFile, "because": testingLineBreaks}

external test: (foo, bar, baz) => {.."age": int, "name": string, "moreFields": veryVeryVeryLongtype, "superLongNameThat": whyDoesATypeWithThisAmountOfCharactersMakeItIntoThisFile, "because": testingLineBreaks} = "primitive"


// attributes
type user = @attr {"age": @attr int}
type magic = @attr {..}
type magic = @attr {.}
type t = @attr {.. "age": int}

external test: (foo, bar, baz) => @attr {.."age": int, "name": string, "moreFields": veryVeryVeryLongtype, "superLongNameThat": whyDoesATypeWithThisAmountOfCharactersMakeItIntoThisFile, "because": testingLineBreaks} = "primitive"

type t = {.}
type t = private {.}

type t = constr<{.}, {.}, {.}>
type t = {
  hr: React.component<{.}>
}
