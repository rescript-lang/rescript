// Invalid escapes in attribute payloads must not crash the analysis.
@res.doc(`\xZZ`)
let a = 1
//  ^xfm

@module(`\xZZ`) external x: int = "x"
//         ^com

@module({from: `\xZZ`}) external y: int = "y"
//                ^com
