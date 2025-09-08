module SectionHeader = {
  @react.component
  let make = (~children) => children
}


let z1 = <> <SectionHeader> {React.string("abc")} </SectionHeader> </>
//                 ^hov

let z2 = <> <SectionHeader> {React.string("abc")} </SectionHeader> </>
//                                                      ^hov