module SomeComplicatedModuleStructure = {
  module NestedModuleHere = {
    type t = string
  }
}

module Component = {
  type props<'name, 'second, 'third, 'fourth, 'fifth, 'sixth, 'seventh, 'eight, 'ninth> = {
    name: 'name,
    second: 'second,
    third: 'third,
    fourth: 'fourth,
    fifth: 'fifth,
    sixth: 'sixth,
    seventh: 'seventh,
    eight: 'eight,
    ninth: 'ninth,
  }
  let make = props => {
    props.name ++
    props.second ++
    props.third ++
    props.fourth ++
    props.fifth ++
    props.sixth ++
    props.seventh ++
    props.eight ++
    props.ninth
  }
}

let dddd = Component.make({nonExistant: "hello"})
