let returned = 42
//  ^ref

let get = () => %return(returned)
//                        ^hov

let typed = () => %return(returned)
//  ^hov

let completion = () => {
  %return(returned)
  //         ^com
}
