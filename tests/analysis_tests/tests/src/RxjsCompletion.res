@@warning("-26")
@@warning("-110")

type keyPress =
  | Up(string)
  | Down(string)

@val
external window: {..} = "window"

let main = async () => {
  let keyMapObservable = {
    open Rxjs

    let keydown =
      fromEvent(Obj.magic(window), "keydown")->pipe2(
        map(event => Down(event["key"])),
        distinctUntilChanged(),
      )

    let keyup =
      fromEvent(Obj.magic(window), "keyup")->pipe2(
        map(event => Up(event["key"])),
        distinctUntilChanged(),
      )

    // merge(keydown, keyup).
    //                       ^com

    // Rxjs.Observable.subscribe, Rxjs.pipe and Rxjs.pipe2 should be completed
  }

  let (a,b) : ( Rxjs.Observable.t<string> , Rxjs.Observable.t<string>) = %todo

  // Rxjs.combineLatest(a, b).
  //                          ^com

  // Rxjs.Observable.subscribe, Rxjs.pipe and Rxjs.pipe2 should be completed
}
