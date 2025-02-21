type timeoutId

@val external setTimeout: (unit => unit, int) => timeoutId = "setTimeout"
@val external setTimeoutFloat: (unit => unit, float) => timeoutId = "setTimeout"
@val external clearTimeout: timeoutId => unit = "clearTimeout"

type intervalId

@val external setInterval: (unit => unit, int) => intervalId = "setInterval"
@val external setIntervalFloat: (unit => unit, float) => intervalId = "setInterval"
@val external clearInterval: intervalId => unit = "clearInterval"

@val external encodeURI: string => string = "encodeURI"
@val external decodeURI: string => string = "decodeURI"

@val external encodeURIComponent: string => string = "encodeURIComponent"
@val external decodeURIComponent: string => string = "decodeURIComponent"
