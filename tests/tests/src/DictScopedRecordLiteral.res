module Hidden = {
  type routeHandlerObject = {get: int}
}

let dictValueInference: Dict.t<Hidden.routeHandlerObject> = dict{
  "health": {get: 200},
}

let primitiveMakeValueInference: Dict.t<Hidden.routeHandlerObject> = dict{"health": {get: 200}}
