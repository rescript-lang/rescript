type t = {
  requireEmitterEarly: list<string>,
  exportEmitterEarly: list<string>,
  requireEmitter: list<string>,
  importEmitter: list<string>,
  exportEmitter: list<string>,
}

let initial = {
  requireEmitterEarly: list{},
  exportEmitterEarly: list{},
  requireEmitter: list{},
  importEmitter: list{},
  exportEmitter: list{},
}

let string = (~emitter, s) => list{s, ...emitter}

let requireEarly = (~emitters, s) => {
  ...emitters,
  requireEmitterEarly: s->string(~emitter=emitters.requireEmitterEarly),
}

let exportEarly = (~emitters, s) => {
  ...emitters,
  exportEmitterEarly: s->string(~emitter=emitters.exportEmitterEarly),
}

let require = (~emitters, s) => {
  ...emitters,
  requireEmitter: s->string(~emitter=emitters.requireEmitter),
}
let \"import" = (~emitters, s) => {
  ...emitters,
  importEmitter: s->string(~emitter=emitters.importEmitter),
}

let \"export" = (~emitters, s) => {
  ...emitters,
  exportEmitter: s->string(~emitter=emitters.exportEmitter),
}

let toString = (~separator, emitters) =>
  list{
    emitters.requireEmitterEarly->List.rev,
    emitters.exportEmitterEarly->List.rev,
    emitters.requireEmitter->List.rev,
    emitters.importEmitter->List.rev,
    emitters.exportEmitter->List.rev,
  }
  ->List.concat
  ->String.concat(separator)
