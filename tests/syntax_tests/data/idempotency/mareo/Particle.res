open Actors

type part_params = {
  sprite: Sprite.sprite,
  rot: float,
  lifetime: int,
}

type particle = {
  params: part_params,
  part_type: Actors.part_typ,
  pos: Actors.xy,
  vel: Actors.xy,
  acc: Actors.xy,
  mutable kill: bool,
  mutable life: int,
}

/* Converts an x,y [pair] to an Actors.xy record */
let pair_to_xy = pair => {x: fst(pair), y: snd(pair)}

/* Function wrapper to assist in generating the template paramss for a
 * particle. */
let make_params = (sprite, rot, lifetime) => {sprite: sprite, rot: rot, lifetime: lifetime}

/* Generate the template for a specific particle type */
let make_type = (typ, ctx) =>
  switch typ {
  | GoombaSquish as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  | BrickChunkL as t => make_params(Sprite.make_particle(t, ctx), 0., 300)
  | BrickChunkR as t => make_params(Sprite.make_particle(t, ctx), 0., 300)
  | Score100 as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  | Score200 as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  | Score400 as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  | Score800 as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  | Score1000 as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  | Score2000 as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  | Score4000 as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  | Score8000 as t => make_params(Sprite.make_particle(t, ctx), 0., 30)
  }

let make = (~vel=(0., 0.), ~acc=(0., 0.), part_type, pos, ctx) => {
  let params = make_type(part_type, ctx)
  let pos = pair_to_xy(pos)
  and vel = pair_to_xy(vel)
  and acc = pair_to_xy(acc)
  {
    params: params,
    part_type: part_type,
    pos: pos,
    vel: vel,
    acc: acc,
    kill: false,
    life: params.lifetime,
  }
}

let make_score = (score, pos, ctx) => {
  let t = switch score {
  | 100 => Score100
  | 200 => Score200
  | 400 => Score400
  | 800 => Score800
  | 1000 => Score1000
  | 2000 => Score2000
  | 4000 => Score4000
  | 8000 => Score8000
  | _ => Score100
  }
  make(~vel=(0.5, -0.7), t, pos, ctx)
}

/* Mutably update the velocity of a particle */
let update_vel = part => {
  part.vel.x = part.vel.x +. part.acc.x
  part.vel.y = part.vel.y +. part.acc.y
}

/* Mutably update the position of a particle */
let update_pos = part => {
  part.pos.x = part.vel.x +. part.pos.x
  part.pos.y = part.vel.y +. part.pos.y
}

let process = part => {
  part.life = part.life - 1
  if part.life == 0 {
    part.kill = true
  }
  update_vel(part)
  update_pos(part)
}
