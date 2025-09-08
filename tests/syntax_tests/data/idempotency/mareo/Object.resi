open Actors

open Particle

let invuln: int /* # of frames of invulnerability */

let dampen_jump: float /* Boost to jump when enemy jumped on */

type aabb = {
  center: xy,
  half: xy,
}

type obj_params = {
  has_gravity: bool,
  speed: float,
}

type obj = {
  params: obj_params,
  pos: xy,
  vel: xy,
  id: int,
  mutable jumping: bool,
  mutable grounded: bool,
  mutable dir: Actors.dir_1d,
  mutable invuln: int,
  mutable kill: bool,
  mutable health: int,
  mutable crouch: bool,
  mutable score: int,
}

type collidable =
  | Player(pl_typ, Sprite.sprite, obj)
  | Enemy(enemy_typ, Sprite.sprite, obj)
  | Item(item_typ, Sprite.sprite, obj)
  | Block(block_typ, Sprite.sprite, obj)

/* Returns the sprite associated with the object */
let get_sprite: collidable => Sprite.sprite

let get_obj: collidable => obj

/* Creates a new object with a given
 * actor type on the the canvas at a given position */
let spawn: (Actors.spawn_typ, Dom_html.canvasRenderingContext2D, (float, float)) => collidable

let equals: (collidable, collidable) => bool

let is_player: collidable => bool

let is_enemy: collidable => bool

let normalize_origin: (xy, Sprite.sprite) => unit

let normalize_pos: (xy, Sprite.sprite_params, Sprite.sprite_params) => unit

/* Destroys the object, returning a list of destruction effect objects */
let kill: (collidable, Dom_html.canvasRenderingContext2D) => list<particle>

let process_obj: (obj, float) => unit

let update_player: (
  obj,
  list<Actors.controls>,
  Dom_html.canvasRenderingContext2D,
) => option<(pl_typ, Sprite.sprite)>

/* Checks whether a collision occured between two objects, returning the
 * direction of the collision if one occurred. */
let check_collision: (collidable, collidable) => option<Actors.dir_2d>

let evolve_enemy: (
  Actors.dir_1d,
  Actors.enemy_typ,
  Sprite.sprite,
  obj,
  Dom_html.canvasRenderingContext2D,
) => option<collidable>

let evolve_block: (obj, Dom_html.canvasRenderingContext2D) => collidable

let dec_health: obj => unit

let rev_dir: (obj, Actors.enemy_typ, Sprite.sprite) => unit

let reverse_left_right: obj => unit

let collide_block: (~check_x: bool=?, Actors.dir_2d, obj) => unit

let spawn_above: (
  Actors.dir_1d,
  obj,
  Actors.item_typ,
  Dom_html.canvasRenderingContext2D,
) => collidable
