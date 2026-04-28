@@config({flags: ["-w", "a", "-bs-no-bin-annot"]})

open Belt

module Random = {
  let self_init = () => ()

  let int: int => int = %raw(`(max) => {
    return ((Math.random() * 100) | 0) % max;
  }`)
}

module Actors: {
  type dir_1d = Left | Right
  type dir_2d = North | South | East | West

  /* Generic xy record for easy position access */
  type xy = {
    mutable x: float,
    mutable y: float,
  }

  /* Controls correspond to keyboard input */
  type controls =
    | CLeft
    | CRight
    | CUp
    | CDown

  /* Player ability type */
  type pl_typ =
    | BigM
    | SmallM

  type item_typ =
    | Mushroom
    | FireFlower
    | Star
    | Coin

  type enemy_typ =
    | Goomba
    | GKoopa
    | RKoopa
    | GKoopaShell
    | RKoopaShell

  type block_typ =
    | QBlock(item_typ)
    | QBlockUsed
    | Brick
    | UnBBlock
    | Cloud
    | Panel
    | Ground

  /* Player action type */
  type player_typ =
    | Standing
    | Jumping
    | Running
    | Crouching

  /* Particle Type */
  type part_typ =
    | GoombaSquish
    | BrickChunkL
    | BrickChunkR
    | Score100
    | Score200
    | Score400
    | Score800
    | Score1000
    | Score2000
    | Score4000
    | Score8000

  /* type unbblock_typ =
  | Wood
  | Earth
  | Brick
| */

  type spawn_typ =
    | SPlayer(pl_typ, player_typ)
    | SEnemy(enemy_typ)
    | SItem(item_typ)
    | SBlock(block_typ)
  /* | SGround of ground_typ */
} = {
  type dir_1d = Left | Right
  type dir_2d = North | South | East | West

  type xy = {
    mutable x: float,
    mutable y: float,
  }

  type controls =
    | CLeft
    | CRight
    | CUp
    | CDown

  type pl_typ =
    | BigM
    | SmallM

  type item_typ =
    | Mushroom
    | FireFlower
    | Star
    | Coin

  type enemy_typ =
    | Goomba
    | GKoopa
    | RKoopa
    | GKoopaShell
    | RKoopaShell

  type block_typ =
    | QBlock(item_typ)
    | QBlockUsed
    | Brick
    | UnBBlock
    | Cloud
    | Panel
    | Ground

  type player_typ =
    | Standing
    | Jumping
    | Running
    | Crouching

  type part_typ =
    | GoombaSquish
    | BrickChunkL
    | BrickChunkR
    | Score100
    | Score200
    | Score400
    | Score800
    | Score1000
    | Score2000
    | Score4000
    | Score8000

  type spawn_typ =
    | SPlayer(pl_typ, player_typ)
    | SEnemy(enemy_typ)
    | SItem(item_typ)
    | SBlock(block_typ)
}
module Dom_html = {
  type imageElement
  type canvasRenderingContext2D
  type canvasElement

  @val external document: Dom.document = "document"
  @val external window: Dom.window = "window"

  /* external createImg: (_ [@as "img"]) -> document -> imageElement = "createElement" [@@send] */
  @send external createImg: (Dom.document, @as("img") _) => imageElement = "createElement"
  @val external requestAnimationFrame: (float => unit) => unit = "requestAnimationFrame"
  @return(null_to_opt) @send
  external getElementById: (Dom.document, string) => option<Dom.element> = "getElementById"
  @send
  external addEventListener: (Dom.document, string, Dom.event_like<'a> => bool, bool) => unit =
    "addEventListener"
  @send
  external addEventListenerImg: (imageElement, string, Dom.event_like<'a> => bool, bool) => unit =
    "addEventListener"

  /* unsafe casts */
  external imageElementToJsObj: imageElement => {..} = "%identity"
  external canvasRenderingContext2DToJsObj: canvasRenderingContext2D => {..} = "%identity"
  external canvasElementToJsObj: canvasElement => {..} = "%identity"
  external keyboardEventToJsObj: Dom.keyboardEvent => {..} = "%identity"
  external elementToCanvasElement: Dom.element => canvasElement = "%identity"
  external windowToJsObj: Dom.window => {..} = "%identity"
}
module Sprite: {
  open Actors

  /* Represents an xy vector */
  type xy = (float, float) /* x, y */

  /* Inherent sprite parameters from which to create the sprite */
  type sprite_params = {
    max_frames: int,
    max_ticks: int,
    img_src: string,
    frame_size: xy,
    src_offset: xy,
    bbox_offset: xy,
    bbox_size: xy,
    loop: bool,
  }

  /* Concrete sprite created to visually represent an object */
  type sprite = {
    mutable params: sprite_params,
    context: Dom_html.canvasRenderingContext2D,
    frame: ref<int>,
    ticks: ref<int>,
    mutable img: Dom_html.imageElement,
  }

  /* Sets up a sprite to create */
  let setup_sprite: (
    ~loop: bool=?,
    ~bb_off: (float, float)=?,
    ~bb_sz: (float, float)=?,
    string,
    int,
    int,
    xy,
    xy,
  ) => sprite_params

  /* Creates a sprite given the actor type */
  let make: (Actors.spawn_typ, Actors.dir_1d, Dom_html.canvasRenderingContext2D) => sprite

  /* Make a background */
  let make_bgd: Dom_html.canvasRenderingContext2D => sprite

  /* Make a particle corresponding to the given type */
  let make_particle: (Actors.part_typ, Dom_html.canvasRenderingContext2D) => sprite

  /* Transform an enemy sprite based on direction */
  let transform_enemy: (Actors.enemy_typ, sprite, Actors.dir_1d) => unit

  /* Updates the sprite's animation */
  let update_animation: sprite => unit
} = {
  open Actors

  type xy = (float, float)

  type sprite_params = {
    max_frames: int,
    max_ticks: int,
    img_src: string,
    frame_size: xy,
    src_offset: xy,
    bbox_offset: xy,
    bbox_size: xy,
    loop: bool,
  }

  type sprite = {
    mutable params: sprite_params,
    context: Dom_html.canvasRenderingContext2D,
    frame: ref<int>,
    ticks: ref<int>,
    mutable img: Dom_html.imageElement,
  }

  /* setup_sprite is used to initialize a sprite. */
  let setup_sprite = (
    ~loop=true,
    ~bb_off as bbox_offset=(0., 0.),
    ~bb_sz as bbox_size=(0., 0.),
    img_src,
    max_frames,
    max_ticks,
    frame_size,
    src_offset,
  ) => {
    let bbox_size = if bbox_size == (0., 0.) {
      frame_size
    } else {
      bbox_size
    }
    let img_src = "./sprites/" ++ img_src
    {
      img_src,
      max_frames,
      max_ticks,
      frame_size,
      src_offset,
      bbox_offset,
      bbox_size,
      loop,
    }
  }

  /* The following functions are used in order to define sprite animations
   *from their sprite sheets. Also creates bounding boxes if necessary. */

  /* Sets sprite for small mario. */
  let make_small_player = ((typ, dir)) =>
    switch dir {
    /* 16x16 grid with 0x0 offset */
    | Left =>
      switch typ {
      | Standing =>
        setup_sprite(
          "mario-small.png",
          ~bb_off=(3., 1.),
          ~bb_sz=(11., 15.),
          1,
          0,
          (16., 16.),
          (0., 0.),
        )
      | Jumping =>
        setup_sprite(
          "mario-small.png",
          ~bb_off=(2., 1.),
          ~bb_sz=(13., 15.),
          2,
          10,
          (16., 16.),
          (16., 16.),
        )
      | Running =>
        setup_sprite(
          "mario-small.png",
          ~bb_off=(2., 1.),
          ~bb_sz=(12., 15.),
          3,
          5,
          (16., 16.),
          (16., 0.),
        )
      | Crouching =>
        setup_sprite(
          "mario-small.png",
          ~bb_off=(1., 5.),
          ~bb_sz=(14., 10.),
          1,
          0,
          (16., 16.),
          (0., 64.),
        )
      }
    | Right =>
      switch typ {
      | Standing =>
        setup_sprite(
          "mario-small.png",
          ~bb_off=(1., 1.),
          ~bb_sz=(11., 15.),
          1,
          0,
          (16., 16.),
          (0., 32.),
        )
      | Jumping =>
        setup_sprite(
          "mario-small.png",
          ~bb_off=(2., 1.),
          ~bb_sz=(13., 15.),
          2,
          10,
          (16., 16.),
          (16., 48.),
        )
      | Running =>
        setup_sprite(
          "mario-small.png",
          ~bb_off=(2., 1.),
          ~bb_sz=(12., 15.),
          3,
          5,
          (16., 16.),
          (16., 32.),
        )
      | Crouching =>
        setup_sprite(
          "mario-small.png",
          ~bb_off=(1., 5.),
          ~bb_sz=(14., 10.),
          1,
          0,
          (16., 16.),
          (0., 64.),
        )
      }
    }

  /* Sets sprite for big mario. */
  let make_big_player = ((typ, dir)) =>
    switch dir {
    | Left =>
      switch typ {
      | Standing =>
        setup_sprite(
          "mario-big.png",
          1,
          0,
          ~bb_off=(2., 1.),
          ~bb_sz=(13., 25.),
          (16., 27.),
          (16., 5.),
        )
      | Jumping =>
        setup_sprite(
          "mario-big.png",
          1,
          0,
          ~bb_off=(2., 1.),
          ~bb_sz=(12., 25.),
          (16., 26.),
          (48., 6.),
        )
      | Running =>
        setup_sprite(
          "mario-big.png",
          4,
          10,
          ~bb_off=(2., 1.),
          ~bb_sz=(13., 25.),
          (16., 27.),
          (0., 37.),
        )
      | Crouching =>
        setup_sprite(
          "mario-big.png",
          1,
          0,
          ~bb_off=(2., 10.),
          ~bb_sz=(13., 17.),
          (16., 27.),
          (32., 5.),
        )
      }
    | Right =>
      switch typ {
      | Standing =>
        setup_sprite(
          "mario-big.png",
          1,
          0,
          ~bb_off=(1., 1.),
          ~bb_sz=(13., 25.),
          (16., 26.),
          (16., 69.),
        )
      | Jumping =>
        setup_sprite(
          "mario-big.png",
          1,
          0,
          ~bb_off=(2., 1.),
          ~bb_sz=(12., 25.),
          (16., 26.),
          (48., 70.),
        )
      | Running =>
        setup_sprite(
          "mario-big.png",
          4,
          10,
          ~bb_off=(2., 1.),
          ~bb_sz=(13., 25.),
          (16., 27.),
          (0., 101.),
        )
      | Crouching =>
        setup_sprite(
          "mario-big.png",
          1,
          0,
          ~bb_off=(2., 10.),
          ~bb_sz=(13., 17.),
          (16., 27.),
          (32., 69.),
        )
      }
    }

  /* Sets sprites for enemies: Goomba, Red Koopa, Green Koopa. */
  let make_enemy = ((typ, dir)) =>
    switch (typ, dir) {
    | (Goomba, _) =>
      setup_sprite(
        "enemies.png",
        ~bb_off=(1., 1.),
        ~bb_sz=(14., 14.),
        2,
        10,
        (16., 16.),
        (0., 128.),
      )
    | (GKoopa, Left) =>
      setup_sprite(
        "enemies.png",
        ~bb_off=(4., 10.),
        ~bb_sz=(11., 16.),
        2,
        10,
        (16., 27.),
        (0., 69.),
      )
    | (GKoopa, Right) =>
      setup_sprite(
        "enemies.png",
        ~bb_off=(1., 10.),
        ~bb_sz=(11., 16.),
        2,
        10,
        (16., 27.),
        (32., 69.),
      )
    | (RKoopa, Left) =>
      setup_sprite("enemies.png", ~bb_off=(4., 10.), ~bb_sz=(11., 16.), 2, 10, (16., 27.), (0., 5.))
    | (RKoopa, Right) =>
      setup_sprite(
        "enemies.png",
        ~bb_off=(1., 10.),
        ~bb_sz=(11., 16.),
        2,
        10,
        (16., 27.),
        (32., 5.),
      )
    | (GKoopaShell, _) =>
      setup_sprite("enemies.png", ~bb_off=(2., 2.), ~bb_sz=(12., 13.), 4, 10, (16., 16.), (0., 96.))
    | (RKoopaShell, _) =>
      setup_sprite("enemies.png", ~bb_off=(2., 2.), ~bb_sz=(12., 13.), 4, 10, (16., 16.), (0., 32.))
    }

  /* Sets sprites for items: coin, fireflower, mushroom, star. */
  let make_item = x =>
    /* 16x16 grid with 0x0 offset */
    switch x {
    | Coin =>
      setup_sprite("items.png", ~bb_off=(3., 0.), ~bb_sz=(12., 16.), 3, 15, (16., 16.), (0., 80.))
    | FireFlower => setup_sprite("items.png", 1, 0, (16., 16.), (0., 188.))
    | Mushroom =>
      setup_sprite("items.png", ~bb_off=(2., 0.), ~bb_sz=(12., 16.), 1, 0, (16., 16.), (0., 0.))
    | Star => setup_sprite("items.png", 1, 0, (16., 16.), (16., 48.))
    }

  /* Sets sprites for blocks: brick, question block, unbreakable block, cloud block
   * panel block, ground block. */
  let make_block = x =>
    /* 16x16 grid with 0x0 offset */
    switch x {
    | Brick => setup_sprite("blocks.png", 5, 10, (16., 16.), (0., 0.))
    | QBlock(_) => setup_sprite("blocks.png", 4, 15, (16., 16.), (0., 16.))
    | QBlockUsed => setup_sprite("blocks.png", 1, 0, (16., 16.), (0., 32.))
    | UnBBlock => setup_sprite("blocks.png", 1, 0, (16., 16.), (0., 48.))
    | Cloud => setup_sprite("blocks.png", 1, 0, (16., 16.), (0., 64.))
    | Panel => setup_sprite("panel.png", 3, 15, (26., 26.), (0., 0.))
    | Ground => setup_sprite("ground.png", 1, 0, (16., 16.), (0., 32.))
    }

  /* Sets sprites for particles, squished goomba, brick chunks (upon destruction
   * of brick), score text. */
  let make_particle = x =>
    switch x {
    | GoombaSquish => setup_sprite("enemies.png", 1, 0, (16., 16.), (0., 144.))
    | BrickChunkL => setup_sprite("chunks.png", 1, 0, (8., 8.), (0., 0.))
    | BrickChunkR => setup_sprite("chunks.png", 1, 0, (8., 8.), (8., 0.))
    | Score100 => setup_sprite("score.png", 1, 0, (12., 8.), (0., 0.))
    | Score200 => setup_sprite("score.png", 1, 0, (12., 9.), (0., 9.))
    | Score400 => setup_sprite("score.png", 1, 0, (12., 9.), (0., 18.))
    | Score800 => setup_sprite("score.png", 1, 0, (12., 9.), (0., 27.))
    | Score1000 => setup_sprite("score.png", 1, 0, (14., 9.), (13., 0.))
    | Score2000 => setup_sprite("score.png", 1, 0, (14., 9.), (13., 9.))
    | Score4000 => setup_sprite("score.png", 1, 0, (14., 9.), (13., 18.))
    | Score8000 => setup_sprite("score.png", 1, 0, (14., 9.), (13., 27.))
    }

  /* Calls to set sprite for either big or small mario. */
  let make_player = (pt, spr_type) =>
    switch pt {
    | BigM => make_big_player(spr_type)
    | SmallM => make_small_player(spr_type)
    }

  /* Calls to set sprites for each type of object. */
  let make_type = (typ, dir: Actors.dir_1d) =>
    switch typ {
    | SPlayer(pt, st) => make_player(pt, (st, dir))
    | SEnemy(t) => make_enemy((t, dir))
    | SItem(t) => make_item(t)
    | SBlock(t) => make_block(t)
    }

  /* Makes a sprite from provided [params]. */
  let make_from_params = (params, context) => {
    let img = Dom_html.createImg(Dom_html.document)
    Dom_html.imageElementToJsObj(img)["src"] = params.img_src
    {
      params,
      context,
      img,
      frame: ref(0),
      ticks: ref(0),
    }
  }

  /* Make is the wrapper function to cycle through sprite animations */
  let make = (spawn, dir, context) => {
    let params = make_type(spawn, dir)
    make_from_params(params, context)
  }

  /* Make a background */
  let make_bgd = context => {
    let params = setup_sprite("bgd-1.png", 1, 0, (512., 256.), (0., 0.))
    make_from_params(params, context)
  }

  /* Make a particle from the given particle type */
  let make_particle = (ptyp, context) => {
    let params = make_particle(ptyp)
    make_from_params(params, context)
  }

  /* Transform_enemy is used in order to switch the direction an enemy faces. */
  let transform_enemy = (enemy_typ, spr, dir) => {
    let params = make_enemy((enemy_typ, dir))
    let img = Dom_html.createImg(Dom_html.document)
    Dom_html.imageElementToJsObj(img)["src"] = params.img_src
    spr.params = params
    spr.img = img
  }

  /* update_animation is the main method to cycle through sprite animations */
  let update_animation = (spr: sprite) => {
    /* Only advance frame when ticked */
    let curr_ticks = spr.ticks.contents
    if curr_ticks >= spr.params.max_ticks {
      spr.ticks := 0
      if spr.params.loop {
        spr.frame := mod(spr.frame.contents + 1, spr.params.max_frames)
      }
    } else {
      spr.ticks := curr_ticks + 1
    }
  }
}
module Particle: {
  open Actors
  open Sprite

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

  let make: (
    ~vel: (float, float)=?,
    ~acc: (float, float)=?,
    Actors.part_typ,
    (float, float),
    Dom_html.canvasRenderingContext2D,
  ) => particle

  let make_score: (int, (float, float), Dom_html.canvasRenderingContext2D) => particle

  let process: particle => unit
} = /* Template params associated with a particle */

/* Backing sprite */
/* Rotation */
/* Life span */

/* Kill the particle in the next frame */
/* Remaining lifespan of particle */

/* Makes a new particle of the given particle type with at a position. */

/* Make a score particle. The first int indicates the score to spawn */

/* Process a particle, updating its velocity and position. Also marks it as
 * killable if it exceeds its lifespan */

{
  open Actors
  open Sprite

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
  let pair_to_xy = pair => {
    x: fst(pair),
    y: snd(pair),
  }

  /* Function wrapper to assist in generating the template paramss for a
   * particle. */
  let make_params = (sprite, rot, lifetime) => {
    sprite,
    rot,
    lifetime,
  }

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
      params,
      part_type,
      pos,
      vel,
      acc,
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
}
module Object: {
  open Sprite
  open Actors
  open Particle

  let invuln: int
  let dampen_jump: float

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
    | Player(pl_typ, sprite, obj)
    | Enemy(enemy_typ, sprite, obj)
    | Item(item_typ, sprite, obj)
    | Block(block_typ, sprite, obj)

  let get_sprite: collidable => Sprite.sprite

  let get_obj: collidable => obj

  let spawn: (Actors.spawn_typ, Dom_html.canvasRenderingContext2D, (float, float)) => collidable

  let equals: (collidable, collidable) => bool

  let is_player: collidable => bool
  let is_enemy: collidable => bool

  let normalize_origin: (xy, Sprite.sprite) => unit

  let normalize_pos: (xy, Sprite.sprite_params, Sprite.sprite_params) => unit

  let kill: (collidable, Dom_html.canvasRenderingContext2D) => list<particle>

  let process_obj: (obj, float) => unit

  let update_player: (
    obj,
    list<Actors.controls>,
    Dom_html.canvasRenderingContext2D,
  ) => option<(pl_typ, sprite)>

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
} = /* # of frames of invulnerability */
/* Boost to jump when enemy jumped on */

/* Returns the sprite associated with the object */

/* Creates a new object with a given
 * actor type on the the canvas at a given position */

/* Destroys the object, returning a list of destruction effect objects */

/* Checks whether a collision occured between two objects, returning the
 * direction of the collision if one occurred. */

{
  open Sprite
  open Actors
  open Particle

  /* Variables */
  let friction = 0.9
  let gravity = 0.2
  let max_y_vel = 4.5
  let player_speed = 2.8
  let player_jump = 5.7
  let player_max_jump = -6.
  let dampen_jump = 4.
  let invuln = 60

  type aabb = {
    center: xy,
    half: xy,
  }

  type obj_params = {
    has_gravity: bool,
    speed: float,
  }

  let id_counter = ref(min_int)

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
    | Player(pl_typ, sprite, obj)
    | Enemy(enemy_typ, sprite, obj)
    | Item(item_typ, sprite, obj)
    | Block(block_typ, sprite, obj)

  /* setup_obj is used to set gravity and speed, with default values true and 1. */
  let setup_obj = (~g as has_gravity=true, ~spd as speed=1., ()) => {
    has_gravity,
    speed,
  }

  /* Sets an object's x velocity to the speed specified in its params based on
   * its direction */
  let set_vel_to_speed = obj => {
    let speed = obj.params.speed
    switch obj.dir {
    | Left => obj.vel.x = -.speed
    | Right => obj.vel.x = speed
    }
  }

  /* The following make functions all set the objects' has_gravity and speed,
   * returning an [obj_params] that can be directly plugged into the [obj]
   * during creation. */
  let make_player = () => setup_obj(~spd=player_speed, ())

  let make_item = x =>
    switch x {
    | Mushroom => setup_obj()
    | FireFlower => setup_obj()
    | Star => setup_obj()
    | Coin => setup_obj(~g=false, ())
    }

  let make_enemy = x =>
    switch x {
    | Goomba => setup_obj()
    | GKoopa => setup_obj()
    | RKoopa => setup_obj()
    | GKoopaShell => setup_obj(~spd=3., ())
    | RKoopaShell => setup_obj(~spd=3., ())
    }

  let make_block = x =>
    switch x {
    | QBlock(i) => setup_obj(~g=false, ())
    | QBlockUsed => setup_obj(~g=false, ())
    | Brick => setup_obj(~g=false, ())
    | UnBBlock => setup_obj(~g=false, ())
    | Cloud => setup_obj(~g=false, ())
    | Panel => setup_obj(~g=false, ())
    | Ground => setup_obj(~g=false, ())
    }

  let make_type = x =>
    switch x {
    | SPlayer(pt, t) => make_player() /* FIXME: why unused param introduced here */
    | SEnemy(t) => make_enemy(t)
    | SItem(t) => make_item(t)
    | SBlock(t) => make_block(t)
    }

  /* Used in object creation and to compare two objects. */
  let new_id = () => {
    id_counter := id_counter.contents + 1
    id_counter.contents
  }

  /* Used to return a new sprite and object of a created spawnable object */
  let make = (~id=None, ~dir=Left, spawnable, context, (posx, posy)) => {
    let spr = Sprite.make(spawnable, dir, context)
    let params = make_type(spawnable)
    let id = switch id {
    | None => new_id()
    | Some(n) => n
    }

    let obj = {
      params,
      pos: {x: posx, y: posy},
      vel: {x: 0.0, y: 0.0},
      id,
      jumping: false,
      grounded: false,
      dir,
      invuln: 0,
      kill: false,
      health: 1,
      crouch: false,
      score: 0,
    }
    (spr, obj)
  }

  /* spawn returns a new collidable */
  let spawn = (spawnable, context, (posx, posy)) => {
    let (spr, obj) = make(spawnable, context, (posx, posy))
    switch spawnable {
    | SPlayer(typ, t) => Player(typ, spr, obj)
    | SEnemy(t) =>
      set_vel_to_speed(obj)
      Enemy(t, spr, obj)
    | SItem(t) => Item(t, spr, obj)
    | SBlock(t) => Block(t, spr, obj)
    }
  }

  /* Helper methods for getting sprites and objects from their collidables */
  let get_sprite = x =>
    switch x {
    | Player(_, s, _) | Enemy(_, s, _) | Item(_, s, _) | Block(_, s, _) => s
    }

  let get_obj = x =>
    switch x {
    | Player(_, _, o) | Enemy(_, _, o) | Item(_, _, o) | Block(_, _, o) => o
    }

  let is_player = x =>
    switch x {
    | Player(_, _, _) => true
    | _ => false
    }

  let is_enemy = x =>
    switch x {
    | Enemy(_, _, _) => true
    | _ => false
    }

  let equals = (col1, col2) => get_obj(col1).id == get_obj(col2).id

  /* Matches the controls being used and updates each of the player's params. */
  let update_player_keys = (player: obj, controls: controls): unit => {
    let lr_acc = player.vel.x *. 0.2
    switch controls {
    | CLeft =>
      if !player.crouch {
        if player.vel.x > -.player.params.speed {
          player.vel.x = player.vel.x -. (0.4 -. lr_acc)
        }
        player.dir = Left
      }
    | CRight =>
      if !player.crouch {
        if player.vel.x < player.params.speed {
          player.vel.x = player.vel.x +. (0.4 +. lr_acc)
        }
        player.dir = Right
      }
    | CUp =>
      if !player.jumping && player.grounded {
        player.jumping = true
        player.grounded = false
        player.vel.y = max(
          player.vel.y -. (player_jump +. abs_float(player.vel.x) *. 0.25),
          player_max_jump,
        )
      }
    | CDown =>
      if !player.jumping && player.grounded {
        player.crouch = true
      }
    }
  }

  /* Used for sprite changing. If sprites change to different dimensions as a result
   *of some action, the new sprite must be normalized so that things aren't
   *jumpy */
  let normalize_pos = (pos, p1: Sprite.sprite_params, p2: Sprite.sprite_params) => {
    let (box1, boy1) = p1.bbox_offset and (box2, boy2) = p2.bbox_offset
    let (bw1, bh1) = p1.bbox_size and (bw2, bh2) = p2.bbox_size
    pos.x = pos.x -. (bw2 +. box2) +. (bw1 +. box1)
    pos.y = pos.y -. (bh2 +. boy2) +. (bh1 +. boy1)
  }

  /* Update player is constantly being called to check for if big or small
   *Mario sprites/collidables should be used. */
  let update_player = (player, keys, context) => {
    let prev_jumping = player.jumping
    let prev_dir = player.dir and prev_vx = abs_float(player.vel.x)
    keys->List.forEach(update_player_keys(player, ...))
    let v = player.vel.x *. friction
    let vel_damped = if abs_float(v) < 0.1 {
      0.
    } else {
      v
    }
    player.vel.x = vel_damped
    let pl_typ = if player.health <= 1 {
      SmallM
    } else {
      BigM
    }
    if !prev_jumping && player.jumping {
      Some(pl_typ, Sprite.make(SPlayer(pl_typ, Jumping), player.dir, context))
    } else if (
      prev_dir != player.dir || (prev_vx == 0. && abs_float(player.vel.x) > 0. && !player.jumping)
    ) {
      Some(pl_typ, Sprite.make(SPlayer(pl_typ, Running), player.dir, context))
    } else if prev_dir != player.dir && (player.jumping && prev_jumping) {
      Some(pl_typ, Sprite.make(SPlayer(pl_typ, Jumping), player.dir, context))
    } else if player.vel.y == 0. && player.crouch {
      Some(pl_typ, Sprite.make(SPlayer(pl_typ, Crouching), player.dir, context))
    } else if player.vel.y == 0. && player.vel.x == 0. {
      Some(pl_typ, Sprite.make(SPlayer(pl_typ, Standing), player.dir, context))
    } else {
      None
    }
  }

  /* The following two helper methods update velocity and position of the player */
  let update_vel = obj =>
    if obj.grounded {
      obj.vel.y = 0.
    } else if obj.params.has_gravity {
      obj.vel.y = min(obj.vel.y +. gravity +. abs_float(obj.vel.y) *. 0.01, max_y_vel)
    }

  let update_pos = obj => {
    obj.pos.x = obj.vel.x +. obj.pos.x
    if obj.params.has_gravity {
      obj.pos.y = obj.vel.y +. obj.pos.y
    }
  }

  /* Calls two above helper functions to update velocity and position of player. */
  let process_obj = (obj, mapy) => {
    update_vel(obj)
    update_pos(obj)
    if obj.pos.y > mapy {
      obj.kill = true
    }
  }

  /* Converts an origin based on the bottom left of the bounding box to the top
   * right of the sprite, to make it easier to place objects flush with the ground. */
  let normalize_origin = (pos, spr: Sprite.sprite) => {
    let p = spr.params
    let (box, boy) = p.bbox_offset and (_, bh) = p.bbox_size
    pos.x = pos.x -. box
    pos.y = pos.y -. (boy +. bh)
  }

  /* Checks upon collision of block and updates the values of the object. */
  let collide_block = (~check_x=true, dir, obj) =>
    switch dir {
    | North => obj.vel.y = -0.001
    | South =>
      obj.vel.y = 0.
      obj.grounded = true
      obj.jumping = false
    | East | West =>
      if check_x {
        obj.vel.x = 0.
      }
    }

  /* Simple helper method that reverses the direction in question */
  let opposite_dir = dir =>
    switch dir {
    | Left => Right
    | Right => Left
    }

  /* Used for enemy-enemy collisions */
  let reverse_left_right = obj => {
    obj.vel.x = -.obj.vel.x
    obj.dir = opposite_dir(obj.dir)
  }

  /* Actually creates a new enemy and deletes the previous. The positions must be
   *normalized. This method is typically called when enemies are killed and a
   *new sprite must be used (i.e., koopa to koopa shell). */
  let evolve_enemy = (player_dir, typ, spr: Sprite.sprite, obj, context) =>
    switch typ {
    | GKoopa =>
      let (new_spr, new_obj) = make(
        ~dir=obj.dir,
        SEnemy(GKoopaShell),
        context,
        (obj.pos.x, obj.pos.y),
      )
      normalize_pos(new_obj.pos, spr.params, new_spr.params)
      Some(Enemy(GKoopaShell, new_spr, new_obj))
    | RKoopa =>
      let (new_spr, new_obj) = make(
        ~dir=obj.dir,
        SEnemy(RKoopaShell),
        context,
        (obj.pos.x, obj.pos.y),
      )
      normalize_pos(new_obj.pos, spr.params, new_spr.params)
      Some(Enemy(RKoopaShell, new_spr, new_obj))
    | GKoopaShell | RKoopaShell =>
      obj.dir = player_dir
      if obj.vel.x != 0. {
        obj.vel.x = 0.
      } else {
        set_vel_to_speed(obj)
      }
      None
    | _ =>
      obj.kill = true
      None
    }

  /* Updates the direction of the sprite. */
  let rev_dir = (o, t, s: sprite) => {
    reverse_left_right(o)
    let old_params = s.params
    Sprite.transform_enemy(t, s, o.dir)
    normalize_pos(o.pos, old_params, s.params)
  }

  /* Used for killing enemies, or to make big Mario into small Mario */
  let dec_health = obj => {
    let health = obj.health - 1
    if health == 0 {
      obj.kill = true
    } else if obj.invuln == 0 {
      obj.health = health
    }
  }

  /* Used for deleting a block and replacing it with a used block */
  let evolve_block = (obj, context) => {
    dec_health(obj)
    let (new_spr, new_obj) = make(SBlock(QBlockUsed), context, (obj.pos.x, obj.pos.y))
    Block(QBlockUsed, new_spr, new_obj)
  }

  /* Used for making a small Mario into a Big Mario */
  let evolve_player = (spr: Sprite.sprite, obj, context) => {
    let (new_spr, new_obj) = make(SPlayer(BigM, Standing), context, (obj.pos.x, obj.pos.y))
    normalize_pos(new_obj.pos, spr.params, new_spr.params)
    Player(BigM, new_spr, new_obj)
  }

  /* Used for spawning items above question mark blocks */
  let spawn_above = (player_dir, obj, typ, context) => {
    let item = spawn(SItem(typ), context, (obj.pos.x, obj.pos.y))
    let item_obj = get_obj(item)
    item_obj.pos.y = item_obj.pos.y -. snd(get_sprite(item).params.frame_size)
    item_obj.dir = opposite_dir(player_dir)
    set_vel_to_speed(item_obj)
    item
  }

  /* Used to get the bounding box. */
  let get_aabb = obj => {
    let spr = get_sprite(obj).params
    let obj = get_obj(obj)
    let (offx, offy) = spr.bbox_offset
    let (box, boy) = (obj.pos.x +. offx, obj.pos.y +. offy)
    let (sx, sy) = spr.bbox_size
    {
      center: {x: box +. sx /. 2., y: boy +. sy /. 2.},
      half: {x: sx /. 2., y: sy /. 2.},
    }
  }

  let col_bypass = (c1, c2) => {
    let o1 = get_obj(c1) and o2 = get_obj(c2)
    let ctypes = switch (c1, c2) {
    | (Item(_, _, _), Enemy(_, _, _))
    | (Enemy(_, _, _), Item(_, _, _))
    | (Item(_, _, _), Item(_, _, _)) => true
    | (Player(_, _, o1), Enemy(_, _, _)) =>
      if o1.invuln > 0 {
        true
      } else {
        false
      }
    | _ => false
    }
    o1.kill || (o2.kill || ctypes)
  }

  /* Used for checking if collisions occur. Compares half-widths and half-heights
   *and adjusts for when collisions do occur, by changing position so that
   *a second collision does not occur again immediately. This causes snapping. */
  let check_collision = (c1, c2) => {
    let b1 = get_aabb(c1) and b2 = get_aabb(c2)
    let o1 = get_obj(c1)
    if col_bypass(c1, c2) {
      None
    } else {
      let vx = b1.center.x -. b2.center.x
      let vy = b1.center.y -. b2.center.y
      let hwidths = b1.half.x +. b2.half.x
      let hheights = b1.half.y +. b2.half.y
      if abs_float(vx) < hwidths && abs_float(vy) < hheights {
        let ox = hwidths -. abs_float(vx)
        let oy = hheights -. abs_float(vy)
        if ox >= oy {
          if vy > 0. {
            o1.pos.y = o1.pos.y +. oy
            Some(North)
          } else {
            o1.pos.y = o1.pos.y -. oy
            Some(South)
          }
        } else if vx > 0. {
          o1.pos.x = o1.pos.x +. ox
          Some(West)
        } else {
          o1.pos.x = o1.pos.x -. ox
          Some(East)
        }
      } else {
        None
      }
    }
  }

  /* "Kills" the matched object by setting certain parameters for each. */
  let kill = (collid, ctx) =>
    switch collid {
    | Enemy(t, s, o) =>
      let pos = (o.pos.x, o.pos.y)
      let score = if o.score > 0 {
        list{Particle.make_score(o.score, pos, ctx)}
      } else {
        list{}
      }
      let remains = switch t {
      | Goomba => list{Particle.make(GoombaSquish, pos, ctx)}
      | _ => list{}
      }
      \"@"(score, remains)
    | Block(t, s, o) =>
      switch t {
      | Brick =>
        let pos = (o.pos.x, o.pos.y)
        let p1 = Particle.make(~vel=(-5., -5.), ~acc=(0., 0.2), BrickChunkL, pos, ctx)
        let p2 = Particle.make(~vel=(-3., -4.), ~acc=(0., 0.2), BrickChunkL, pos, ctx)
        let p3 = Particle.make(~vel=(3., -4.), ~acc=(0., 0.2), BrickChunkR, pos, ctx)
        let p4 = Particle.make(~vel=(5., -5.), ~acc=(0., 0.2), BrickChunkR, pos, ctx)
        list{p1, p2, p3, p4}
      | _ => list{}
      }
    | Item(t, s, o) =>
      switch t {
      | Mushroom => list{Particle.make_score(o.score, (o.pos.x, o.pos.y), ctx)}
      | _ => list{}
      }
    | _ => list{}
    }
}
module Draw: {
  let render: (Sprite.sprite, (float, float)) => unit

  let clear_canvas: Dom_html.canvasElement => unit

  let draw_bgd: (Sprite.sprite, float) => unit

  let render_bbox: (Sprite.sprite, (float, float)) => unit

  let fps: (Dom_html.canvasElement, float) => unit

  let hud: (Dom_html.canvasElement, int, int) => unit

  let game_win: Dom_html.canvasRenderingContext2D => unit

  let game_loss: Dom_html.canvasRenderingContext2D => unit
} = /* Renders a given object on the canvas */

/* Clears the canvas */

/* Draw the given sprite as a background */

/* Draws the axis aligned bounding box of the sprite at the position */

/* Draws the fps on the canvas */

/* Draw the heads up display */

/* Draw the game win screen */

/* Draw the game loss screen */

{
  open Object
  open Sprite
  module Html = Dom_html
  let document = Html.document

  let get_context = canvas => canvas["getContext"]("2d")

  let render_bbox = (sprite, (posx, posy)) => {
    let context = Dom_html.canvasRenderingContext2DToJsObj(sprite.context)
    let (bbox, bboy) = sprite.params.bbox_offset
    let (bbsx, bbsy) = sprite.params.bbox_size
    context["strokeStyle"] = "#FF0000"
    context["strokeRect"](posx +. bbox, posy +. bboy, bbsx, bbsy)
  }

  /* Draws a sprite onto the canvas. */
  let render = (sprite, (posx, posy)) => {
    let context = Dom_html.canvasRenderingContext2DToJsObj(sprite.context)
    let (sx, sy) = sprite.params.src_offset
    let (sw, sh) = sprite.params.frame_size
    let (dx, dy) = (posx, posy)
    let (dw, dh) = sprite.params.frame_size
    let sx = sx +. float_of_int(sprite.frame.contents) *. sw
    /* Console.log(sprite.frame) */
    /* context##clearRect(0.,0.,sw, sh); */
    context["drawImage"](sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)
  }

  /* Draws two background images, which needs to be done because of the
   *constantly changing viewport, which is always at most going to be
   *between two background images. */
  let draw_bgd = (bgd, off_x) => {
    render(bgd, (-.off_x, 0.))
    render(bgd, (fst(bgd.params.frame_size) -. off_x, 0.))
  }

  /* Used for animation updating. Canvas is cleared each frame and redrawn. */
  let clear_canvas = canvas => {
    let canvas = Dom_html.canvasElementToJsObj(canvas)
    let context = Dom_html.canvasRenderingContext2DToJsObj(canvas["getContext"]("2d"))
    let cwidth = float_of_int(canvas["width"])
    let cheight = float_of_int(canvas["height"])
    ignore(context["clearRect"](0., 0., cwidth, cheight))
  }

  /* Displays the text for score and coins. */
  let hud = (canvas, score, coins) => {
    let score_string = Js.Int.toString(score)
    let coin_string = Js.Int.toString(coins)
    let canvas = Dom_html.canvasElementToJsObj(canvas)
    let context = Dom_html.canvasRenderingContext2DToJsObj(canvas["getContext"]("2d"))
    ignore(context["font"] = "10px 'Press Start 2P'")
    ignore(
      context["fillText"]("Score: " ++ score_string, float_of_int(canvas["width"]) -. 140., 18.),
    )
    ignore(context["fillText"]("Coins: " ++ coin_string, 120., 18.))
  }

  /* Displays the fps. */
  let fps = (canvas, fps_val) => {
    let fps_str = fps_val->Js.Float.toFixed
    let canvas = Dom_html.canvasElementToJsObj(canvas)
    let context = Dom_html.canvasRenderingContext2DToJsObj(canvas["getContext"]("2d"))
    ignore(context["fillText"](fps_str, 10., 18.))
  }

  /* game_win displays a black screen when you finish a game. */
  let game_win = ctx => {
    let ctx = Dom_html.canvasRenderingContext2DToJsObj(ctx)
    ctx["rect"](0., 0., 512., 512.)
    ctx["fillStyle"] = "black"
    ctx["fill"]()
    ctx["fillStyle"] = "white"
    ctx["font"] = "20px 'Press Start 2P'"
    ctx["fillText"]("You win!", 180., 128.)
    failwith("Game over.")
  }

  /* gave_loss displays a black screen stating a loss to finish that level play. */
  let game_loss = ctx => {
    let ctx = Dom_html.canvasRenderingContext2DToJsObj(ctx)
    ctx["rect"](0., 0., 512., 512.)
    ctx["fillStyle"] = "black"
    ctx["fill"]()
    ctx["fillStyle"] = "white"
    ctx["font"] = "20px 'Press Start 2P'"
    ctx["fillText"]("GAME OVER. You lose!", 60., 128.)
    failwith("Game over.")
  }

  let draw_background_color = canvas => failwith("todo")
}
module Viewport: {
  open Actors

  type viewport = {
    pos: Actors.xy /* Absolute position of viewport relative to map */,
    v_dim: Actors.xy /* Dimensions of viewport */,
    m_dim: Actors.xy /* Dimensions of map */,
  }

  /* Makes a new viewport of viewport dimensions and map dimensions */
  let make: ((float, float), (float, float)) => viewport

  /* Calculates the viewport origin point */
  let calc_viewport_point: (float, float, float) => float

  /* Whether the supplied position is outside of the viewport */
  let in_viewport: (viewport, Actors.xy) => bool

  /* Whether the supplied position is below the viewport */
  let out_of_viewport_below: (viewport, float) => bool

  /* Converts absolute coordinates to viewport coodinates */
  let coord_to_viewport: (viewport, Actors.xy) => Actors.xy

  /* Update the viewport */
  let update: (viewport, Actors.xy) => viewport
} = {
  open Actors

  type viewport = {
    pos: Actors.xy,
    v_dim: Actors.xy,
    m_dim: Actors.xy,
  }

  let make = ((vx, vy), (mx, my)) => {
    pos: {x: 0., y: 0.},
    v_dim: {x: vx, y: vy},
    m_dim: {x: mx, y: my},
  }

  /* Calculates the viewport origin coordinate given the centering coordinate
   * [cc], the canvas coordinate [vc], and the map coordinate [mc]. This function
   * works for both x and y. At the extreme points, it will ensure that the
   * viewport is always within bounds of the map, even if it is no longer
   * centered about the origin point. */
  let calc_viewport_point = (cc, vc, mc) => {
    let vc_half = vc /. 2.
    min(max(cc -. vc_half, 0.), min(mc -. vc, abs_float(cc -. vc_half)))
  }

  /* Returns whether a coordinate pair [pos] is inside the viewport [v] */
  let in_viewport = (v, pos) => {
    let margin = 32.
    let (v_min_x, v_max_x) = (v.pos.x -. margin, v.pos.x +. v.v_dim.x)
    let (v_min_y, v_max_y) = (v.pos.y -. margin, v.pos.y +. v.v_dim.y)
    let (x, y) = (pos.x, pos.y)
    x >= v_min_x && (x <= v_max_x && (y >= v_min_y && y <= v_max_y))
  }

  /* Returns whether an object is outside of the viewport and below it. This is
   * useful for determining whether to process falling out of screen normally. */
  let out_of_viewport_below = (v, y) => {
    let v_max_y = v.pos.y +. v.v_dim.y
    y >= v_max_y
  }

  /* Converts a x,y [coord] pair in absolute coordinates to coordinates relative
   * to the viewport */
  let coord_to_viewport = (viewport, coord) => {
    x: coord.x -. viewport.pos.x,
    y: coord.y -. viewport.pos.y,
  }

  /* Update the viewport [vpt] given the new center x,y coordinate pair [ctr] */
  let update = (vpt, ctr) => {
    let new_x = calc_viewport_point(ctr.x, vpt.v_dim.x, vpt.m_dim.x)
    let new_y = calc_viewport_point(ctr.y, vpt.v_dim.y, vpt.m_dim.y)
    let pos = {x: new_x, y: new_y}
    {...vpt, pos}
  }
}
module Director: {
  let update_loop: (
    Dom_html.canvasElement,
    (Object.collidable, list<Object.collidable>),
    (float, float),
  ) => unit

  let keydown: Dom.keyboardEvent => bool

  let keyup: Dom.keyboardEvent => bool
} = /* Initiates the main game loop */

/* Keydown event handler function */

/* Keyup event handler function */

{
  open Sprite
  open Object
  open Actors
  open Viewport
  open Particle

  /* Represents the values of relevant key bindings. */
  type keys = {
    mutable left: bool,
    mutable right: bool,
    mutable up: bool,
    mutable down: bool,
    mutable bbox: int,
  }

  /* st represents the state of the game. It includes a background sprite (e.g.,
   * (e.g., hills), a context (used for rendering onto the page), a viewport
   * (used for moving the player's "camera"), a score (which is kept track
   * throughout the game), coins (also kept track through the game),
   * a multiplier (used for when you kill multiple enemies before ever touching
   * the ground, as in the actual Super Mario), and a game_over bool (which
   * is only true when the game is over). */
  type st = {
    bgd: sprite,
    ctx: Dom_html.canvasRenderingContext2D,
    vpt: viewport,
    map: float,
    mutable score: int,
    mutable coins: int,
    mutable multiplier: int,
    mutable game_over: bool,
  }

  /* pressed_keys instantiates the keys. */
  let pressed_keys = {
    left: false,
    right: false,
    up: false,
    down: false,
    bbox: 0,
  }

  let collid_objs = ref(list{}) /* List of next iteration collidable objects */
  let particles = ref(list{}) /* List of next iteration particles */
  let last_time = ref(0.) /* Used for calculating fps */

  /* Calculates fps as the difference between [t0] and [t1] */
  let calc_fps = (t0, t1) => {
    let delta = (t1 -. t0) /. 1000.
    1. /. delta
  }

  /* Adds [i] to the score in [state] */
  let update_score = (state, i) => state.score = state.score + i

  /* player_attack_enemy is called for a player hitting an enemy from the north.
   *This causes the player to either kill the enemy or move the enemy, in the
   *case that the enemy is a shell. Invulnerability, jumping, and grounded
   *are used for fine tuning the movements. */
  let player_attack_enemy = (s1, o1, typ, s2, o2, state, context) => {
    o1.invuln = 10
    o1.jumping = false
    o1.grounded = true
    switch typ {
    | GKoopaShell | RKoopaShell =>
      let r2 = evolve_enemy(o1.dir, typ, s2, o2, context)
      o1.vel.y = -.dampen_jump
      o1.pos.y = o1.pos.y -. 5.
      (None, r2)
    | _ =>
      dec_health(o2)
      o1.vel.y = -.dampen_jump
      if state.multiplier == 8 {
        update_score(state, 800)
        o2.score = 800
        (None, evolve_enemy(o1.dir, typ, s2, o2, context))
      } else {
        let score = 100 * state.multiplier
        update_score(state, score)
        o2.score = score
        state.multiplier = state.multiplier * 2
        (None, evolve_enemy(o1.dir, typ, s2, o2, context))
      }
    }
  }

  /* enemy_attack_player is used when an enemy kills a player. */
  let enemy_attack_player = (s1, o1: Object.obj, t2, s2, o2: Object.obj, context) =>
    switch t2 {
    | GKoopaShell | RKoopaShell =>
      let r2 = if o2.vel.x == 0. {
        evolve_enemy(o1.dir, t2, s2, o2, context)
      } else {
        dec_health(o1)
        o1.invuln = invuln
        None
      }
      (None, r2)
    | _ =>
      dec_health(o1)
      o1.invuln = invuln
      (None, None)
    }

  /* In the case that two enemies collide, they are to reverse directions. However,
   *in the case that one or more of the two enemies is a koopa shell, then
   *the koopa shell kills the other enemy. */
  let col_enemy_enemy = (t1, s1, o1, t2, s2, o2, dir) =>
    switch (t1, t2) {
    | (GKoopaShell, GKoopaShell)
    | (GKoopaShell, RKoopaShell)
    | (RKoopaShell, RKoopaShell)
    | (RKoopaShell, GKoopaShell) =>
      dec_health(o1)
      dec_health(o2)
      (None, None)
    | (RKoopaShell, _) | (GKoopaShell, _) =>
      if o1.vel.x == 0. {
        rev_dir(o2, t2, s2)
        (None, None)
      } else {
        dec_health(o2)
        (None, None)
      }
    | (_, RKoopaShell) | (_, GKoopaShell) =>
      if o2.vel.x == 0. {
        rev_dir(o1, t1, s1)
        (None, None)
      } else {
        dec_health(o1)
        (None, None)
      }
    | (_, _) =>
      switch dir {
      | West | East =>
        rev_dir(o1, t1, s1)
        rev_dir(o2, t2, s2)
        (None, None)
      | _ => (None, None)
      }
    }

  /* Gets the object at a given position */
  let obj_at_pos = (dir, pos: xy, collids: list<Object.collidable>): list<Object.collidable> =>
    switch dir {
    | Left =>
      collids->List.filter((col: Object.collidable) =>
        get_obj(col).pos.y == pos.y && get_obj(col).pos.x == pos.x -. 16.
      )
    | _ =>
      collids->List.filter((col: Object.collidable) =>
        get_obj(col).pos.y == pos.y && get_obj(col).pos.x == pos.x +. 16.
      )
    }

  /* Returns whether the object at a given position is a block */
  let is_block = (dir, pos, collids) =>
    switch obj_at_pos(dir, pos, collids) {
    | list{} => false
    | list{Block(_, _, _)} => true
    | _ => false
    }

  /* Returns whether the given object is a red koopa */
  let is_rkoopa = collid =>
    switch collid {
    | Enemy(RKoopa, _, _) => true
    | _ => false
    }

  /* Process collision is called to match each of the possible collisions that
   * may occur. Returns a pair of collidable options, representing objects that
   * were created from the existing ones. That is, the first element represents
   * a new item spawned as a result of the first collidable. None indicates that
   * no new item should be spawned. Transformations to existing objects occur
   * mutably, as many changes are side-effectual. */
  let process_collision = (
    dir: Actors.dir_2d,
    c1: Object.collidable,
    c2: Object.collidable,
    state: st,
  ): (option<Object.collidable>, option<Object.collidable>) => {
    let context = state.ctx
    switch (c1, c2, dir) {
    | (Player(_, s1, o1), Enemy(typ, s2, o2), South)
    | (Enemy(typ, s2, o2), Player(_, s1, o1), North) =>
      player_attack_enemy(s1, o1, typ, s2, o2, state, context)
    | (Player(_, s1, o1), Enemy(t2, s2, o2), _)
    | (Enemy(t2, s2, o2), Player(_, s1, o1), _) =>
      enemy_attack_player(s1, o1, t2, s2, o2, context)
    | (Player(_, s1, o1), Item(t2, s2, o2), _)
    | (Item(t2, s2, o2), Player(_, s1, o1), _) =>
      switch t2 {
      | Mushroom =>
        dec_health(o2)
        if o1.health == 2 {
          ()
        } else {
          o1.health = o1.health + 1
        }
        o1.vel.x = 0.
        o1.vel.y = 0.
        update_score(state, 1000)
        o2.score = 1000
        (None, None)
      | Coin =>
        state.coins = state.coins + 1
        dec_health(o2)
        update_score(state, 100)
        (None, None)
      | _ =>
        dec_health(o2)
        update_score(state, 1000)
        (None, None)
      }
    | (Enemy(t1, s1, o1), Enemy(t2, s2, o2), dir) => col_enemy_enemy(t1, s1, o1, t2, s2, o2, dir)
    | (Enemy(t1, s1, o1), Block(t2, s2, o2), East)
    | (Enemy(t1, s1, o1), Block(t2, s2, o2), West) =>
      switch (t1, t2) {
      /* FIXME */
      | (RKoopaShell, Brick) | (GKoopaShell, Brick) =>
        dec_health(o2)
        reverse_left_right(o1)
        (None, None)
      | (RKoopaShell, QBlock(typ)) | (GKoopaShell, QBlock(typ)) =>
        let updated_block = evolve_block(o2, context)
        let spawned_item = spawn_above(o1.dir, o2, typ, context)
        rev_dir(o1, t1, s1)
        (Some(updated_block), Some(spawned_item))
      | (_, _) =>
        rev_dir(o1, t1, s1)
        (None, None)
      }
    | (Item(_, s1, o1), Block(typ2, s2, o2), East)
    | (Item(_, s1, o1), Block(typ2, s2, o2), West) =>
      reverse_left_right(o1)
      (None, None)
    | (Enemy(_, s1, o1), Block(typ2, s2, o2), _)
    | (Item(_, s1, o1), Block(typ2, s2, o2), _) =>
      collide_block(dir, o1)
      (None, None)
    | (Player(t1, s1, o1), Block(t, s2, o2), North) =>
      switch t {
      | QBlock(typ) =>
        let updated_block = evolve_block(o2, context)
        let spawned_item = spawn_above(o1.dir, o2, typ, context)
        collide_block(dir, o1)
        (Some(spawned_item), Some(updated_block))
      | Brick =>
        if t1 == BigM {
          collide_block(dir, o1)
          dec_health(o2)
          (None, None)
        } else {
          collide_block(dir, o1)
          (None, None)
        }
      | Panel =>
        Draw.game_win(state.ctx)
        (None, None)
      | _ =>
        collide_block(dir, o1)
        (None, None)
      }
    | (Player(_, s1, o1), Block(t, s2, o2), _) =>
      switch t {
      | Panel =>
        Draw.game_win(state.ctx)
        (None, None)
      | _ =>
        switch dir {
        | South =>
          state.multiplier = 1
          collide_block(dir, o1)
          (None, None)
        | _ =>
          collide_block(dir, o1)
          (None, None)
        }
      }
    | (_, _, _) => (None, None)
    }
  }

  /* Run the broad phase object filtering */
  let broad_phase = (collid, all_collids, state) => {
    let obj = get_obj(collid)
    all_collids->List.filter(c =>
      in_viewport(state.vpt, obj.pos) ||
      (is_player(collid) ||
      out_of_viewport_below(state.vpt, obj.pos.y))
    )
  }

  /* narrow_phase of collision is used in order to continuously loop through
   *each of the collidable objects to constantly check if collisions are
   *occurring. */
  let rec narrow_phase = (c, cs, state) => {
    let rec narrow_helper = (c, cs, state, acc) =>
      switch cs {
      | list{} => acc
      | list{h, ...t} =>
        let c_obj = get_obj(c)
        let new_objs = if !equals(c, h) {
          switch Object.check_collision(c, h) {
          | None => (None, None)
          | Some(dir) =>
            if get_obj(h).id != c_obj.id {
              /* ( (if (if is_rkoopa c then
            begin match c_obj.dir with
            | Left -> is_block c_obj.dir {x= c_obj.pos.x -. 16.; y= c_obj.pos.y -. 27.} cs
            | _ -> is_block c_obj.dir {x= c_obj.pos.x +. 16.; y= c_obj.pos.y -. 27.} cs
            end else false) then rev_dir c_obj RKoopa (Object.get_sprite c) else
            ());*/
              process_collision(dir, c, h, state)
            } else {
              (None, None)
            }
          }
        } else {
          (None, None)
        }
        let acc = switch new_objs {
        | (None, Some(o)) => list{o, ...acc}
        | (Some(o), None) => list{o, ...acc}
        | (Some(o1), Some(o2)) => list{o1, o2, ...acc}
        | (None, None) => acc
        }

        narrow_helper(c, t, state, acc)
      }
    narrow_helper(c, cs, state, list{})
  }

  /* This is an optimization setp to determine which objects require narrow phase
   * checking. This excludes static collidables, allowing collision to only be
   * checked with moving objects. This method is called once per collidable.
   * Collision detection proceeds as follows:
   * 1. Broad phase - filter collidables that cannot possibly collide with
   *    this object.
   * 2. Narrow phase - compare against all objects to determine whether there
   *    is a collision, and process the collision.
   * This method returns a list of objects that are created, which should be
   * added to the list of collidables for the next iteration.
   * */
  let check_collisions = (collid, all_collids, state) =>
    switch collid {
    | Block(_, _, _) => list{}
    | _ =>
      let broad = broad_phase(collid, all_collids, state)
      narrow_phase(collid, broad, state)
    }

  /* Returns whether the bounding box should be drawn */
  let check_bbox_enabled = () => pressed_keys.bbox == 1

  /* update_collidable is the primary update method for collidable objects,
   * checking the collision, updating the object, and drawing to the canvas. */
  let update_collidable = (state, collid: Object.collidable, all_collids) => {
    /* TODO: optimize. Draw static elements only once */
    let obj = Object.get_obj(collid)
    let spr = Object.get_sprite(collid)
    obj.invuln = if obj.invuln > 0 {
      obj.invuln - 1
    } else {
      0
    }
    /* Prevent position from being updated outside of viewport */
    let viewport_filter =
      in_viewport(state.vpt, obj.pos) ||
      (is_player(collid) ||
      out_of_viewport_below(state.vpt, obj.pos.y))
    if !obj.kill && viewport_filter {
      obj.grounded = false
      Object.process_obj(obj, state.map)
      /* Run collision detection if moving object */
      let evolved = check_collisions(collid, all_collids, state)
      /* Render and update animation */
      let vpt_adj_xy = coord_to_viewport(state.vpt, obj.pos)
      Draw.render(spr, (vpt_adj_xy.x, vpt_adj_xy.y))
      if check_bbox_enabled() {
        Draw.render_bbox(spr, (vpt_adj_xy.x, vpt_adj_xy.y))
      }

      if obj.vel.x != 0. || !is_enemy(collid) {
        Sprite.update_animation(spr)
      }
      evolved
    } else {
      list{}
    }
  }

  /* Converts a keypress to a list of control keys, allowing more than one key
   * to be processed each frame. */
  let translate_keys = () => {
    let k = pressed_keys
    let ctrls = list{(k.left, CLeft), (k.right, CRight), (k.up, CUp), (k.down, CDown)}
    ctrls->List.reduceReverse(list{}, (a, x) =>
      if fst(x) {
        list{snd(x), ...a}
      } else {
        a
      }
    )
  }

  /* run_update is used to update all of the collidables at once. Primarily used
   * as a wrapper method. This method is necessary to differentiate between
   * the player collidable and the remaining collidables, as special operations
   * such as viewport centering only occur with the player. */
  let run_update_collid = (state, collid, all_collids) =>
    switch collid {
    | Player(t, s, o) as p =>
      let keys = translate_keys()
      o.crouch = false
      let player = switch Object.update_player(o, keys, state.ctx) {
      | None => p
      | Some(new_typ, new_spr) =>
        Object.normalize_pos(o.pos, s.params, new_spr.params)
        Player(new_typ, new_spr, o)
      }
      let evolved = update_collidable(state, player, all_collids)
      collid_objs := \"@"(collid_objs.contents, evolved)
      player
    | _ =>
      let obj = get_obj(collid)
      let evolved = update_collidable(state, collid, all_collids)
      if !obj.kill {
        collid_objs := list{collid, ...\"@"(collid_objs.contents, evolved)}
      }
      let new_parts = if obj.kill {
        Object.kill(collid, state.ctx)
      } else {
        list{}
      }
      particles := \"@"(particles.contents, new_parts)
      collid
    }

  /* Primary update function to update and persist a particle */
  let run_update_particle = (state, part) => {
    Particle.process(part)
    let x = part.pos.x -. state.vpt.pos.x and y = part.pos.y -. state.vpt.pos.y
    Draw.render(part.params.sprite, (x, y))
    if !part.kill {
      particles := list{part, ...particles.contents}
    }
  }

  /* update_loop is constantly being called to check for collisions and to
   *update each of the objects in the game. */
  let update_loop = (canvas, (player, objs), map_dim) => {
    let scale = 1.
    let ctx = Dom_html.canvasElementToJsObj(canvas)["getContext"]("2d")
    let cwidth = float_of_int(Dom_html.canvasElementToJsObj(canvas)["width"]) /. scale
    let cheight = float_of_int(Dom_html.canvasElementToJsObj(canvas)["height"]) /. scale
    let viewport = Viewport.make((cwidth, cheight), map_dim)
    let state = {
      bgd: Sprite.make_bgd(ctx),
      vpt: Viewport.update(viewport, get_obj(player).pos),
      ctx,
      score: 0,
      coins: 0,
      multiplier: 1,
      map: snd(map_dim),
      game_over: false,
    }
    Dom_html.canvasRenderingContext2DToJsObj(state.ctx)["scale"](scale, scale)
    let rec update_helper = (time, state, player, objs, parts) =>
      if state.game_over == true {
        Draw.game_win(state.ctx)
      } else {
        collid_objs := list{}
        particles := list{}

        let fps = calc_fps(last_time.contents, time)
        last_time := time

        Draw.clear_canvas(canvas)

        /* Parallax background */
        let vpos_x_int = int_of_float(state.vpt.pos.x /. 5.)
        let bgd_width = int_of_float(fst(state.bgd.params.frame_size))
        Draw.draw_bgd(state.bgd, float_of_int(mod(vpos_x_int, bgd_width)))

        let player = run_update_collid(state, player, objs)

        if get_obj(player).kill == true {
          Draw.game_loss(state.ctx)
        } else {
          let state = {
            ...state,
            vpt: Viewport.update(state.vpt, get_obj(player).pos),
          }
          objs->List.forEach(obj => ignore(run_update_collid(state, obj, objs)))
          parts->List.forEach(part => run_update_particle(state, part))
          Draw.fps(canvas, fps)
          Draw.hud(canvas, state.score, state.coins)
          ignore(
            Dom_html.requestAnimationFrame((t: float) =>
              update_helper(t, state, player, collid_objs.contents, particles.contents)
            ),
          )
        }
      }
    update_helper(0., state, player, objs, list{})
  }

  /* Keydown event handler translates a key press */
  let keydown = evt => {
    let evt = Dom_html.keyboardEventToJsObj(evt)
    let () = switch evt["keyCode"] {
    | 38 | 32 | 87 => pressed_keys.up = true
    | 39 | 68 => pressed_keys.right = true
    | 37 | 65 => pressed_keys.left = true
    | 40 | 83 => pressed_keys.down = true
    | 66 => pressed_keys.bbox = mod(pressed_keys.bbox + 1, 2)
    | _ => ()
    }
    true
  }

  /* Keyup event handler translates a key release */
  let keyup = evt => {
    let evt = Dom_html.keyboardEventToJsObj(evt)
    let () = switch evt["keyCode"] {
    | 38 | 32 | 87 => pressed_keys.up = false
    | 39 | 68 => pressed_keys.right = false
    | 37 | 65 => pressed_keys.left = false
    | 40 | 83 => pressed_keys.down = false
    | _ => ()
    }
    true
  }
}
module Procedural_generator: {
  open Object
  open Actors

  type obj_coord

  let init: unit => unit

  /* Procedurally generates a new map of default size */
  let generate: (float, float, Dom_html.canvasRenderingContext2D) => (collidable, list<collidable>)
} = {
  open Actors
  open Object

  /* Note: Canvas is 512 by 256 (w*h) -> 32 by 16 blocks */

  /* Holds obj typ and its coordinates. (int, (x-coord, y-coord)) */
  type obj_coord = (int, (float, float))

  /* Checks if the given location checkloc is already part of the list of locations
   * in loclist. */
  let rec mem_loc = (checkloc: (float, float), loclist: list<obj_coord>): bool =>
    switch loclist {
    | list{} => false
    | list{h, ...t} =>
      if checkloc == snd(h) {
        true
      } else {
        mem_loc(checkloc, t)
      }
    }

  /* Converts list of locations from blocksize to pixelsize by multiplying (x,y) by
   * 16. */
  let rec convert_list = (lst: list<obj_coord>): list<obj_coord> =>
    switch lst {
    | list{} => list{}
    | list{h, ...t} =>
      \"@"(list{(fst(h), (fst(snd(h)) *. 16., snd(snd(h)) *. 16.))}, convert_list(t))
    }

  /* Chooses what type of enemy should be instantiated given typ number */
  let choose_enemy_typ = (typ: int): enemy_typ =>
    switch typ {
    | 0 => RKoopa
    | 1 => GKoopa
    | 2 => Goomba
    | _ => failwith("Shouldn't reach here")
    }

  /* Chooses what type of block should be instantiated given typ number */
  let choose_sblock_typ = (typ: int): block_typ =>
    switch typ {
    | 0 => Brick
    | 1 => UnBBlock
    | 2 => Cloud
    | 3 => QBlock(Mushroom)
    | 4 => Ground
    | _ => failwith("Shouldn't reach here")
    }

  /* Optimizes lst such that there are no two items in the list that have the same
   * coordinates. If there is one, it is removed. */
  let rec avoid_overlap = (lst: list<obj_coord>, currentLst: list<obj_coord>): list<obj_coord> =>
    switch lst {
    | list{} => list{}
    | list{h, ...t} =>
      if mem_loc(snd(h), currentLst) {
        avoid_overlap(t, currentLst)
      } else {
        \"@"(list{h}, avoid_overlap(t, currentLst))
      }
    }

  /* Gets rid of objects with coordinates in the ending frame, within 128 pixels of
   * the start, at the very top, and two blocks from the ground. */
  let rec trim_edges = (lst: list<obj_coord>, blockw: float, blockh: float): list<obj_coord> =>
    switch lst {
    | list{} => list{}
    | list{h, ...t} =>
      let cx = fst(snd(h))
      let cy = snd(snd(h))
      let pixx = blockw *. 16.
      let pixy = blockh *. 16.
      if cx < 128. || (pixx -. cx < 528. || (cy == 0. || pixy -. cy < 48.)) {
        trim_edges(t, blockw, blockh)
      } else {
        \"@"(list{h}, trim_edges(t, blockw, blockh))
      }
    }

  /* Generates a stair formation with block typ being dependent on typ. This type
   * of stair formation requires that the first step be on the ground. */
  let generate_ground_stairs = (cbx, cby, typ) => {
    let four = list{
      (typ, (cbx, cby)),
      (typ, (cbx +. 1., cby)),
      (typ, (cbx +. 2., cby)),
      (typ, (cbx +. 3., cby)),
    }
    let three = list{
      (typ, (cbx +. 1., cby -. 1.)),
      (typ, (cbx +. 2., cby -. 1.)),
      (typ, (cbx +. 3., cby -. 1.)),
    }
    let two = list{(typ, (cbx +. 2., cby -. 2.)), (typ, (cbx +. 3., cby -. 2.))}
    let one = list{(typ, (cbx +. 3., cby -. 3.))}
    \"@"(four, \"@"(three, \"@"(two, one)))
  }

  /* Generates a stair formation going upwards. */
  let generate_airup_stairs = (cbx, cby, typ) => {
    let one = list{(typ, (cbx, cby)), (typ, (cbx +. 1., cby))}
    let two = list{(typ, (cbx +. 3., cby -. 1.)), (typ, (cbx +. 4., cby -. 1.))}
    let three = list{
      (typ, (cbx +. 4., cby -. 2.)),
      (typ, (cbx +. 5., cby -. 2.)),
      (typ, (cbx +. 6., cby -. 2.)),
    }
    \"@"(one, \"@"(two, three))
  }

  /* Generates a stair formation going downwards */
  let generate_airdown_stairs = (cbx, cby, typ) => {
    let three = list{(typ, (cbx, cby)), (typ, (cbx +. 1., cby)), (typ, (cbx +. 2., cby))}
    let two = list{(typ, (cbx +. 2., cby +. 1.)), (typ, (cbx +. 3., cby +. 1.))}
    let one = list{(typ, (cbx +. 5., cby +. 2.)), (typ, (cbx +. 6., cby +. 2.))}
    \"@"(three, \"@"(two, one))
  }

  /* Generates a cloud block platform with some length num. */
  let rec generate_clouds = (cbx, cby, typ, num) =>
    if num == 0 {
      list{}
    } else {
      \"@"(list{(typ, (cbx, cby))}, generate_clouds(cbx +. 1., cby, typ, num - 1))
    }

  /* Generates an obj_coord list (typ, coordinates) of coins to be placed. */
  let rec generate_coins = (block_coord: list<obj_coord>): list<obj_coord> => {
    let place_coin = Random.int(2)
    switch block_coord {
    | list{} => list{}
    | list{h, ...t} =>
      if place_coin == 0 {
        let xc = fst(snd(h))
        let yc = snd(snd(h))
        \"@"(list{(0, (xc, yc -. 16.))}, generate_coins(t))
      } else {
        generate_coins(t)
      }
    }
  }

  /* Chooses the form of the blocks to be placed.
   * When called, leaves a 1 block gap from canvas size.
   * 1. If current xblock or yblock is greater than canvas width or height
   *    respectively, return an empty list.
   * 2. If current xblock or yblock is within 10 blocks of the left and right sides
   *    of the level map, prevent any objects from being initialized.
   * 3. Else call helper methods to created block formations and return obj_coord
   *    list.
   * */
  let choose_block_pattern = (
    blockw: float,
    blockh: float,
    cbx: float,
    cby: float,
    prob: int,
  ): list<obj_coord> =>
    if cbx > blockw || cby > blockh {
      list{}
    } else {
      let block_typ = Random.int(4)
      let stair_typ = Random.int(2)
      let life_block_chance = Random.int(5)
      let middle_block = if life_block_chance == 0 {
        3
      } else {
        stair_typ
      }
      let obj_coord = switch prob {
      | 0 =>
        if blockw -. cbx > 2. {
          list{
            (stair_typ, (cbx, cby)),
            (middle_block, (cbx +. 1., cby)),
            (stair_typ, (cbx +. 2., cby)),
          }
        } else if blockw -. cbx > 1. {
          list{(block_typ, (cbx, cby)), (block_typ, (cbx +. 1., cby))}
        } else {
          list{(block_typ, (cbx, cby))}
        }
      | 1 =>
        let num_clouds = Random.int(5) + 5
        if cby < 5. {
          generate_clouds(cbx, cby, 2, num_clouds)
        } else {
          list{}
        }
      | 2 =>
        if blockh -. cby == 1. {
          generate_ground_stairs(cbx, cby, stair_typ)
        } else {
          list{}
        }
      | 3 =>
        if stair_typ == 0 && blockh -. cby > 3. {
          generate_airdown_stairs(cbx, cby, stair_typ)
        } else if blockh -. cby > 2. {
          generate_airup_stairs(cbx, cby, stair_typ)
        } else {
          list{(stair_typ, (cbx, cby))}
        }
      | 4 =>
        if cby +. 3. -. blockh == 2. {
          list{(stair_typ, (cbx, cby))}
        } else if cby +. 3. -. blockh == 1. {
          list{(stair_typ, (cbx, cby)), (stair_typ, (cbx, cby +. 1.))}
        } else {
          list{
            (stair_typ, (cbx, cby)),
            (stair_typ, (cbx, cby +. 1.)),
            (stair_typ, (cbx, cby +. 2.)),
          }
        }
      | 5 => list{(3, (cbx, cby))}
      | _ => failwith("Shouldn't reach here")
      }
      obj_coord
    }

  /* Generates a list of enemies to be placed on the ground. */
  let rec generate_enemies = (
    blockw: float,
    blockh: float,
    cbx: float,
    cby: float,
    acc: list<obj_coord>,
  ) =>
    if cbx > blockw -. 32. {
      list{}
    } else if cby > blockh -. 1. || cbx < 15. {
      generate_enemies(blockw, blockh, cbx +. 1., 0., acc)
    } else if mem_loc((cbx, cby), acc) || cby == 0. {
      generate_enemies(blockw, blockh, cbx, cby +. 1., acc)
    } else {
      let prob = Random.int(30)
      let enem_prob = 3
      if prob < enem_prob && blockh -. 1. == cby {
        let enemy = list{(prob, (cbx *. 16., cby *. 16.))}
        \"@"(enemy, generate_enemies(blockw, blockh, cbx, cby +. 1., acc))
      } else {
        generate_enemies(blockw, blockh, cbx, cby +. 1., acc)
      }
    }

  /* Generates a list of enemies to be placed upon the block objects. */
  let rec generate_block_enemies = (block_coord: list<obj_coord>): list<obj_coord> => {
    let place_enemy = Random.int(20)
    let enemy_typ = Random.int(3)
    switch block_coord {
    | list{} => list{}
    | list{h, ...t} =>
      if place_enemy == 0 {
        let xc = fst(snd(h))
        let yc = snd(snd(h))
        \"@"(list{(enemy_typ, (xc, yc -. 16.))}, generate_block_enemies(t))
      } else {
        generate_block_enemies(t)
      }
    }
  }

  /* Generates an obj_coord list (typ, coordinates) of blocks to be placed. */
  let rec generate_block_locs = (
    blockw: float,
    blockh: float,
    cbx: float,
    cby: float,
    acc: list<obj_coord>,
  ): list<obj_coord> =>
    if blockw -. cbx < 33. {
      acc
    } else if cby > blockh -. 1. {
      generate_block_locs(blockw, blockh, cbx +. 1., 0., acc)
    } else if mem_loc((cbx, cby), acc) || cby == 0. {
      generate_block_locs(blockw, blockh, cbx, cby +. 1., acc)
    } else {
      let prob = Random.int(100)
      let block_prob = 5
      if prob < block_prob {
        let newacc = choose_block_pattern(blockw, blockh, cbx, cby, prob)
        let undup_lst = avoid_overlap(newacc, acc)
        let called_acc = \"@"(acc, undup_lst)
        generate_block_locs(blockw, blockh, cbx, cby +. 1., called_acc)
      } else {
        generate_block_locs(blockw, blockh, cbx, cby +. 1., acc)
      }
    }

  /* Generates the ending item panel at the end of the level. Games ends upon
   * collision with player. */
  let generate_panel = (
    context: Dom_html.canvasRenderingContext2D,
    blockw: float,
    blockh: float,
  ): collidable => {
    let ob = Object.spawn(
      SBlock(Panel),
      context,
      (blockw *. 16. -. 256., blockh *. 16. *. 2. /. 3.),
    )
    ob
  }

  /* Generates the list of brick locations needed to display the ground.
   * 1/10 chance that a ground block is skipped each call to create holes. */
  let rec generate_ground = (blockw: float, blockh: float, inc: float, acc: list<obj_coord>): list<
    obj_coord,
  > =>
    if inc > blockw {
      acc
    } else if inc > 10. {
      let skip = Random.int(10)
      let newacc = \"@"(acc, list{(4, (inc *. 16., blockh *. 16.))})
      if skip == 7 && blockw -. inc > 32. {
        generate_ground(blockw, blockh, inc +. 1., acc)
      } else {
        generate_ground(blockw, blockh, inc +. 1., newacc)
      }
    } else {
      let newacc = \"@"(acc, list{(4, (inc *. 16., blockh *. 16.))})
      generate_ground(blockw, blockh, inc +. 1., newacc)
    }

  /* Converts the obj_coord list called by generate_block_locs to a list of objects
   * with the coordinates given from the obj_coord list. */
  let rec convert_to_block_obj = (
    lst: list<obj_coord>,
    context: Dom_html.canvasRenderingContext2D,
  ): list<collidable> =>
    switch lst {
    | list{} => list{}
    | list{h, ...t} =>
      let sblock_typ = choose_sblock_typ(fst(h))
      let ob = Object.spawn(SBlock(sblock_typ), context, snd(h))
      \"@"(list{ob}, convert_to_block_obj(t, context))
    }

  /* Converts the obj_coord list called by generate_enemies to a list of objects
   * with the coordinates given from the obj_coord list. */
  let rec convert_to_enemy_obj = (
    lst: list<obj_coord>,
    context: Dom_html.canvasRenderingContext2D,
  ): list<collidable> =>
    switch lst {
    | list{} => list{}
    | list{h, ...t} =>
      let senemy_typ = choose_enemy_typ(fst(h))
      let ob = Object.spawn(SEnemy(senemy_typ), context, snd(h))
      \"@"(list{ob}, convert_to_enemy_obj(t, context))
    }

  /* Converts the list of coordinates into a list of Coin objects */
  let rec convert_to_coin_obj = (
    lst: list<obj_coord>,
    context: Dom_html.canvasRenderingContext2D,
  ): list<collidable> =>
    switch lst {
    | list{} => list{}
    | list{h, ...t} =>
      let sitem_typ = Coin
      let ob = Object.spawn(SItem(sitem_typ), context, snd(h))
      \"@"(list{ob}, convert_to_coin_obj(t, context))
    }

  /* Procedurally generates a list of collidables given canvas width, height and
   * context. Arguments block width (blockw) and block height (blockh) are in
   * block form, not pixels. */
  let generate_helper = (
    blockw: float,
    blockh: float,
    cx: float,
    cy: float,
    context: Dom_html.canvasRenderingContext2D,
  ): list<collidable> => {
    let block_locs = generate_block_locs(blockw, blockh, 0., 0., list{})
    let converted_block_locs = trim_edges(convert_list(block_locs), blockw, blockh)
    let obj_converted_block_locs = convert_to_block_obj(converted_block_locs, context)
    let ground_blocks = generate_ground(blockw, blockh, 0., list{})
    let obj_converted_ground_blocks = convert_to_block_obj(ground_blocks, context)
    let block_locations = \"@"(block_locs, ground_blocks)
    let all_blocks = \"@"(obj_converted_block_locs, obj_converted_ground_blocks)
    let enemy_locs = generate_enemies(blockw, blockh, 0., 0., block_locations)
    let obj_converted_enemies = convert_to_enemy_obj(enemy_locs, context)
    let coin_locs = generate_coins(converted_block_locs)
    let undup_coin_locs = trim_edges(avoid_overlap(coin_locs, converted_block_locs), blockw, blockh)
    let converted_block_coin_locs = \"@"(converted_block_locs, coin_locs)
    let enemy_block_locs = generate_block_enemies(converted_block_locs)
    let undup_enemy_block_locs = avoid_overlap(enemy_block_locs, converted_block_coin_locs)
    let obj_enemy_blocks = convert_to_enemy_obj(undup_enemy_block_locs, context)
    let coin_objects = convert_to_coin_obj(undup_coin_locs, context)
    let obj_panel = generate_panel(context, blockw, blockh)
    \"@"(
      all_blocks,
      \"@"(obj_converted_enemies, \"@"(coin_objects, \"@"(obj_enemy_blocks, list{obj_panel}))),
    )
  }

  /* Main function called to procedurally generate the level map. w and h args
   * are in pixel form. Converts to block form to call generate_helper. Spawns
   * the list of collidables received from generate_helper to display on canvas. */
  let generate = (w: float, h: float, context: Dom_html.canvasRenderingContext2D): (
    collidable,
    list<collidable>,
  ) => {
    let blockw = w /. 16.
    let blockh = h /. 16. -. 1.
    let collide_list = generate_helper(blockw, blockh, 0., 0., context)
    let player = Object.spawn(SPlayer(SmallM, Standing), context, (100., 224.))
    (player, collide_list)
  }

  /* Makes sure level map is uniquely generated at each call. */
  let init = () => Random.self_init()
}
module Main = {
  open Actors
  open Sprite
  open Object
  module Html = Dom_html
  module Pg = Procedural_generator

  let loadCount = ref(0)
  let imgsToLoad = 4
  let level_width = 2400.
  let level_height = 256.

  /* Canvas is chosen from the index.html file. The context is obtained from
   *the canvas. Listeners are added. A level is generated and the general
   *update_loop method is called to make the level playable. */
  let load = _ => {
    Random.self_init()
    let canvas_id = "canvas"
    let canvas = switch Dom_html.getElementById(Dom_html.document, canvas_id) {
    | None =>
      Console.log("cant find canvas " ++ canvas_id ++ "")
      failwith("fail")
    | Some(el) => Dom_html.elementToCanvasElement(el)
    }

    let context = Dom_html.canvasElementToJsObj(canvas)["getContext"]("2d")
    let _ = Dom_html.addEventListener(Dom_html.document, "keydown", Director.keydown, true)
    let _ = Dom_html.addEventListener(Dom_html.document, "keyup", Director.keyup, true)
    let () = Pg.init()
    let _ = Director.update_loop(
      canvas,
      Pg.generate(level_width, level_height, context),
      (level_width, level_height),
    )
    Console.log("asd")
    ()
  }

  let inc_counter = _ => {
    loadCount := loadCount.contents + 1
    if loadCount.contents == imgsToLoad {
      load()
    } else {
      ()
    }
  }

  /* Used for concurrency issues. */
  let preload = _ => {
    let root_dir = "sprites/"
    let imgs = list{"blocks.png", "items.png", "enemies.png", "mario-small.png"}
    imgs->List.map(img_src => {
      let img_src = root_dir ++ img_src
      let img = Html.createImg(Dom_html.document)
      Dom_html.imageElementToJsObj(img)["src"] = img_src
      ignore(
        Dom_html.addEventListenerImg(
          img,
          "load",
          ev => {
            inc_counter()
            true
          },
          true,
        ),
      )
    })
  }

  let _ = Dom_html.windowToJsObj(Dom_html.window)["onload"] = _ => {
    ignore(preload())
    true
  }
}
