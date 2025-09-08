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
    img_src: img_src,
    max_frames: max_frames,
    max_ticks: max_ticks,
    frame_size: frame_size,
    src_offset: src_offset,
    bbox_offset: bbox_offset,
    bbox_size: bbox_size,
    loop: loop,
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
    setup_sprite("enemies.png", ~bb_off=(1., 1.), ~bb_sz=(14., 14.), 2, 10, (16., 16.), (0., 128.))
  | (GKoopa, Left) =>
    setup_sprite("enemies.png", ~bb_off=(4., 10.), ~bb_sz=(11., 16.), 2, 10, (16., 27.), (0., 69.))
  | (GKoopa, Right) =>
    setup_sprite("enemies.png", ~bb_off=(1., 10.), ~bb_sz=(11., 16.), 2, 10, (16., 27.), (32., 69.))
  | (RKoopa, Left) =>
    setup_sprite("enemies.png", ~bb_off=(4., 10.), ~bb_sz=(11., 16.), 2, 10, (16., 27.), (0., 5.))
  | (RKoopa, Right) =>
    setup_sprite("enemies.png", ~bb_off=(1., 10.), ~bb_sz=(11., 16.), 2, 10, (16., 27.), (32., 5.))
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
  {params: params, context: context, img: img, frame: ref(0), ticks: ref(0)}
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
