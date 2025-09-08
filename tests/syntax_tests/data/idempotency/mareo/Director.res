open Belt

open Sprite

open Object

open Actors

/* open Viewport */
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
  vpt: Viewport.viewport,
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
  ignore(s1)
  o1.invuln = 10
  o1.jumping = false
  o1.grounded = true
  switch typ {
  | GKoopaShell
  | RKoopaShell =>
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
let enemy_attack_player = (s1, o1: Object.obj, t2, s2, o2: Object.obj, context) => {
  ignore(s1)
  switch t2 {
  | GKoopaShell
  | RKoopaShell =>
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
  | (RKoopaShell, _)
  | (GKoopaShell, _) =>
    if o1.vel.x == 0. {
      rev_dir(o2, t2, s2)
      (None, None)
    } else {
      dec_health(o2)
      (None, None)
    }
  | (_, RKoopaShell)
  | (_, GKoopaShell) =>
    if o2.vel.x == 0. {
      rev_dir(o1, t1, s1)
      (None, None)
    } else {
      dec_health(o1)
      (None, None)
    }
  | (_, _) =>
    switch dir {
    | West
    | East =>
      rev_dir(o1, t1, s1)
      rev_dir(o2, t2, s2)
      (None, None)
    | _ => (None, None)
    }
  }

/* Gets the object at a given position */
/* let obj_at_pos dir (pos: xy) (collids: Object.collidable list)
                                             : Object.collidable list =
   match dir with
   | Left -> List.filter (fun (col: Object.collidable) ->
       (get_obj col).pos.y = pos.y && (get_obj col).pos.x = pos.x -. 16.)
             collids
   | _ -> List.filter (fun (col: Object.collidable) ->
       (get_obj col).pos.y = pos.y && (get_obj col).pos.x = pos.x +. 16.)
             collids */
/* Returns whether the object at a given position is a block */
/* let is_block dir pos collids =
   match obj_at_pos dir pos collids with
   | [] -> false
   | [Block (_,_,_)] -> true
   | _ -> false */
/* Returns whether the given object is a red koopa */
/* let is_rkoopa collid =
   match collid with
   | Enemy(RKoopa,_,_) -> true
   | _ -> false */
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
) => {
  let context = state.ctx
  switch (c1, c2, dir) {
  | (Player(_, s1, o1), Enemy(typ, s2, o2), South)
  | (Enemy(typ, s2, o2), Player(_, s1, o1), North) =>
    player_attack_enemy(s1, o1, typ, s2, o2, state, context)
  | (Player(_, s1, o1), Enemy(t2, s2, o2), _)
  | (Enemy(t2, s2, o2), Player(_, s1, o1), _) =>
    enemy_attack_player(s1, o1, t2, s2, o2, context)
  | (Player(_, _, o1), Item(t2, _, o2), _)
  | (Item(t2, _, o2), Player(_, _, o1), _) =>
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
  | (Enemy(t1, s1, o1), Block(t2, _, o2), East)
  | (Enemy(t1, s1, o1), Block(t2, _, o2), West) =>
    switch (t1, t2) {
    | (RKoopaShell, Brick)
    | (GKoopaShell, Brick) =>
      dec_health(o2)
      reverse_left_right(o1)
      (None, None)
    | (RKoopaShell, QBlock(typ))
    | (GKoopaShell, QBlock(typ)) =>
      let updated_block = evolve_block(o2, context)
      let spawned_item = spawn_above(o1.dir, o2, typ, context)
      rev_dir(o1, t1, s1)
      (Some(updated_block), Some(spawned_item))
    | (_, _) =>
      rev_dir(o1, t1, s1)
      (None, None)
    }
  | (Item(_, _, o1), Block(_, _, _), East)
  | (Item(_, _, o1), Block(_, _, _), West) =>
    reverse_left_right(o1)
    (None, None)
  | (Enemy(_, _, o1), Block(_, _, _), _)
  | (Item(_, _, o1), Block(_, _, _), _) =>
    collide_block(dir, o1)
    (None, None)
  | (Player(t1, _, o1), Block(t, _, o2), North) =>
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
  | (Player(_, _, o1), Block(t, _, _), _) =>
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
  List.keep(all_collids, _c =>
    Viewport.in_viewport(state.vpt, obj.pos) ||
    (is_player(collid) ||
    Viewport.out_of_viewport_below(state.vpt, obj.pos.y))
  )
}

/* narrow_phase of collision is used in order to continuously loop through
 *each of the collidable objects to constantly check if collisions are
 *occurring. */
let narrow_phase = (c, cs, state) => {
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
    Viewport.in_viewport(state.vpt, obj.pos) ||
    (is_player(collid) ||
    Viewport.out_of_viewport_below(state.vpt, obj.pos.y))
  if !obj.kill && viewport_filter {
    obj.grounded = false
    Object.process_obj(obj, state.map)
    /* Run collision detection if moving object */
    let evolved = check_collisions(collid, all_collids, state)
    /* Render and update animation */
    let vpt_adj_xy = Viewport.coord_to_viewport(state.vpt, obj.pos)
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
  List.reduce(ctrls, list{}, (a, x) =>
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
  | Player(_, s, o) as p =>
    let keys = translate_keys()
    o.crouch = false
    let player = switch Object.update_player(o, keys, state.ctx) {
    | None => p
    | Some((new_typ, new_spr)) =>
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
  let x = part.pos.x -. state.vpt.pos.x
  and y = part.pos.y -. state.vpt.pos.y
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
    ctx: ctx,
    score: 0,
    coins: 0,
    multiplier: 1,
    map: snd(map_dim),
    game_over: false,
  }
  ignore(Dom_html.canvasRenderingContext2DToJsObj(state.ctx)["scale"](scale, scale))
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
        List.forEach(objs, obj => ignore(run_update_collid(state, obj, objs)))
        List.forEach(parts, part => run_update_particle(state, part))
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
  | 38
  | 32
  | 87 =>
    pressed_keys.up = true
  | 39
  | 68 =>
    pressed_keys.right = true
  | 37
  | 65 =>
    pressed_keys.left = true
  | 40
  | 83 =>
    pressed_keys.down = true
  | 66 => pressed_keys.bbox = mod(pressed_keys.bbox + 1, 2)
  | _ => ()
  }
  true
}

/* Keyup event handler translates a key release */
let keyup = evt => {
  let evt = Dom_html.keyboardEventToJsObj(evt)
  let () = switch evt["keyCode"] {
  | 38
  | 32
  | 87 =>
    pressed_keys.up = false
  | 39
  | 68 =>
    pressed_keys.right = false
  | 37
  | 65 =>
    pressed_keys.left = false
  | 40
  | 83 =>
    pressed_keys.down = false
  | _ => ()
  }
  true
}
