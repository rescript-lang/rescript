open Actors

type viewport = {
  pos: Actors.xy,
  v_dim: Actors.xy,
  m_dim: Actors.xy,
}

let make = ((vx, vy), (mx, my)) => {
  pos: {
    x: 0.,
    y: 0.,
  },
  v_dim: {
    x: vx,
    y: vy,
  },
  m_dim: {
    x: mx,
    y: my,
  },
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
  {...vpt, pos: pos}
}
