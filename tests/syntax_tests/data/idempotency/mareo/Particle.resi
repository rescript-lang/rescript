/* Template params associated with a particle */
type part_params = {
  sprite: Sprite.sprite /* Backing sprite */,
  rot: float /* Rotation */,
  lifetime: int /* Life span */,
}

type particle = {
  params: part_params,
  part_type: Actors.part_typ,
  pos: Actors.xy,
  vel: Actors.xy,
  acc: Actors.xy,
  mutable kill: bool /* Kill the particle in the next frame */,
  mutable life: int /* Remaining lifespan of particle */,
}

/* Makes a new particle of the given particle type with at a position. */
let make: (
  ~vel: (float, float)=?,
  ~acc: (float, float)=?,
  Actors.part_typ,
  (float, float),
  Dom_html.canvasRenderingContext2D,
) => particle

/* Make a score particle. The first int indicates the score to spawn */
let make_score: (int, (float, float), Dom_html.canvasRenderingContext2D) => particle

/* Process a particle, updating its velocity and position. Also marks it as
 * killable if it exceeds its lifespan */
let process: particle => unit
