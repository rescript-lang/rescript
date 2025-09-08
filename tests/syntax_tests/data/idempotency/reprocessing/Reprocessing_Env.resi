let width: Reprocessing_Types.Types.glEnvT => int

let height: Reprocessing_Types.Types.glEnvT => int

let mouse: Reprocessing_Types.Types.glEnvT => (int, int)

let pmouse: Reprocessing_Types.Types.glEnvT => (int, int)

let mousePressed: Reprocessing_Types.Types.glEnvT => bool

let keyCode: Reprocessing_Types.Types.glEnvT => Reprocessing_Events.keycodeT

let key: (Reprocessing_Common.KeySet.elt, Reprocessing_Common.glEnv) => bool

let keyPressed: (Reprocessing_Common.KeySet.elt, Reprocessing_Common.glEnv) => bool

let keyReleased: (Reprocessing_Common.KeySet.elt, Reprocessing_Common.glEnv) => bool

let size: (~width: int, ~height: int, Reprocessing_Types.Types.glEnvT) => unit

let title: (string, Reprocessing_Types.Types.glEnvT) => unit

let resizeable: (bool, Reprocessing_Types.Types.glEnvT) => unit

let frameRate: Reprocessing_Types.Types.glEnvT => int

let frameCount: Reprocessing_Types.Types.glEnvT => int

let loadSound: (string, Reprocessing_Types.Types.glEnvT) => Reprocessing_Types.Types.soundT

let playSound: (
  Reprocessing_Types.Types.soundT,
  ~volume: float=?,
  ~loop: bool=?,
  Reprocessing_Types.Types.glEnvT,
) => unit

/* ** Time in seconds since the last frame */
let deltaTime: Reprocessing_Types.Types.glEnvT => float

/* **
 Localize a point in canvas coordinates to the current env's
 transformed coordinates
 */
let localizePoint: ((int, int), Reprocessing_Types.Types.glEnvT) => (int, int)

let localizePointf: ((float, float), Reprocessing_Types.Types.glEnvT) => (float, float)
