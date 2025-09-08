open Common

// This is a map of the sound name to the volume level.
let names = list{
  ("drop", 1.0),
  ("pickup", 1.0),
  ("win", 0.1),
  ("lose", 0.1),
  ("moving_boulder", 0.05),
  ("rock_crack", 0.1),
  ("background_tunes", 0.1),
}

let load = env => {
  let loadSoundHelper = (sounds, (name: string, volume)) =>
    StringMap.add(
      name,
      (Reprocessing.Env.loadSound(Printf.sprintf("assets/sounds/%s.wav", name), env), volume),
      sounds,
    )
  List.fold_left(loadSoundHelper, StringMap.empty, names)
}

let play = (name, state, ~loop=false, env) =>
  switch StringMap.find(name, state.soundData) {
  | (s, volume) => Reprocessing.Env.playSound(s, ~loop, ~volume, env)
  | exception Not_found => print_endline("Couldn't find sound " ++ name)
  }
