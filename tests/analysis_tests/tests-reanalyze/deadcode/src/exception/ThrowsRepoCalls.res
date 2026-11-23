// Calls into ThrowsRepo, whose annotations name exceptions of ThrowsErrors.

let catchFind = x =>
  try ThrowsRepo.find(x) catch {
  | ThrowsErrors.NotFound => 0
  }

let catchFindExternal = x =>
  try ThrowsRepo.findExternal(x) catch {
  | ThrowsErrors.NotFound => 0
  }

let catchViaAlias = x =>
  try ThrowsRepo.viaAlias(x) catch {
  | ThrowsErrors.NotFound => 0
  }

let catchViaOpen = x =>
  try ThrowsRepo.viaOpen(x) catch {
  | ThrowsErrors.Bad(_) => 0
  }

@throws(ThrowsErrors.NotFound)
let annotated = x => ThrowsRepo.find(x)

open ThrowsErrors

@throws(NotFound)
let annotatedViaOpen = x => ThrowsRepo.find(x)
