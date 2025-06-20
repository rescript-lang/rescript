type preRelease = Alpha(int) | Beta(int) | Dev(int) | Rc(int)

type t = {major: int, minor: int, patch: int, preRelease: option<preRelease>}

/**
  Takes a `version` string starting with a "v" and ending in major.minor.patch or
  major.minor.patch-prerelease.identifier (e.g. "v10.1.0" or "v10.1.0-alpha.2")
  */
let parse = (versionStr: string) => {
  let parsePreRelease = str => {
    switch str->String.split("-") {
    | [_, identifier] =>
      switch identifier->String.split(".") {
      | [name, number] =>
        switch Int.fromString(number) {
        | None => None
        | Some(buildIdentifier) =>
          switch name {
          | "dev" => buildIdentifier->Dev->Some
          | "beta" => buildIdentifier->Beta->Some
          | "alpha" => buildIdentifier->Alpha->Some
          | "rc" => buildIdentifier->Rc->Some
          | _ => None
          }
        }
      | _ => None
      }
    | _ => None
    }
  }

  // Some version contain a suffix. Example: v11.0.0-alpha.5, v11.0.0-beta.1
  let isPrerelease = versionStr->String.search(/-/) != -1

  // Get the first part i.e vX.Y.Z
  let versionNumber = versionStr->String.split("-")->Array.get(0)->Option.getOr(versionStr)

  switch versionNumber->String.replace("v", "")->String.split(".") {
  | [major, minor, patch] =>
    switch (major->Int.fromString, minor->Int.fromString, patch->Int.fromString) {
    | (Some(major), Some(minor), Some(patch)) =>
      let preReleaseIdentifier = if isPrerelease {
        parsePreRelease(versionStr)
      } else {
        None
      }
      Some({major, minor, patch, preRelease: preReleaseIdentifier})
    | _ => None
    }
  | _ => None
  }
}

let toString = ({major, minor, patch, preRelease}) => {
  let mainVersion = `v${major->Int.toString}.${minor->Int.toString}.${patch->Int.toString}`

  switch preRelease {
  | None => mainVersion
  | Some(identifier) =>
    let identifier = switch identifier {
    | Dev(number) => `dev.${number->Int.toString}`
    | Alpha(number) => `alpha.${number->Int.toString}`
    | Beta(number) => `beta.${number->Int.toString}`
    | Rc(number) => `rc.${number->Int.toString}`
    }

    `${mainVersion}-${identifier}`
  }
}

let tryGetMajorString = (versionStr: string) =>
  switch versionStr->parse {
  | None => versionStr // fallback to given version if it cannot be parsed
  | Some({major}) => "v" ++ major->Int.toString
  }
