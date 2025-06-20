/* **
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

open GenTypeCommon

let version = Version.version

let signFile = s => s

type cliCommand =
  | Add(string)
  | Clean
  | DCE(option<string>)
  | NoOp
  | Rm(string)
  | Termination(option<string>)

let cli = () => {
  let bsVersion = ref(None)
  let cliCommand = ref(NoOp)
  let setBsVersion = s => bsVersion := Some(s)
  let usage = "genType version " ++ version
  let versionAndExit = () => {
    print_endline(usage)
    exit(0)
  }
  let rec printUsageAndExit = () => {
    Arg.usage(speclist, usage)
    exit(0)
  }
  and setCliCommand = command => {
    if cliCommand.contents != NoOp {
      printUsageAndExit()
    }
    cliCommand := command
  }
  and setAdd = s => Add(s)->setCliCommand
  and setRm = s => Rm(s)->setCliCommand
  and setClean = () => Clean->setCliCommand
  and setDCE = cmtRoot => DCE(cmtRoot)->setCliCommand
  and setTermination = cmtRoot => Termination(cmtRoot)->setCliCommand
  and speclist = list{
    ("-bs-version", Arg.String(setBsVersion), "set the bucklescript version"),
    ("-clean", Arg.Unit(setClean), "clean all the generated files"),
    ("-cmt-add", Arg.String(setAdd), "compile a .cmt[i] file"),
    ("-cmt-rm", Arg.String(setRm), "remove a .cmt[i] file"),
    ("-dce", Arg.Unit(() => setDCE(None)), "experimental DCE"),
    (
      "-dce-cmt",
      Arg.String(s => setDCE(Some(s))),
      "root_path experimental DCE for all the .cmt files under the root path",
    ),
    ("-termination", Arg.Unit(() => setTermination(None)), "experimental termination"),
    (
      "-termination-cmt",
      Arg.String(s => setTermination(Some(s))),
      "root_path experimental termination for all the .cmt files under the root path",
    ),
    ("-version", Arg.Unit(versionAndExit), "show version information and exit"),
    ("--version", Arg.Unit(versionAndExit), "show version information and exit"),
  }

  let executeCliCommand = (~bsVersion, cliCommand) =>
    switch cliCommand {
    | Add(s) =>
      Log_.Color.forceColor := true
      let splitColon = Str.split(Str.regexp(":"), s)
      let (cmt, mlast) = switch splitColon {
      | list{cmt, ...rest} =>
        let mlast = rest->String.concat("")
        (cmt, mlast)
      | _ => assert(false)
      }
      let config = Paths.readConfig(~bsVersion, ~namespace=cmt->Paths.findNameSpace)
      if Debug.basic.contents {
        Log_.item("Add %s  %s\n", cmt, mlast)
      }
      cmt->GenTypeMain.processCmtFile(~signFile, ~config)
      exit(0)

    | Clean =>
      let config = Paths.readConfig(~bsVersion, ~namespace=None)
      let sourceDirs = ModuleResolver.readSourceDirs(~configSources=config.sources)
      if Debug.basic.contents {
        Log_.item("Clean %d dirs\n", sourceDirs.dirs->List.length)
      }
      let count = ref(0)
      sourceDirs.dirs->List.iter(dir => {
        let files = Sys.readdir(dir)
        files->Array.iter(file =>
          if Filename.check_suffix(file, ".re") {
            let extension = EmitType.outputFileSuffix(~config)
            let generated = Filename.concat(dir, (file->Filename.chop_extension) ++ extension)
            if Sys.file_exists(generated) {
              Unix.unlink(generated)
              incr(count)
            }
          }
        )
      })
      if Debug.basic.contents {
        Log_.item("Cleaned %d files\n", count.contents)
      }
      exit(0)

    | NoOp => printUsageAndExit()

    | Rm(s) =>
      let splitColon = Str.split(Str.regexp(":"), s)->Array.of_list
      assert (Array.length(splitColon) === 1)
      let cmtAbsolutePath: string = splitColon[0]
      /* somehow the CMT hook is passing an absolute path here */
      let cmt = cmtAbsolutePath->Paths.relativePathFromBsLib
      let config = Paths.readConfig(~bsVersion, ~namespace=cmt->Paths.findNameSpace)
      let outputFile = cmt->Paths.getOutputFile(~config)
      if Debug.basic.contents {
        Log_.item("Remove %s\n", cmt)
      }
      if Sys.file_exists(outputFile) {
        Unix.unlink(outputFile)
      }
      exit(0)

    | DCE(cmtRoot) => DeadCode.runAnalysis(~cmtRoot)
    | Termination(cmtRoot) => DeadCode.runTerminationAnalysis(~cmtRoot)
    }

  Arg.parse(speclist, print_endline, usage)

  executeCliCommand(~bsVersion=bsVersion.contents, cliCommand.contents)
}

cli()
