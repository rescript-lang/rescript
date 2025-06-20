type fileAction =
  | NoMatch /* No @genType annotation found. */
  | Replace /* Replace existing file on disk with new contents. */
  | Identical /* File already on disk with identical contents. Skip. */
  | TypeError /* The cmt file was produced after a type error -- don't delete generated files. */
  | Write /* File not present on disk. */

let logFileAction = (fileAction, fileName) =>
  if Debug.basic.contents {
    Log_.item(
      "%s  %s\n",
      switch fileAction {
      | NoMatch => "NoMatch"
      | Replace => "Replace"
      | Identical => "Identical"
      | TypeError => "TypeError"
      | Write => "Write"
      },
      fileName,
    )
  }

let readLines = (file: string): list<string> => {
  let lines = ref(list{})
  let chan = open_in(file)
  let finished_lines = try {
    while true {
      lines := list{input_line(chan), ...lines.contents}
    }
    list{}
  } catch {
  | End_of_file =>
    close_in(chan)
    lines.contents->List.rev
  }
  finished_lines
}

let readFile = (file: string): string => String.concat("\n", readLines(file))

let writeFile = (filePath: string, contents: string) => {
  let outFile = open_out(filePath)
  output_string(outFile, contents)
  close_out(outFile)
}

let writeFileIfRequired = (~outputFile, ~fileContents) =>
  if Sys.file_exists(outputFile) {
    let oldContents = readFile(outputFile)
    let identical = oldContents == fileContents
    if identical {
      outputFile->logFileAction(Identical)
    } else {
      outputFile->logFileAction(Replace)
      writeFile(outputFile, fileContents)
    }
  } else {
    outputFile->logFileAction(Write)
    writeFile(outputFile, fileContents)
  }
