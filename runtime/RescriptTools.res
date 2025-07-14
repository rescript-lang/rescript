module Docgen = RescriptTools_Docgen
module ExtractCodeBlocks = RescriptTools_ExtractCodeBlocks

/** Returns the full file system path to the `rescript-tools` binary for the current platform, side stepping the JS that wraps the CLI.
 
You can use this when you're already running a JS process and want to avoid the overhead of starting another one.

## Examples

```rescript
// Prints the current ReScript Tools version.
let stringifiedJson = ChildProcess.execFileSync(RescriptTools.binaryPath, ["-v"])
```
*/
@module("#cli/bins")
external binaryPath: string = "rescript_tools_exe"
