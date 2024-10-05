//@ts-check
var os = require("os");
var arg = require("./rescript_arg.js");

var format_usage = `Usage: rescript format <options> [files]

\`rescript format\` formats the current directory
`;
var child_process = require("child_process");
var util = require("util");
var asyncExecFile = util.promisify(child_process.execFile);
var path = require("path");
var fs = require("fs");
var asyncFs = fs.promises;
/**
 * @type {arg.stringref}
 */
var stdin = { val: undefined };

/**
 * @type {arg.boolref}
 */
var format = { val: undefined };

/**
 * @type {arg.boolref}
 */
var check = { val: undefined };

/**
 * @type{arg.specs}
 */
var specs = [
  [
    "-stdin",
    { kind: "String", data: { kind: "String_set", data: stdin } },
    `[.res|.resi|.ml|.mli] Read the code from stdin and print
the formatted code to stdout in ReScript syntax`,
  ],
  [
    "-all",
    { kind: "Unit", data: { kind: "Unit_set", data: format } },
    "Format the whole project ",
  ],
  [
    "-check",
    { kind: "Unit", data: { kind: "Unit_set", data: check } },
    "Check formatting for file or the whole project. Use `-all` to check the whole project",
  ],
];
var formattedStdExtensions = [".res", ".resi", ".ml", ".mli"];
var formattedFileExtensions = [".res", ".resi"];

/**
 *
 * @param {string[]} extensions
 */
function hasExtension(extensions) {
  /**
   * @param {string} x
   */
  var pred = x => extensions.some(ext => x.endsWith(ext));
  return pred;
}
async function readStdin() {
  var stream = process.stdin;
  const chunks = [];
  for await (const chunk of stream) chunks.push(chunk);
  return Buffer.concat(chunks).toString("utf8");
}

const numThreads = os.cpus().length;

/**
 * Splits an array into smaller chunks of a specified size.
 *
 * @template T
 * @param {T[]} array - The array to split into chunks.
 * @param {number} chunkSize - The size of each chunk.
 * @returns {T[][]} - An array of chunks, where each chunk is an array of type T.
 */
function chunkArray(array, chunkSize) {
  /** @type {T[][]} */
  const result = [];

  for (let i = 0; i < array.length; i += chunkSize) {
    result.push(array.slice(i, i + chunkSize));
  }

  return result;
}

/**
 * @param {string[]} files
 * @param {string} bsc_exe
 * @param {(x: string) => boolean} isSupportedFile
 * @param {boolean} checkFormatting
 */
async function formatFiles(files, bsc_exe, isSupportedFile, checkFormatting) {
  const supportedFiles = files.filter(isSupportedFile);
  const batchSize = 4 * os.cpus().length;
  const batches = chunkArray(supportedFiles, batchSize);

  let incorrectlyFormattedFiles = 0;
  try {
    for (const batch of batches) {
      await Promise.all(
        batch.map(async file => {
          const flags = checkFormatting
            ? ["-format", file]
            : ["-o", file, "-format", file];
          const { stdout } = await asyncExecFile(bsc_exe, flags);
          if (check.val) {
            const original = await asyncFs.readFile(file, "utf-8");
            if (original != stdout) {
              console.error("[format check]", file);
              incorrectlyFormattedFiles++;
            }
          }
        })
      );
    }
  } catch (err) {
    console.error(err);
    process.exit(2);
  }
  if (incorrectlyFormattedFiles > 0) {
    if (incorrectlyFormattedFiles == 1) {
      console.error("The file listed above needs formatting");
    } else {
      console.error(
        `The ${incorrectlyFormattedFiles} files listed above need formatting`
      );
    }
    process.exit(3);
  }
}

/**
 * @param {string[]} argv
 * @param {string} rescript_exe
 * @param {string} bsc_exe
 */
async function main(argv, rescript_exe, bsc_exe) {
  var isSupportedFile = hasExtension(formattedFileExtensions);
  var isSupportedStd = hasExtension(formattedStdExtensions);

  try {
    /**
     * @type {string[]}
     */
    var files = [];
    arg.parse_exn(format_usage, argv, specs, xs => {
      files = xs;
    });

    var format_project = format.val;
    var use_stdin = stdin.val;

    // Only -check arg
    // Require: -all or path to a file
    if (check.val && !format_project && files.length == 0) {
      console.error(
        "format check require path to a file or use `-all` to check the whole project"
      );
      process.exit(2);
    }

    if (format_project) {
      if (use_stdin || files.length !== 0) {
        console.error("format -all can not be in use with other flags");
        process.exit(2);
      }
      // -all
      // TODO: check the rest arguments
      var output = child_process.spawnSync(
        rescript_exe,
        ["info", "-list-files"],
        {
          encoding: "utf-8",
        }
      );
      if (output.status !== 0) {
        console.error(output.stdout);
        console.error(output.stderr);
        process.exit(2);
      }
      files = output.stdout.split("\n").map(x => x.trim());
      await formatFiles(files, bsc_exe, isSupportedFile, check.val);
    } else if (use_stdin) {
      if (check.val) {
        console.error("format -stdin cannot be used with -check flag");
        process.exit(2);
      }
      if (isSupportedStd(use_stdin)) {
        var crypto = require("crypto");
        var os = require("os");
        var filename = path.join(
          os.tmpdir(),
          "rescript_" +
            crypto.randomBytes(8).toString("hex") +
            path.parse(use_stdin).base
        );
        (async function () {
          var content = await readStdin();
          var fd = fs.openSync(filename, "wx", 0o600); // Avoid overwriting existing file
          fs.writeFileSync(fd, content, "utf8");
          fs.closeSync(fd);
          process.addListener("exit", () => fs.unlinkSync(filename));
          child_process.execFile(
            bsc_exe,
            ["-format", filename],
            (error, stdout, stderr) => {
              if (error === null) {
                process.stdout.write(stdout);
              } else {
                console.error(stderr);
                process.exit(2);
              }
            }
          );
        })();
      } else {
        console.error(`Unsupported extension ${use_stdin}`);
        console.error(`Supported extensions: ${formattedStdExtensions} `);
        process.exit(2);
      }
    } else {
      if (files.length === 0) {
        // none of argumets set
        // format the current directory
        files = fs.readdirSync(process.cwd()).filter(isSupportedFile);
      }

      for (let i = 0; i < files.length; ++i) {
        let file = files[i];
        if (!isSupportedStd(file)) {
          console.error(`Don't know what do with ${file}`);
          console.error(`Supported extensions: ${formattedFileExtensions}`);
          process.exit(2);
        }
      }
      await formatFiles(files, bsc_exe, isSupportedFile, check.val);
    }
  } catch (e) {
    if (e instanceof arg.ArgError) {
      console.error(e.message);
      process.exit(2);
    } else {
      throw e;
    }
  }
}
exports.main = main;
