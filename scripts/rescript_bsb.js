//@ts-check

const fs = require("fs");
const path = require("path");
var os = require("os");
const child_process = require("child_process");
const rescript_exe = require("./bin_path").rescript_exe;

const cwd = process.cwd();
const lockFileName = path.join(cwd, ".bsb.lock");

/**
 * @type {child_process.ChildProcess | null}
 */
let ownerProcess = null;
function releaseBuild() {
  if (ownerProcess) {
    try {
      ownerProcess.kill();
      fs.unlinkSync(lockFileName);
    } catch {}
    ownerProcess = null;
  }
}

/**
 * We use [~perm:0o664] rather than our usual default perms, [0o666], because
 * lock files shouldn't rely on the umask to disallow tampering by other.
 *
 * @param {Array<string>} args
 * @param {child_process.SpawnOptions} [options]
 */
function acquireBuild(args, options) {
  if (ownerProcess) {
    return null;
  } else {
    try {
      ownerProcess = child_process.spawn(rescript_exe, args, {
        stdio: "inherit",
        ...options,
      });
      fs.writeFileSync(lockFileName, ownerProcess.pid.toString(), {
        encoding: "utf8",
        mode: 0o664,
      });
    } catch (err) {
      if (err.code === "EEXIST") {
        console.warn(lockFileName, "already exists, try later");
      } else console.log(err);
    }
    return ownerProcess;
  }
}

/**
 * @param {Array<string>} args
 * @param {(code: number) => void} [maybeOnClose]
 */
function delegate(args, maybeOnClose) {
  let p;
  if ((p = acquireBuild(args))) {
    p.once("error", e => {
      console.error(String(e));
      releaseBuild();
      process.exit(2);
    });

    // The 'close' event will always emit after 'exit' was already emitted, or
    // 'error' if the child failed to spawn.
    p.once("close", code => {
      releaseBuild();
      const exitCode = code === null ? 1 : code;
      if (maybeOnClose) {
        maybeOnClose(exitCode);
        return;
      }
      process.exit(exitCode);
    });
  } else {
    console.warn(`Another build detected or stale lockfile ${lockFileName}`);
    // rasing magic code
    process.exit(133);
  }
}

/**
 * @param {Array<string>} args
 */
function info(args) {
  delegate(["info", ...args]);
}

/**
 * @param {Array<string>} args
 */
function clean(args) {
  delegate(["clean", ...args]);
}

const isTtyError = process.stderr.isTTY;
const isTtyStd = process.stdout.isTTY;

/**
 * @type {[number,number]}
 */
let startTime;
function updateStartTime() {
  startTime = process.hrtime();
}
function updateFinishTime() {
  const diff = process.hrtime(startTime);
  return diff[0] * 1e9 + diff[1];
}

/**
 * @param {number} [code]
 */
function logFinishCompiling(code) {
  let log = `>>>> Finish compiling`;
  if (code) {
    log = log + " (exit: " + code + ")";
  }
  if (isTtyStd) {
    log = "\x1b[36m" + log + "\x1b[0m";
  }
  if (code) {
    console.log(log);
  } else {
    console.log(log, Math.floor(updateFinishTime() / 1e6), "mseconds");
  }
}

function logStartCompiling() {
  updateStartTime();
  let log = `>>>> Start compiling`;
  if (isTtyStd) {
    log = "\x1b[36m" + log + "\x1b[0m";
  }
  console.log(log);
}

function exitProcess() {
  releaseBuild();
  process.exit(0);
}

/**
 * @param {string} file
 * @returns
 */
function getProjectFiles(file) {
  if (fs.existsSync(file)) {
    return JSON.parse(fs.readFileSync(file, "utf8"));
  } else {
    return { dirs: [], generated: [] };
  }
}

/**
 * @param {Array<string>} args
 */
function watch(args) {
  // All clients of type MiniWebSocket
  /**
   * @type {any[]}
   */
  let wsClients = [];
  let withWebSocket = false;
  let webSocketHost = "localhost";
  let webSocketPort = 9999;

  let resConfig = "rescript.json";
  if (!fs.existsSync(resConfig)) {
    resConfig = "bsconfig.json";
  }

  const sourcedirs = path.join("lib", "bs", ".sourcedirs.json");

  var LAST_SUCCESS_BUILD_STAMP = 0;

  let LAST_BUILD_START = 0;
  let LAST_FIRED_EVENT = 0;
  /**
   * @type {[string,string][]}
   */
  let reasonsToRebuild = [];
  /**
   * @type {string[]}
   */
  let watchGenerated = [];

  /**
   * @type {WatcherRef[]}
   * watchers are held so that we close it later
   */
  let watchers = [];

  const verbose = args.includes("-verbose");
  const dlog = verbose ? console.log : () => {};

  var wsParamIndex = args.indexOf("-ws");
  if (wsParamIndex > -1) {
    var hostAndPortNumber = (args[wsParamIndex + 1] || "").split(":");
    /**
     * @type {number}
     */
    var portNumber;
    if (hostAndPortNumber.length === 1) {
      portNumber = parseInt(hostAndPortNumber[0]);
    } else {
      webSocketHost = hostAndPortNumber[0];
      portNumber = parseInt(hostAndPortNumber[1]);
    }
    if (!isNaN(portNumber)) {
      webSocketPort = portNumber;
    }
    withWebSocket = true;
    dlog(`WebSocket host & port number: ${webSocketHost}:${webSocketPort}`);
  }

  const rescriptWatchBuildArgs = verbose
    ? ["build", "-no-deps", "-verbose"]
    : ["build", "-no-deps"];

  function notifyClients() {
    wsClients = wsClients.filter(x => !x.closed && !x.socket.destroyed);
    var wsClientsLen = wsClients.length;
    dlog(`Alive sockets number: ${wsClientsLen}`);
    var data = '{"LAST_SUCCESS_BUILD_STAMP":' + LAST_SUCCESS_BUILD_STAMP + "}";
    for (var i = 0; i < wsClientsLen; ++i) {
      // in reverse order, the last pushed get notified earlier
      var client = wsClients[wsClientsLen - i - 1];
      if (!client.closed) {
        client.sendText(data);
      }
    }
  }

  function setUpWebSocket() {
    var WebSocket = require("../lib/minisocket.js").MiniWebSocket;
    var id = setInterval(notifyClients, 3000);
    require("http")
      .createServer()
      .on("upgrade", function (req, socket, upgradeHead) {
        dlog("connection opened");
        var ws = new WebSocket(req, socket, upgradeHead);
        socket.on("error", function (err) {
          dlog(`Socket Error ${err}`);
        });
        wsClients.push(ws);
      })
      .on("error", function (err) {
        // @ts-ignore
        if (err !== undefined && err.code === "EADDRINUSE") {
          var error = isTtyStd ? `\x1b[1;31mERROR:\x1b[0m` : `ERROR:`;
          console.error(`${error} The websocket port number ${webSocketPort} is in use.
Please pick a different one using the \`-ws [host:]port\` flag from bsb.`);
        } else {
          console.error(err);
        }
        process.exit(2);
      })
      .listen(webSocketPort, webSocketHost);
  }

  /**
   * @param {ProjectFiles} projectFiles
   */
  function watchBuild(projectFiles) {
    var watchFiles = projectFiles.dirs;
    watchGenerated = projectFiles.generated;
    // close and remove all unused watchers
    watchers = watchers.filter(function (watcher) {
      if (watcher.dir === resConfig) {
        return true;
      } else if (watchFiles.indexOf(watcher.dir) < 0) {
        dlog(`${watcher.dir} is no longer watched`);
        watcher.watcher.close();
        return false;
      } else {
        return true;
      }
    });

    // adding new watchers
    for (var i = 0; i < watchFiles.length; ++i) {
      var dir = watchFiles[i];
      if (
        !watchers.find(function (watcher) {
          return watcher.dir === dir;
        })
      ) {
        dlog(`watching dir ${dir} now`);
        var watcher = fs.watch(dir, onChange);
        watchers.push({ dir: dir, watcher: watcher });
      } else {
        // console.log(dir, 'already watched')
      }
    }
  }

  /**
   * @param {string | null} fileName
   */
  function checkIsRebuildReason(fileName) {
    // Return true if filename is nil, filename is only provided on Linux, macOS, Windows, and AIX.
    // On other systems, we just have to assume that any change is valid.
    // This could cause problems if source builds (generating js files in the same directory) are supported.
    if (!fileName) return true;

    return (
      ((fileName.endsWith(".res") ||
        fileName.endsWith(".resi") ||
        fileName.endsWith(".ml") ||
        fileName.endsWith(".mli")) &&
        !watchGenerated.includes(fileName)) ||
      fileName === resConfig
    );
  }

  /**
   * @return {boolean}
   */
  function needRebuild() {
    return reasonsToRebuild.length !== 0;
  }

  /**
   * @param {number} code
   */
  function buildFinishedCallback(code) {
    if (code === 0) {
      LAST_SUCCESS_BUILD_STAMP = Date.now();
      notifyClients();
    }
    logFinishCompiling(code);
    releaseBuild();
    if (needRebuild()) {
      build(0);
    } else {
      watchBuild(getProjectFiles(sourcedirs));
    }
  }

  /**
   * TODO: how to make it captured by vscode
   * @param error {string}
   * @param highlight {string}
   */
  function outputError(error, highlight) {
    if (isTtyError && highlight) {
      process.stderr.write(
        error.replace(highlight, "\x1b[1;31m" + highlight + "\x1b[0m")
      );
    } else {
      process.stderr.write(error);
    }
  }

  // Note this function filters the error output
  // it relies on the fact that ninja will merege stdout and stderr
  // of the compiler output, if it does not
  // then we should have a way to not filter the compiler output
  /**
   *
   * @param {number} depth
   */
  function build(depth) {
    if (reasonsToRebuild.length === 0) {
      dlog("No need to rebuild");
      return;
    } else {
      dlog(`Rebuilding since ${reasonsToRebuild}`);
    }
    let p;
    if (
      (p = acquireBuild(rescriptWatchBuildArgs, {
        stdio: ["inherit", "inherit", "pipe"],
      }))
    ) {
      logStartCompiling();
      p.on("data", function (s) {
        outputError(s, "ninja: error");
      })
        .once("exit", buildFinishedCallback)
        .stderr.setEncoding("utf8");
      // This is important to clean up all
      // previous queued events
      reasonsToRebuild = [];
      LAST_BUILD_START = Date.now();
    }
    // if acquiring lock failed, no need retry here
    // since buildFinishedCallback will try again
    // however this is no longer the case for multiple-process
    // it could fail due to other issues like .bsb.lock
    else {
      dlog(
        `Acquire lock failed, do the build later ${depth} : ${reasonsToRebuild}`
      );
      const waitTime = Math.pow(2, depth) * 40;
      setTimeout(() => {
        build(Math.min(depth + 1, 5));
      }, waitTime);
    }
  }

  /**
   *
   * @param {fs.WatchEventType} event
   * @param {string | null} reason
   */
  function onChange(event, reason) {
    var eventTime = Date.now();
    var timeDiff = eventTime - LAST_BUILD_START;
    var eventDiff = eventTime - LAST_FIRED_EVENT;
    dlog(`Since last build: ${timeDiff} -- ${eventDiff}`);
    if (timeDiff < 5 || eventDiff < 5) {
      // for 5ms, we could think that the ninja not get
      // kicked yet, so there is really no need
      // to send more events here

      // note reasonsToRebuild also
      // helps avoid redundant build, but this will
      // save the event loop call `setImmediate`
      return;
    }
    if (checkIsRebuildReason(reason)) {
      dlog(`\nEvent ${event} ${reason}`);
      LAST_FIRED_EVENT = eventTime;
      reasonsToRebuild.push([event, reason || ""]);
      // Some editors are using temporary files to store edits.
      // This results in two sync change events: change + rename and two sync builds.
      // Using setImmediate will ensure that only one build done.
      setImmediate(() => {
        if (needRebuild()) {
          if (process.env.BS_WATCH_CLEAR && console.clear) {
            console.clear();
          }
          build(0);
        }
      });
    }
  }

  /**
   *
   * @param {boolean} withWebSocket
   */
  function startWatchMode(withWebSocket) {
    if (withWebSocket) {
      setUpWebSocket();
    }
    // for column one based error message

    process.stdin.on("close", exitProcess);
    // close when stdin stops
    if (os.platform() !== "win32") {
      process.stdin.on("end", exitProcess);
      process.stdin.resume();
    }

    watchers.push({ watcher: fs.watch(resConfig, onChange), dir: resConfig });
  }

  logStartCompiling();
  delegate(["build", ...args], _ => {
    startWatchMode(withWebSocket);
    buildFinishedCallback(0);
  });
}

/**
 * @param {Array<string>} args
 */
function build(args) {
  // We want to show the compile time for build
  // But bsb might show a help message when --help or invalid arguments are passed
  // We don't want to show the compile time in that case
  // But since we don't have a proper parsing,
  // we can be sure about that only when building without any additional args
  if (args.length === 0) {
    logStartCompiling();
    delegate(["build"], exitCode => {
      logFinishCompiling(exitCode);
      process.exit(exitCode);
    });
    return;
  }
  if (args.some(arg => /-h|-help|--help/.test(arg))) {
    delegate(["build", "-h"]);
    return;
  }
  if (args.includes("-w")) {
    watch(args);
    return;
  }
  delegate(["build", ...args]);
}

exports.releaseBuild = releaseBuild;
exports.info = info;
exports.clean = clean;
exports.build = build;
