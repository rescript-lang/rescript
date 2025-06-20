#!/usr/bin/env node

// @ts-check

import * as fs from "node:fs";
import * as path from "node:path";
import * as readline from "node:readline";

/**
 *
 * @param {string} file
 * @param {(line:string)=>void} lineCb
 * @param {()=>void} finish
 */
function processEntry(file, lineCb, finish) {
  const input = fs.createReadStream(file);
  input.on("error", error => {
    console.error(error.message);
    console.error(
      "make sure you are running after bsb building and in the top directory",
    );
    process.exit(2);
  });
  const rl = readline.createInterface({
    input: input,
    crlfDelay: Number.POSITIVE_INFINITY,
  });

  rl.on("line", lineCb);
  rl.on("close", finish);
}

class Interval {
  /**
   *
   * @param {number} start
   * @param {number} end
   */
  constructor(start, end) {
    this.start = start;
    this.end = end;
    /**
     * @type {string[]}
     */
    this.targets = [];
  }
}

class Threads {
  constructor() {
    /**
     * @type {number[]}
     */
    this.workers = [];
  }
  /**
   *
   * @param {{start : number; end : number}} target
   */
  alloc(target) {
    for (let i = 0; i < this.workers.length; ++i) {
      if (this.workers[i] <= target.start) {
        this.workers[i] = target.end;
        return i;
      }
    }
    this.workers.push(target.end);
    return this.workers.length - 1;
  }
}
/**
 *
 * @param {Map<string,Interval>} map
 * @param {string} key
 * @param {Interval} def
 *
 * */
function setDefault(map, key, def) {
  if (map.has(key)) {
    return map.get(key);
  }
  map.set(key, def);
  return def;
}

// https://github.com/catapult-project/catapult/blob/master/tracing/tracing/base/color_scheme.html#L50
const colors = [
  "thread_state_uninterruptible",
  "thread_state_iowait",
  "thread_state_running",
  "thread_state_runnable",
  "thread_state_sleeping",
  "thread_state_unknown",
  "background_memory_dump",
  "light_memory_dump",
  "detailed_memory_dump",
  "vsync_highlight_color",
  "generic_work",
  "good",
  "bad",
  "terrible",
  "black",
  "grey",
  "white",
  "yellow",
  "olive",
  "rail_response",
  "rail_animation",
  "rail_idle",
  "rail_load",
  "startup",
  "heap_dump_stack_frame",
  "heap_dump_object_type",
  "heap_dump_child_node_arrow",
  "cq_build_running",
  "cq_build_passed",
  "cq_build_failed",
  "cq_build_abandoned",
  "cq_build_attempt_runnig",
  "cq_build_attempt_passed",
  "cq_build_attempt_failed",
];

const allocated = new Map();

function getColorName(obj, cat) {
  obj.cat = cat;
  let i;
  if (allocated.has(cat)) {
    i = allocated.get(cat);
  } else {
    allocated.set(cat, allocated.size);
  }
  obj.cname = colors[i % colors.length];
  return;
}
/**
 *
 * @param {Interval} target
 */
function category(target, obj) {
  const targets = target.targets;
  if (targets.length === 1) {
    const curTar = targets[0];
    if (curTar.endsWith(".d")) {
      getColorName(obj, "dep");
    } else if (curTar.endsWith(".mlast") || curTar.endsWith(".mliast")) {
      getColorName(obj, "parse");
    } else if (curTar.endsWith(".cmi")) {
      getColorName(obj, "cmi");
    } else if (curTar.endsWith(".cmj")) {
      getColorName(obj, "cmj-only");
    } else {
      getColorName(obj, "unknown");
    }
  } else {
    getColorName(obj, "cmj");
  }
  obj.name = target.targets.map(x => path.parse(x).base).join(",");
  return obj;
}
/**
 * @param {string} file
 * @param {boolean} showAll
 * @param {string} outputFile
 */
function readIntervals(file, showAll, outputFile) {
  let lastEndSeen = 0;
  /**
   * @type {Map<string,Interval>}
   */
  let targets = new Map();
  let offset = 0;
  processEntry(
    file,
    line => {
      const lineTrim = line.trim();
      if (lineTrim.startsWith("#")) {
        return;
      }

      let [start, end, _, name, cmdHash] = lineTrim.split("\t");
      cmdHash += `/${end}`;
      if (+end < lastEndSeen) {
        // This is a guess
        // it could be wrong, when there's multiple small compilation
        if (showAll) {
          offset += lastEndSeen + 1000;
          console.log(`new session starting from: ${name} : ${offset}`);
        } else {
          targets = new Map();
        }
      }
      lastEndSeen = +end; // new mark
      setDefault(
        targets,
        cmdHash,
        new Interval(Number(start) + offset, Number(end) + offset),
      ).targets.push(name);
    },
    () => {
      const sorted = [...targets.values()].sort((a, b) => {
        return a.start - b.start;
      });
      const jsonArray = [];
      const threads = new Threads();
      for (const target of sorted) {
        jsonArray.push(
          category(target, {
            ph: "X",
            pid: 0,
            dur: (target.end - target.start) * 1000,
            ts: target.start * 1000,
            tid: threads.alloc(target),
            args: {},
          }),
        );
      }
      console.log(` ${outputFile} is produced, loade it via chrome://tracing/`);
      fs.writeFileSync(outputFile, JSON.stringify(jsonArray), "utf8");
    },
  );
}
const logName = ".ninja_log";

/**
 * @type {string}
 */
let file;
/**
 *
 * @param ps {string[]}
 */
function tryLocation(ps) {
  for (const p of ps) {
    const log = path.join(p, logName);
    if (fs.existsSync(log)) {
      file = log;
      return;
    }
  }
  console.error(
    "no .ninja_log found in specified paths, make sure you set bstracing to the proper directory",
  );
  process.exit(2);
}

let showAll = false;
const curDate = new Date();
let outputFile = `tracing_${curDate.getHours()}_${curDate.getMinutes()}_${curDate.getSeconds()}.json`;

{
  let index = process.argv.indexOf("-C");
  if (index >= 0) {
    const p = process.argv[index + 1];
    tryLocation([p, path.join(p, "lib", "bs")]);
  } else {
    tryLocation([".", path.join("lib", "bs")]);
  }
  if (process.argv.includes("-all")) {
    showAll = true;
  }
  index = process.argv.indexOf("-o");
  if (index >= 0) {
    outputFile = process.argv[index + 1];
  }
}

console.log("loading build log", file, "is used");
readIntervals(file, showAll, outputFile);
