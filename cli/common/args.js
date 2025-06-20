// @ts-check

class StringBuilder {
  constructor() {
    this.val = "";
  }
  /**
   *
   * @param {any} v
   * @returns
   */
  add(v) {
    this.val = this.val + v;
    return this;
  }
}

export class ArgError extends Error {}

/**
 * @param {string} s
 */
export function bad_arg(s) {
  throw new ArgError(s);
}

/**
 * @typedef {{ val: string }} stringref
 * @typedef {{ val: boolean }} boolref
 * @typedef {(
 *   | { kind: "Unit_call", data: () => void }
 *   | { kind: "Unit_set", data: boolref }
 * )} unit_action
 * @typedef {(
 *   | { kind: "String_call", data: (s: string) => void }
 *   | { kind: "String_set", data: stringref }
 * )} string_action
 * @typedef {(
 *   | { kind: "Unit", data: unit_action }
 *   | { kind: "String", data: string_action}
 * )} action
 * @typedef {Array<[string, action, string]>} specs
 * @param {StringBuilder} b
 * @param {string} usage
 * @param {specs} specs
 */
function usage_b(b, usage, specs) {
  b.add(usage);
  if (specs.length === 0) {
    return;
  }
  b.add("\nOptions:\n");
  let max_col = 0;
  for (const [key] of specs) {
    if (key.length > max_col) {
      max_col = key.length;
    }
  }
  for (let i = 0; i < specs.length; i++) {
    const [key, _, doc] = specs[i];
    if (!doc.startsWith("*internal*")) {
      b.add("  ")
        .add(key)
        .add(" ".repeat(max_col - key.length + 2));
      let cur = 0;
      const doc_length = doc.length;
      while (cur < doc_length) {
        if (cur !== 0) {
          b.add("\n").add(" ".repeat(max_col + 4));
        }
        const i = doc.indexOf("\n", cur);
        if (i < 0) {
          b.add(doc.substring(cur));
          break;
        }
        b.add(doc.substring(cur, i - cur));
        cur = i + 1;
      }
      b.add("\n");
    }
  }
}

/**
 * @typedef {(
 *   | { kind: "Unknown", data:string }
 *   | { kind: "Missing", data:string }
 * )} error
 * @param {string} usage
 * @param {error} error
 * @param {specs} specs
 */
function stop_raise(usage, error, specs) {
  const b = new StringBuilder();
  switch (error.kind) {
    case "Unknown":
      if (["-help", "--help", "-h"].includes(error.data)) {
        usage_b(b, usage, specs);
        process.stdout.write(b.val);
        process.exit(0);
      } else {
        b.add(`Unknown option "${error.data}".\n'`);
      }
      break;
    case "Missing":
      b.add(`Option "${error.data}" needs an argument.\n'`);
      break;
  }
  usage_b(b, usage, specs);
  bad_arg(b.val);
}

/**
 * @param {string} usage
 * @param {Array<string>} argv
 * @param {specs} specs
 * @param {(args: Array<string>) => void} annofun
 * @param {number} start
 * @param {number} finish
 */
export function parse_exn(
  usage,
  argv,
  specs,
  annofun,
  start = 0,
  // first 3 are [node, rescript, subcommand]
  finish = argv.length,
) {
  let current = start;
  const list = [];
  while (current < finish) {
    const s = argv[current];
    ++current;
    if (s !== "" && s[0] === "-") {
      const out = specs.find(([flag]) => flag === s);
      if (out !== undefined) {
        const [_, action] = out;
        switch (action.kind) {
          case "Unit":
            switch (action.data.kind) {
              case "Unit_call":
                action.data.data();
                break;
              case "Unit_set":
                action.data.data.val = true;
                break;
            }
            break;
          case "String":
            // switch(action.data.kind)
            if (current >= finish) {
              stop_raise(usage, { kind: "Missing", data: s }, specs);
            } else {
              const arg = argv[current];
              ++current;
              switch (action.data.kind) {
                case "String_call":
                  action.data.data(arg);
                  break;
                case "String_set":
                  action.data.data.val = arg;
                  break;
              }
            }
            break;
        }
      } else {
        stop_raise(usage, { kind: "Unknown", data: s }, specs);
      }
    } else {
      list.push(s);
    }
  }
  annofun(list);
}
