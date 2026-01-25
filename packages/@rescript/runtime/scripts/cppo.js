// @ts-check

import { execFileSync } from "node:child_process";
import * as path from "node:path";

const targets = [
  ["Belt_HashSetString.res", "hashset.res.cppo", "TYPE_STRING"],
  ["Belt_HashSetString.resi", "hashset.resi.cppo", "TYPE_STRING"],
  ["Belt_HashSetInt.res", "hashset.res.cppo", "TYPE_INT"],
  ["Belt_HashSetInt.resi", "hashset.resi.cppo", "TYPE_INT"],
  ["Belt_HashMapString.res", "hashmap.res.cppo", "TYPE_STRING"],
  ["Belt_HashMapString.resi", "hashmap.resi.cppo", "TYPE_STRING"],
  ["Belt_HashMapInt.res", "hashmap.res.cppo", "TYPE_INT"],
  ["Belt_HashMapInt.resi", "hashmap.resi.cppo", "TYPE_INT"],
  ["Belt_MapString.res", "map.res.cppo", "TYPE_STRING"],
  ["Belt_MapString.resi", "map.resi.cppo", "TYPE_STRING"],
  ["Belt_MapInt.res", "map.res.cppo", "TYPE_INT"],
  ["Belt_MapInt.resi", "map.resi.cppo", "TYPE_INT"],
  ["Belt_SetString.res", "belt_Set.res.cppo", "TYPE_STRING"],
  ["Belt_SetString.resi", "belt_Set.resi.cppo", "TYPE_STRING"],
  ["Belt_SetInt.res", "belt_Set.res.cppo", "TYPE_INT"],
  ["Belt_SetInt.resi", "belt_Set.resi.cppo", "TYPE_INT"],
  ["Belt_MutableMapString.res", "mapm.res.cppo", "TYPE_STRING"],
  ["Belt_MutableMapString.resi", "mapm.resi.cppo", "TYPE_STRING"],
  ["Belt_MutableMapInt.res", "mapm.res.cppo", "TYPE_INT"],
  ["Belt_MutableMapInt.resi", "mapm.resi.cppo", "TYPE_INT"],
  ["Belt_MutableSetString.res", "setm.res.cppo", "TYPE_STRING"],
  ["Belt_MutableSetString.resi", "setm.resi.cppo", "TYPE_STRING"],
  ["Belt_MutableSetInt.res", "setm.res.cppo", "TYPE_INT"],
  ["Belt_MutableSetInt.resi", "setm.resi.cppo", "TYPE_INT"],
  ["Belt_SortArrayString.res", "sort.res.cppo", "TYPE_STRING"],
  ["Belt_SortArrayString.resi", "sort.resi.cppo", "TYPE_STRING"],
  ["Belt_SortArrayInt.res", "sort.res.cppo", "TYPE_INT"],
  ["Belt_SortArrayInt.resi", "sort.resi.cppo", "TYPE_INT"],
  ["Belt_internalMapString.res", "internal_map.res.cppo", "TYPE_STRING"],
  ["Belt_internalMapInt.res", "internal_map.res.cppo", "TYPE_INT"],
  ["Belt_internalSetString.res", "internal_set.res.cppo", "TYPE_STRING"],
  ["Belt_internalSetInt.res", "internal_set.res.cppo", "TYPE_INT"],
];

const runtimePath = path.join(import.meta.dirname, "..");
for (const [output, input, type] of targets) {
  const inputPath = path.join(runtimePath, "cppo", input);
  const outputPath = path.join(runtimePath, output);
  execFileSync("cppo", ["-n", "-D", type, inputPath, "-o", outputPath], {
    stdio: "inherit",
  });
}
