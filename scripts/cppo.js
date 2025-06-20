// @ts-check

import { execFileSync } from "node:child_process";

const targets = [
  ["belt_HashSetString.res", "hashset.res.cppo", "TYPE_STRING"],
  ["belt_HashSetString.resi", "hashset.resi.cppo", "TYPE_STRING"],
  ["belt_HashSetInt.res", "hashset.res.cppo", "TYPE_INT"],
  ["belt_HashSetInt.resi", "hashset.resi.cppo", "TYPE_INT"],
  ["belt_HashMapString.res", "hashmap.res.cppo", "TYPE_STRING"],
  ["belt_HashMapString.resi", "hashmap.resi.cppo", "TYPE_STRING"],
  ["belt_HashMapInt.res", "hashmap.res.cppo", "TYPE_INT"],
  ["belt_HashMapInt.resi", "hashmap.resi.cppo", "TYPE_INT"],
  ["belt_MapString.res", "map.res.cppo", "TYPE_STRING"],
  ["belt_MapString.resi", "map.resi.cppo", "TYPE_STRING"],
  ["belt_MapInt.res", "map.res.cppo", "TYPE_INT"],
  ["belt_MapInt.resi", "map.resi.cppo", "TYPE_INT"],
  ["belt_SetString.res", "belt_Set.res.cppo", "TYPE_STRING"],
  ["belt_SetString.resi", "belt_Set.resi.cppo", "TYPE_STRING"],
  ["belt_SetInt.res", "belt_Set.res.cppo", "TYPE_INT"],
  ["belt_SetInt.resi", "belt_Set.resi.cppo", "TYPE_INT"],
  ["belt_MutableMapString.res", "mapm.res.cppo", "TYPE_STRING"],
  ["belt_MutableMapString.resi", "mapm.resi.cppo", "TYPE_STRING"],
  ["belt_MutableMapInt.res", "mapm.res.cppo", "TYPE_INT"],
  ["belt_MutableMapInt.resi", "mapm.resi.cppo", "TYPE_INT"],
  ["belt_MutableSetString.res", "setm.res.cppo", "TYPE_STRING"],
  ["belt_MutableSetString.resi", "setm.resi.cppo", "TYPE_STRING"],
  ["belt_MutableSetInt.res", "setm.res.cppo", "TYPE_INT"],
  ["belt_MutableSetInt.resi", "setm.resi.cppo", "TYPE_INT"],
  ["belt_SortArrayString.res", "sort.res.cppo", "TYPE_STRING"],
  ["belt_SortArrayString.resi", "sort.resi.cppo", "TYPE_STRING"],
  ["belt_SortArrayInt.res", "sort.res.cppo", "TYPE_INT"],
  ["belt_SortArrayInt.resi", "sort.resi.cppo", "TYPE_INT"],
  ["belt_internalMapString.res", "internal_map.res.cppo", "TYPE_STRING"],
  ["belt_internalMapInt.res", "internal_map.res.cppo", "TYPE_INT"],
  ["belt_internalSetString.res", "internal_set.res.cppo", "TYPE_STRING"],
  ["belt_internalSetInt.res", "internal_set.res.cppo", "TYPE_INT"],
];

for (const [output, input, type] of targets) {
  execFileSync(
    "cppo",
    ["-n", "-D", type, `runtime/cppo/${input}`, "-o", `runtime/${output}`],
    { stdio: "inherit" },
  );
}
