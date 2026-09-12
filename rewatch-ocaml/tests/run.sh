#!/bin/sh
set -eu

port="$1"
port_directory=$(CDPATH= cd -- "$(dirname "$port")" && pwd)
port="$port_directory/$(basename "$port")"
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
: "${RESCRIPT_BSC_EXE:=$root/_build/default/compiler/bsc/rescript_compiler_main.exe}"
: "${RESCRIPT_RUNTIME:=$root/packages/@rescript/runtime}"
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME
work="$root/tmp/rewatch-ocaml/test-$$"
mkdir -p "$work"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/cleanup-lifecycle"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/cleanup-failure"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/suffix-removal"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/suffix-failure"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/packaged-basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/runtime-discovery"
mkdir -p "$work/no-bin-annot/src"
printf '{"name":"no-bin-annot","sources":["src"]}\n' \
  >"$work/no-bin-annot/rescript.json"
printf '@@config({flags: ["-bs-no-bin-annot"]})\nlet value = 1\n' \
  >"$work/no-bin-annot/src/NoBinAnnot.res"
cp -R "$root/rewatch-ocaml/tests/basic" "$work/legacy-config"
cp -R "$root/rewatch-ocaml/tests/cycle" "$work/cycle"
cp -R "$root/rewatch-ocaml/tests/failure" "$work/failure"
cp -R "$root/rewatch-ocaml/tests/features" "$work/features"
cp -R "$root/rewatch-ocaml/tests/feature-dependencies" "$work/feature-dependencies"
cp -R "$root/rewatch-ocaml/tests/gentype" "$work/gentype"
cp -R "$root/rewatch-ocaml/tests/dependency" "$work/dependency"
cp -R "$root/rewatch-ocaml/tests/package-output-dependency" \
  "$work/package-output-dependency"
mkdir -p "$work/feature-dependencies/node_modules"
for dependency in consumer dep-union dep-transitive dep-empty; do
  ln -s "../packages/$dependency" \
    "$work/feature-dependencies/node_modules/$dependency"
done
unlinked_dependency="$work/unlinked-dependency"
mkdir -p "$unlinked_dependency/src" \
  "$unlinked_dependency/packages/dep/src" \
  "$unlinked_dependency/packages/dep/lib/ocaml"
printf '%s\n' \
  '{"name":"unlinked-root","sources":"src","dependencies":["dep"]}' \
  >"$unlinked_dependency/rescript.json"
printf 'let value = 1\n' >"$unlinked_dependency/src/Root.res"
printf '%s\n' '{"name":"dep","sources":"src"}' \
  >"$unlinked_dependency/packages/dep/rescript.json"
printf 'let value = 1\n' >"$unlinked_dependency/packages/dep/src/Dep.res"
printf 'export const value = 1;\n' \
  >"$unlinked_dependency/packages/dep/src/Dep.js"
printf 'owned outside the resolved graph\n' \
  >"$unlinked_dependency/packages/dep/lib/ocaml/marker"
mkdir -p "$work/standalone-output/src" \
  "$work/standalone-output/node_modules"
cp -R "$root/rewatch-ocaml/tests/shared-dep" \
  "$work/standalone-output/node_modules/dep"
printf '%s\n' \
  '{"name":"standalone-output","sources":"src","dependencies":["dep"],"package-specs":{"module":"esmodule","in-source":false,"suffix":".mjs"}}' \
  >"$work/standalone-output/rescript.json"
printf 'let value = Dep.value\n' >"$work/standalone-output/src/Main.res"
mkdir -p "$work/gentype/node_modules" "$work/dependency/node_modules" \
  "$work/package-output-dependency/node_modules"
cp -R "$root/rewatch-ocaml/tests/shared-dep" "$work/gentype/node_modules/dep"
cp -R "$root/rewatch-ocaml/tests/shared-dep" "$work/dependency/node_modules/dep"
cp -R "$root/rewatch-ocaml/tests/shared-dep" \
  "$work/package-output-dependency/node_modules/dep"
cp -R "$root/rewatch-ocaml/tests/external-boundary" "$work/external-boundary"
cp -R "$root/rewatch-ocaml/tests/post-build" "$work/post-build"
cp -R "$root/rewatch-ocaml/tests/out-of-source" "$work/out-of-source"
cp -R "$root/rewatch-ocaml/tests/ppx-filter" "$work/ppx-filter"
cp -R "$root/rewatch-ocaml/tests/namespace" "$work/namespace"
cp -R "$root/rewatch-ocaml/tests/namespace-entry" "$work/namespace-entry"
cp -R "$root/rewatch-ocaml/tests/qualified-namespace" \
  "$work/qualified-namespace"
cp -R "$root/rewatch-ocaml/tests/namespace-collision" \
  "$work/namespace-collision"
mkdir -p "$work/namespace-collision/node_modules"
for dependency in namespace-one namespace-two; do
  ln -s "../packages/$dependency" \
    "$work/namespace-collision/node_modules/$dependency"
done
cp -R "$root/rewatch-ocaml/tests/source-map" "$work/source-map"
cp -R "$root/rewatch-ocaml/tests/warning-replay" "$work/warning-replay"
cp -R "$root/rewatch-ocaml/tests/monorepo" "$work/monorepo"
basic="$work/basic"
cleanup_lifecycle="$work/cleanup-lifecycle"
cleanup_failure="$work/cleanup-failure"
suffix_removal="$work/suffix-removal"
suffix_failure="$work/suffix-failure"
packaged_basic="$work/packaged-basic"
runtime_discovery="$work/runtime-discovery"
no_bin_annot="$work/no-bin-annot"
legacy_config="$work/legacy-config"
cycle="$work/cycle"
failure="$work/failure"
features="$work/features"
feature_dependencies="$work/feature-dependencies"
gentype="$work/gentype"
dependency="$work/dependency"
package_output_dependency="$work/package-output-dependency"
standalone_output="$work/standalone-output"
external_boundary="$work/external-boundary"
post_build="$work/post-build"
out_of_source="$work/out-of-source"
ppx_filter="$work/ppx-filter"
namespace="$work/namespace"
namespace_entry="$work/namespace-entry"
qualified_namespace="$work/qualified-namespace"
namespace_collision="$work/namespace-collision"
source_map="$work/source-map"
warning_replay="$work/warning-replay"
monorepo="$work/monorepo"

package_name_mismatch="$work/package-name-mismatch"
mkdir -p "$package_name_mismatch/src" \
  "$package_name_mismatch/packages/dep/src" \
  "$package_name_mismatch/node_modules"
printf '%s\n' \
  '{"name":"root","sources":"src","dependencies":["dep"]}' \
  >"$package_name_mismatch/rescript.json"
printf '%s\n' '{"name":"root"}' >"$package_name_mismatch/package.json"
printf 'let value: int = Dep.value\n' \
  >"$package_name_mismatch/src/Main.res"
printf '%s\n' '{"name":"old-name","sources":"src"}' \
  >"$package_name_mismatch/packages/dep/rescript.json"
printf '%s\n' '{"name":"dep"}' \
  >"$package_name_mismatch/packages/dep/package.json"
printf 'let value = 1\n' \
  >"$package_name_mismatch/packages/dep/src/Dep.res"
ln -s ../packages/dep "$package_name_mismatch/node_modules/dep"

config_cleanup_invalidation="$work/config-cleanup-invalidation"
cp -R "$package_name_mismatch" "$config_cleanup_invalidation"
printf '%s\n' '{"name":"dep","sources":"src"}' \
  >"$config_cleanup_invalidation/packages/dep/rescript.json"

transitive_local_watch="$work/transitive-local-watch"
mkdir -p "$transitive_local_watch/src" \
  "$transitive_local_watch/node_modules/external/src" \
  "$transitive_local_watch/node_modules/external/node_modules" \
  "$transitive_local_watch/packages/local/src"
printf '%s\n' '{"workspaces":["packages/*"]}' \
  >"$transitive_local_watch/package.json"
printf '%s\n' \
  '{"name":"transitive-local-watch","sources":"src","dependencies":["external"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$transitive_local_watch/rescript.json"
printf 'let value = External.value\n' \
  >"$transitive_local_watch/src/Main.res"
printf '%s\n' \
  '{"name":"external","sources":"src","dependencies":["local"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$transitive_local_watch/node_modules/external/rescript.json"
printf 'let value = Local.value\n' \
  >"$transitive_local_watch/node_modules/external/src/External.res"
printf '%s\n' \
  '{"name":"local","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$transitive_local_watch/packages/local/rescript.json"
printf 'let value = 1\n' \
  >"$transitive_local_watch/packages/local/src/Local.res"
ln -s ../../../packages/local \
  "$transitive_local_watch/node_modules/external/node_modules/local"

namespace_invalidation="$work/namespace-invalidation"
mkdir -p "$namespace_invalidation/src" \
  "$namespace_invalidation/packages/dep/src" \
  "$namespace_invalidation/node_modules"
printf '%s\n' \
  '{"name":"namespace-consumer","sources":"src","dependencies":["namespace-dep"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$namespace_invalidation/rescript.json"
printf 'let value = Ns.A.value\n' >"$namespace_invalidation/src/Main.res"
printf '%s\n' \
  '{"name":"namespace-dep","namespace":"Ns","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$namespace_invalidation/packages/dep/rescript.json"
printf 'let value = 1\n' \
  >"$namespace_invalidation/packages/dep/src/A.res"
printf 'let value = 2\n' \
  >"$namespace_invalidation/packages/dep/src/B.res"
ln -s ../packages/dep \
  "$namespace_invalidation/node_modules/namespace-dep"

namespace_restart="$work/namespace-restart"
cp -R "$namespace_invalidation" "$namespace_restart"
printf 'let value: int = Ns.A.value\n' >"$namespace_restart/src/Main.res"

namespace_repair="$work/namespace-repair"
cp -R "$namespace_invalidation" "$namespace_repair"
printf 'let other = 1\n' >"$namespace_repair/src/Other.res"

interface_failure_recovery="$work/interface-failure-recovery"
mkdir -p "$interface_failure_recovery/src"
printf '%s\n' \
  '{"name":"interface-failure-recovery","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$interface_failure_recovery/rescript.json"
printf 'let value: int\n' >"$interface_failure_recovery/src/A.resi"
printf 'let value = 1\n' >"$interface_failure_recovery/src/A.res"
printf 'let dependent = A.value + 1\n' \
  >"$interface_failure_recovery/src/B.res"
printf 'let valid = true\n' \
  >"$interface_failure_recovery/src/Broken.res"

atomic_save="$work/atomic-save"
mkdir -p "$atomic_save/src"
printf '%s\n' \
  '{"name":"atomic-save","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$atomic_save/rescript.json"
printf 'let value = 1\n' >"$atomic_save/src/Main.res"

directory_symlink="$work/directory-symlink"
mkdir -p "$directory_symlink/shared"
printf '%s\n' \
  '{"name":"directory-symlink","sources":[{"dir":"src","subdirs":true}],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$directory_symlink/rescript.json"
printf 'let value = 1\n' >"$directory_symlink/shared/Linked.res"
ln -s shared "$directory_symlink/src"

recursive_lib="$work/recursive-lib"
mkdir -p "$recursive_lib/src/lib"
printf '%s\n' \
  '{"name":"recursive-lib","sources":[{"dir":"src","subdirs":true}],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$recursive_lib/rescript.json"
printf 'let value = 1\n' >"$recursive_lib/src/lib/Nested.res"

filtered_dependency="$work/filtered-dependency"
mkdir -p "$filtered_dependency/src" \
  "$filtered_dependency/packages/dep/src" \
  "$filtered_dependency/node_modules"
printf '%s\n' \
  '{"name":"filtered-root","sources":"src","dependencies":["dep"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$filtered_dependency/rescript.json"
printf 'let value = Helper.value\n' >"$filtered_dependency/src/Main.res"
printf 'let ignored = 1\n' >"$filtered_dependency/src/Ignored.res"
printf '%s\n' \
  '{"name":"dep","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$filtered_dependency/packages/dep/rescript.json"
printf 'let value = 1\n' \
  >"$filtered_dependency/packages/dep/src/Helper.res"
ln -s '../packages/dep' "$filtered_dependency/node_modules/dep"

symlink_source="$work/symlink-source"
mkdir -p "$symlink_source/src" "$symlink_source/shared"
printf '%s\n' \
  '{"name":"symlink-source","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$symlink_source/rescript.json"
printf 'let value = 1\n' >"$symlink_source/shared/Source.js"
ln -s '../shared/Source.js' "$symlink_source/src/Linked.res"

symlink_alias="$work/symlink-alias"
mkdir -p "$symlink_alias/src"
printf '%s\n' \
  '{"name":"symlink-alias","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$symlink_alias/rescript.json"
printf 'let value = 1\n' >"$symlink_alias/src/Original.res"
ln -s Original.res "$symlink_alias/src/Linked.res"

multiple_cycles="$work/multiple-cycles"
mkdir -p "$multiple_cycles/src"
printf '%s\n' \
  '{"name":"multiple-cycles","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$multiple_cycles/rescript.json"
printf 'let value = B.value\n' >"$multiple_cycles/src/A.res"
printf 'let value = A.value\n' >"$multiple_cycles/src/B.res"
printf 'let value = D.value\n' >"$multiple_cycles/src/C.res"
printf 'let value = C.value\n' >"$multiple_cycles/src/D.res"
printf 'let value = 1\n' >"$multiple_cycles/src/Valid.res"

post_build_cmi="$work/post-build-cmi"
mkdir -p "$post_build_cmi/src"
printf '%s\n' \
  '{"name":"post-build-cmi","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"},"js-post-build":{"cmd":"node -e \"process.exit(require('\''fs'\'').existsSync('\''allow-post-build'\'') ? 0 : 7)\""}}' \
  >"$post_build_cmi/rescript.json"
printf 'let value = 1\n' >"$post_build_cmi/src/A.res"
printf 'let dependent = A.value + 1\n' >"$post_build_cmi/src/B.res"
touch "$post_build_cmi/allow-post-build"

post_build_retry="$work/post-build-retry"
mkdir -p "$post_build_retry/src"
printf '%s\n' \
  '{"name":"post-build-retry","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"},"js-post-build":{"cmd":"node -e \"process.exit(require('\''fs'\'').existsSync('\''allow-post-build'\'') ? 0 : 7)\""}}' \
  >"$post_build_retry/rescript.json"
printf 'let value = 1\n' >"$post_build_retry/src/A.res"
touch "$post_build_retry/allow-post-build"

parse_publication="$work/parse-publication"
mkdir -p "$parse_publication/src"
printf '%s\n' \
  '{"name":"parse-publication","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$parse_publication/rescript.json"
printf 'let value = 1\n' >"$parse_publication/src/A.res"
printf 'let other = 1\n' >"$parse_publication/src/B.res"

multi_package_pending="$work/multi-package-pending"
mkdir -p "$multi_package_pending/src" \
  "$multi_package_pending/packages/dep1/src" \
  "$multi_package_pending/packages/dep2/src" \
  "$multi_package_pending/node_modules"
printf '%s\n' \
  '{"name":"multi-package-pending","sources":"src","dependencies":["dep1","dep2"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$multi_package_pending/rescript.json"
printf 'let value = A.value + B.value\n' \
  >"$multi_package_pending/src/Main.res"
printf 'let other = 1\n' >"$multi_package_pending/src/Other.res"
printf '%s\n' \
  '{"name":"dep1","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$multi_package_pending/packages/dep1/rescript.json"
printf 'let value = 1\n' \
  >"$multi_package_pending/packages/dep1/src/A.res"
printf '%s\n' \
  '{"name":"dep2","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$multi_package_pending/packages/dep2/rescript.json"
printf 'let value = 1\n' \
  >"$multi_package_pending/packages/dep2/src/B.res"
ln -s ../packages/dep1 "$multi_package_pending/node_modules/dep1"
ln -s ../packages/dep2 "$multi_package_pending/node_modules/dep2"

full_watch_recovery="$work/full-watch-recovery"
mkdir -p "$full_watch_recovery/src"
printf '%s\n' \
  '{"name":"full-watch-recovery","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$full_watch_recovery/rescript.json"
printf 'let value = B.value\n' >"$full_watch_recovery/src/A.res"
printf 'let value = 1\n' >"$full_watch_recovery/src/B.res"
printf 'let value = 1\n' >"$full_watch_recovery/src/C.res"

deletion_retry="$work/deletion-retry"
cp -R "$full_watch_recovery" "$deletion_retry"

moved_source="$work/moved-source"
mkdir -p "$moved_source/src/nested"
printf '%s\n' \
  '{"name":"moved-source","sources":{"dir":"src","subdirs":true},"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$moved_source/rescript.json"
printf 'let value = 1\n' >"$moved_source/src/A.res"

publication_cmi="$work/publication-cmi"
mkdir -p "$publication_cmi/src"
printf '%s\n' \
  '{"name":"publication-cmi","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$publication_cmi/rescript.json"
printf 'let value = 1\n' >"$publication_cmi/src/A.res"
printf 'let dependent = A.value + 1\n' >"$publication_cmi/src/B.res"

retained_cycle="$work/retained-cycle"
mkdir -p "$retained_cycle/src"
printf '%s\n' \
  '{"name":"retained-cycle","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$retained_cycle/rescript.json"
printf 'let value = 1\n' >"$retained_cycle/src/A.res"
printf 'let dependent = A.value\n' >"$retained_cycle/src/B.res"

retained_parse="$work/retained-parse"
mkdir -p "$retained_parse/src"
printf '%s\n' \
  '{"name":"retained-parse","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$retained_parse/rescript.json"
printf 'let value = 1\n' >"$retained_parse/src/A.res"
printf 'let other = 1\n' >"$retained_parse/src/B.res"

initial_failure_freshness="$work/initial-failure-freshness"
mkdir -p "$initial_failure_freshness/src"
printf '%s\n' \
  '{"name":"initial-failure-freshness","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$initial_failure_freshness/rescript.json"
printf 'let value = 1\n' >"$initial_failure_freshness/src/A.res"
printf 'let other = 1\n' >"$initial_failure_freshness/src/B.res"

duplicate_selection="$work/duplicate-selection"
mkdir -p "$duplicate_selection/src" \
  "$duplicate_selection/node_modules" \
  "$duplicate_selection/packages/a/src" \
  "$duplicate_selection/packages/a/node_modules" \
  "$duplicate_selection/packages/shared/src" \
  "$duplicate_selection/packages/nested-shared/src"
printf '%s\n' \
  '{"name":"duplicate-root","sources":"src","dependencies":["a","shared"],"package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$duplicate_selection/rescript.json"
printf 'let value = Shared.value\n' >"$duplicate_selection/src/Main.res"
printf '%s\n' \
  '{"name":"a","sources":"src","dependencies":["shared"]}' \
  >"$duplicate_selection/packages/a/rescript.json"
printf 'let value = 1\n' >"$duplicate_selection/packages/a/src/A.res"
printf '%s\n' \
  '{"name":"shared","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$duplicate_selection/packages/shared/rescript.json"
printf 'let value = 1\n' \
  >"$duplicate_selection/packages/shared/src/Shared.res"
printf '%s\n' \
  '{"name":"shared","sources":"src","package-specs":{"module":"esmodule","in-source":true,"suffix":".mjs"}}' \
  >"$duplicate_selection/packages/nested-shared/rescript.json"
printf 'let value = 2\n' \
  >"$duplicate_selection/packages/nested-shared/src/Shared.res"
ln -s ../packages/a "$duplicate_selection/node_modules/a"
ln -s ../packages/shared "$duplicate_selection/node_modules/shared"
ln -s ../../nested-shared \
  "$duplicate_selection/packages/a/node_modules/shared"

external_duplicate_base="$work/external-duplicate"
external_duplicate="$external_duplicate_base/project"
mkdir -p "$external_duplicate/src" \
  "$external_duplicate/node_modules/a/src" \
  "$external_duplicate/node_modules/a/node_modules/shared/src" \
  "$external_duplicate_base/node_modules/shared/src"
printf '%s\n' \
  '{"name":"external-duplicate-root","sources":"src","dependencies":["a","shared"],"package-specs":{"module":"esmodule","in-source":true}}' \
  >"$external_duplicate/rescript.json"
printf 'let value = Shared.value\n' >"$external_duplicate/src/Main.res"
printf '%s\n' '{"name":"a","sources":"src","dependencies":["shared"]}' \
  >"$external_duplicate/node_modules/a/rescript.json"
printf 'let value = 1\n' >"$external_duplicate/node_modules/a/src/A.res"
printf '%s\n' '{"name":"shared","sources":"src"}' \
  >"$external_duplicate/node_modules/a/node_modules/shared/rescript.json"
printf 'let value = 1\n' \
  >"$external_duplicate/node_modules/a/node_modules/shared/src/Shared.res"
printf '%s\n' '{"name":"shared","sources":"src"}' \
  >"$external_duplicate_base/node_modules/shared/rescript.json"
printf 'let value = 2\n' \
  >"$external_duplicate_base/node_modules/shared/src/Shared.res"

external_diagnostics_base="$work/external-diagnostics"
external_cycle="$external_diagnostics_base/project"
external_cycle_dependency="$external_diagnostics_base/store/cycle-dep"
mkdir -p "$external_cycle/src" "$external_cycle/node_modules" \
  "$external_cycle_dependency/src" "$external_cycle_dependency/node_modules"
printf '%s\n' \
  '{"name":"external-cycle-root","sources":"src","dependencies":["cycle-dep"]}' \
  >"$external_cycle/rescript.json"
printf 'let value = CycleDep.value\n' >"$external_cycle/src/Root.res"
printf '%s\n' \
  '{"name":"cycle-dep","sources":"src","dependencies":["external-cycle-root"]}' \
  >"$external_cycle_dependency/rescript.json"
printf 'let value = Root.value\n' >"$external_cycle_dependency/src/CycleDep.res"
ln -s ../../store/cycle-dep "$external_cycle/node_modules/cycle-dep"
ln -s ../../../project \
  "$external_cycle_dependency/node_modules/external-cycle-root"

external_namespace="$external_diagnostics_base/namespace-project"
mkdir -p "$external_namespace/src" "$external_namespace/node_modules" \
  "$external_diagnostics_base/store/ns-one/src" \
  "$external_diagnostics_base/store/ns-two/src"
printf '%s\n' \
  '{"name":"external-namespace-root","sources":"src","dependencies":["ns-one","ns-two"]}' \
  >"$external_namespace/rescript.json"
printf 'let value = 1\n' >"$external_namespace/src/Main.res"
for namespace_dependency_name in ns-one ns-two; do
  printf '%s\n' \
    "{\"name\":\"$namespace_dependency_name\",\"namespace\":\"SharedNs\",\"sources\":\"src\"}" \
    >"$external_diagnostics_base/store/$namespace_dependency_name/rescript.json"
  printf 'let value = 1\n' \
    >"$external_diagnostics_base/store/$namespace_dependency_name/src/Value.res"
  ln -s "../../store/$namespace_dependency_name" \
    "$external_namespace/node_modules/$namespace_dependency_name"
done

dev_include_order="$work/dev-include-order"
mkdir -p "$dev_include_order/src" "$dev_include_order/dev" \
  "$dev_include_order/node_modules/regular/src" \
  "$dev_include_order/node_modules/development/src"
printf '%s\n' \
  '{"name":"dev-include-root","sources":["src",{"dir":"dev","type":"dev"}],"dependencies":["regular"],"dev-dependencies":["development"]}' \
  >"$dev_include_order/rescript.json"
printf 'let value = 1\n' >"$dev_include_order/src/Main.res"
printf 'let value = 2\n' >"$dev_include_order/dev/Test.res"
printf '%s\n' '{"name":"regular","sources":"src"}' \
  >"$dev_include_order/node_modules/regular/rescript.json"
printf 'let value = 1\n' \
  >"$dev_include_order/node_modules/regular/src/Regular.res"
printf '%s\n' '{"name":"development","sources":"src"}' \
  >"$dev_include_order/node_modules/development/rescript.json"
printf 'let value = 1\n' \
  >"$dev_include_order/node_modules/development/src/Development.res"

if [ -x "$port_directory/bsc.exe" ]; then
  env -u RESCRIPT_BSC_EXE "$port" build "$packaged_basic" \
    >"$packaged_basic/build.log"
  test -f "$packaged_basic/src/A.mjs"
fi

mkdir -p "$runtime_discovery/node_modules/@rescript/runtime"
runtime_path=$(CDPATH= cd -- \
  "$runtime_discovery/node_modules/@rescript/runtime" && pwd)
runtime_args=$(env -u RESCRIPT_RUNTIME \
  "$port" compiler-args "$runtime_discovery/src/A.res")
printf '%s\n' "$runtime_args" | grep -F "\"$runtime_path\"" >/dev/null

missing_project="$work/does-not-exist"
if "$port" build "$missing_project" >"$work/missing-project.log" 2>&1; then
  echo "build unexpectedly accepted a missing project folder" >&2
  exit 1
fi
grep -F \
  "Could not start Rescript build: Could not write lockfile because the specified project folder does not exist: $missing_project" \
  "$work/missing-project.log" >/dev/null

compiler_args_json=$("$port" compiler-args "$basic/src/A.res")
printf '%s\n' "$compiler_args_json" | grep '"compiler_args"' >/dev/null
printf '%s\n' "$compiler_args_json" | node -e '
  const path = require("path");
  let input = "";
  process.stdin.on("data", chunk => input += chunk);
  process.stdin.on("end", () => {
    const args = JSON.parse(input).parser_args;
    if (args.at(-1) !== path.join("..", "..", "src", "A.res")) process.exit(1);
  });
'
sed 's/"suffix": "\.mjs"/"suffix": "\.mjs", "bsc-flags": ["-w -9"]/' "$basic/rescript.json" > "$basic/rescript.next"
mv "$basic/rescript.next" "$basic/rescript.json"
sed 's/"module": "esmodule"/"module": "es6"/' "$basic/rescript.json" > "$basic/rescript.next"
mv "$basic/rescript.next" "$basic/rescript.json"
"$port" compiler-args "$basic/src/A.res" | grep '"-9"' >/dev/null
gentype_compiler_args=$("$port" compiler-args "$gentype/src/Main.res")
printf '%s\n' "$gentype_compiler_args" | grep '"-bs-gentype-generated-extension"' >/dev/null
printf '%s\n' "$gentype_compiler_args" | grep '"-bs-gentype-bsb-project-root"' >/dev/null
printf '%s\n' "$gentype_compiler_args" | node -e '
  let input = "";
  process.stdin.on("data", chunk => input += chunk);
  process.stdin.on("end", () => {
    const args = JSON.parse(input).compiler_args;
    const runtime = args.indexOf("-runtime-path");
    const dependencyInclude = args.indexOf("-I", 2);
    if (runtime < 0 || (dependencyInclude >= 0 && runtime > dependencyInclude)) {
      process.exit(1);
    }
  });
'
if printf '%s\n' "$gentype_compiler_args" | grep -E '"-bs-gentype-(dep-path|source-dir)"' >/dev/null; then
  echo "compiler-args unexpectedly included full-build GenType paths" >&2
  exit 1
fi

cleanup() {
  for pid in $background_pids; do
    kill -TERM "$pid" 2>/dev/null || true
  done
  for pid in $background_pids; do
    wait "$pid" 2>/dev/null || true
  done
  rm -rf "$work"
}
background_pids=""
trap cleanup EXIT

wait_for_file() {
  file="$1"
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    if [ -f "$file" ]; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

wait_for_text() {
  file="$1"
  pattern="$2"
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    if grep -q "$pattern" "$file" 2>/dev/null; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

wait_for_any_text() {
  file="$1"
  shift
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    for pattern in "$@"; do
      if grep -q "$pattern" "$file" 2>/dev/null; then
        return 0
      fi
    done
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

wait_for_count() {
  file="$1"
  pattern="$2"
  expected="$3"
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    count=$(grep -c "$pattern" "$file" 2>/dev/null) || count=0
    if [ "$count" -ge "$expected" ]; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

wait_for_file_gone() {
  file="$1"
  attempts=0
  while [ "$attempts" -lt 200 ]; do
    if [ ! -f "$file" ]; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

wait_for_pid_gone() {
  pid="$1"
  attempts=0
  while [ "$attempts" -lt 50 ]; do
    if ! kill -0 "$pid" 2>/dev/null; then
      return 0
    fi
    attempts=$((attempts + 1))
    sleep 0.1
  done
  return 1
}

printf 'let formatted=1\n' | "$port" format --stdin .res | grep 'let formatted = 1' >/dev/null
if printf 'let =\n' | "$port" format --stdin .res \
  >"$work/format-invalid.out" 2>"$work/format-invalid.err"; then
  echo "format stdin unexpectedly accepted invalid syntax" >&2
  exit 1
fi
grep -F "Error formatting stdin:" "$work/format-invalid.err" >/dev/null

format_stdin_tmp="$work/format-stdin-tmp"
format_stdin_fifo="$work/format-stdin.fifo"
mkdir -p "$format_stdin_tmp"
mkfifo "$format_stdin_fifo"
exec 9<>"$format_stdin_fifo"
TMPDIR="$format_stdin_tmp" "$port" format --stdin .res \
  <"$format_stdin_fifo" >"$work/format-signal.out" \
  2>"$work/format-signal.err" &
format_stdin_pid=$!
background_pids="$background_pids $format_stdin_pid"
attempts=0
while [ "$attempts" -lt 100 ] && [ -z "$(find "$format_stdin_tmp" -type f -print -quit)" ]; do
  attempts=$((attempts + 1))
  sleep 0.05
done
if [ "$attempts" -eq 100 ]; then
  echo "stdin formatter did not start reading" >&2
  exit 1
fi
kill -TERM "$format_stdin_pid"
if ! wait_for_pid_gone "$format_stdin_pid"; then
  echo "stdin formatter did not respond to termination" >&2
  exit 1
fi
wait "$format_stdin_pid" 2>/dev/null || true
exec 9>&-
if find "$format_stdin_tmp" -type f -print -quit | grep . >/dev/null; then
  echo "stdin formatter left a temporary file after termination" >&2
  exit 1
fi

printf 'let unformatted=1\n' >"$work/unformatted.res"
if "$port" format --check "$work/unformatted.res" \
  >"$work/format-check.out" 2>"$work/format-check.err"; then
  echo "format check unexpectedly accepted an unformatted file" >&2
  exit 1
fi
grep -F "[format check] $work/unformatted.res" \
  "$work/format-check.err" >/dev/null
grep -F "The file listed above needs formatting" \
  "$work/format-check.err" >/dev/null
grep -F "Formatting check failed" "$work/format-check.err" >/dev/null

"$port" build "$no_bin_annot" >/dev/null
test -f "$no_bin_annot/lib/bs/src/NoBinAnnot.cmi"
test -f "$no_bin_annot/lib/bs/src/NoBinAnnot.cmj"
test -f "$no_bin_annot/src/NoBinAnnot.js"
test ! -e "$no_bin_annot/lib/bs/src/NoBinAnnot.cmt"
test ! -e "$no_bin_annot/lib/ocaml/NoBinAnnot.cmt"

if (cd "$basic/src" && "$port" format --check) \
  >"$work/format-nested.out" 2>"$work/format-nested.err"; then
  echo "format unexpectedly searched above the current directory" >&2
  exit 1
fi
grep -F "Could not read rescript.json at $basic/src" \
  "$work/format-nested.err" >/dev/null

rm -rf "$basic/lib" "$cycle/lib" "$failure/lib"
rm -rf "$legacy_config/lib"
mv "$legacy_config/rescript.json" "$legacy_config/bsconfig.json"
rm -rf "$features/lib"
rm -rf "$gentype/lib"
rm -rf "$gentype/node_modules/dep/lib"
rm -rf "$dependency/lib" "$dependency/node_modules/dep/lib"
sed 's/"dependencies"/"bs-dependencies"/' "$dependency/rescript.json" > "$dependency/rescript.next"
mv "$dependency/rescript.next" "$dependency/rescript.json"
sed 's/}$/,"suffix":".mjs"}/' "$dependency/rescript.json" > "$dependency/rescript.next"
mv "$dependency/rescript.next" "$dependency/rescript.json"
sed 's/}$/,"suffix":".cjs"}/' "$dependency/node_modules/dep/rescript.json" > "$dependency/node_modules/dep/rescript.next"
mv "$dependency/node_modules/dep/rescript.next" "$dependency/node_modules/dep/rescript.json"
rm -rf "$post_build/lib"
rm -rf "$ppx_filter/lib"
rm -rf "$out_of_source/lib"
rm -rf "$namespace/lib"
rm -rf "$namespace_entry/lib"
rm -rf "$source_map/lib"
mkdir -p "$monorepo/node_modules"
ln -s ../packages/consumer "$monorepo/node_modules/consumer"
ln -s ../packages/dep "$monorepo/node_modules/dep"

# A command run from a listed workspace package may resolve sibling packages,
# but those siblings are ordinary dependencies for this invocation. In
# particular, their development-only dependency graph must remain dormant and
# their sources must not be added to the invoking package's analysis metadata.
cp "$monorepo/packages/dep/rescript.json" \
  "$monorepo/packages/dep/rescript.original"
printf '%s\n' \
  '{"name":"dep","sources":"src","dev-dependencies":["missing-dev"]}' \
  >"$monorepo/packages/dep/rescript.json"
"$port" build "$monorepo/packages/consumer"
test -f "$monorepo/packages/consumer/src/Consumer.js"
test -f "$monorepo/packages/dep/src/Dep.js"
test ! -f "$monorepo/packages/dep/lib/bs/.sourcedirs.json"
node - "$monorepo/packages/consumer/lib/bs/.sourcedirs.json" \
  "$monorepo/packages/dep" <<'NODE'
const fs = require("fs");
const [sourceDirsPath, dependencyPath] = process.argv.slice(2);
const sourceDirs = JSON.parse(fs.readFileSync(sourceDirsPath, "utf8"));
if (JSON.stringify(sourceDirs.dirs) !== JSON.stringify(["src"])) {
  throw new Error(`unexpected direct-package source dirs: ${JSON.stringify(sourceDirs.dirs)}`);
}
if (sourceDirs.cmt_scan.length !== 1 || sourceDirs.cmt_scan[0].build_root !== "lib/bs") {
  throw new Error(`unexpected direct-package scan plan: ${JSON.stringify(sourceDirs.cmt_scan)}`);
}
const packages = new Map(sourceDirs.pkgs);
if (packages.get("dep") !== dependencyPath) {
  throw new Error(`missing absolute sibling dependency path: ${JSON.stringify(sourceDirs.pkgs)}`);
}
NODE
mv "$monorepo/packages/dep/rescript.original" \
  "$monorepo/packages/dep/rescript.json"
rm -f "$basic/src/A.mjs" "$basic/src/B.mjs" "$basic/src/WithInterface.mjs"

"$port" build "$legacy_config"
test -f "$legacy_config/src/A.mjs"

"$port" build --filter 'A\.res$' "$basic"
test -f "$basic/src/A.mjs"
test -f "$basic/src/Authored.js"
test ! -f "$basic/src/B.mjs"
rm -rf "$basic/lib"
rm -f "$basic/src/A.mjs"
mkdir -p "$basic/lib/bs/other"
touch "$basic/lib/bs/other/Authored.js"

"$port" build --after-build 'test -f src/A.mjs' "$basic"
test -f "$basic/lib/bs/build.ninja"
test -f "$basic/src/A.mjs"
test -f "$basic/src/Authored.js"
test -f "$basic/src/B.mjs"
test -f "$basic/src/WithInterface.mjs"
test -f "$basic/lib/ocaml/A.cmi"
test -f "$basic/lib/ocaml/WithInterface.cmti"

printf '\nlet streamedAfterBuild = 1\n' >>"$basic/src/A.res"
after_build_release="$basic/after-build-release"
after_build_log="$basic/after-build-stream.log"
"$port" build --after-build \
  "$root/rewatch-ocaml/tests/stream-after-build.sh $after_build_release" \
  "$basic" >"$after_build_log" 2>&1 &
after_build_pid=$!
background_pids="$background_pids $after_build_pid"
if ! wait_for_text "$after_build_log" REWATCH_AFTER_BUILD_READY; then
  cat "$after_build_log" >&2
  exit 1
fi
touch "$after_build_release"
wait "$after_build_pid"

# Keep a stale working CMI only while its dependents compile, so bsc can emit
# its source-level missing-module diagnostic. It must not survive the command.
"$port" build "$cleanup_lifecycle" >/dev/null
mv "$cleanup_lifecycle/src/A.res" "$cleanup_lifecycle/src/A2.res"
if "$port" build "$cleanup_lifecycle" >/dev/null 2>&1; then
  echo "build after a depended-on rename unexpectedly succeeded" >&2
  exit 1
fi
test ! -f "$cleanup_lifecycle/lib/bs/src/A.cmi"
test ! -f "$cleanup_lifecycle/lib/ocaml/A.cmi"

# Destructive cleanup must invalidate consumers before a later deletion error
# can abort the command and erase the removed module's AST provenance.
"$port" build "$cleanup_failure" >/dev/null
rm "$cleanup_failure/src/A.res" "$cleanup_failure/src/A.mjs"
mkdir "$cleanup_failure/src/A.mjs"
if "$port" build "$cleanup_failure" >/dev/null 2>&1; then
  echo "cleanup with an obstructing output unexpectedly succeeded" >&2
  exit 1
fi
rmdir "$cleanup_failure/src/A.mjs"
if "$port" build "$cleanup_failure" \
  >"$cleanup_failure/retry.log" 2>&1; then
  echo "cleanup failure forgot the removed dependency" >&2
  exit 1
fi
grep -F 'A' "$cleanup_failure/retry.log" >/dev/null

"$port" build "$config_cleanup_invalidation" >/dev/null
printf '%s\n' '{"name":"dep","sources":[]}' \
  >"$config_cleanup_invalidation/packages/dep/rescript.json"
if "$port" build "$config_cleanup_invalidation" \
  >"$config_cleanup_invalidation/removal.log" 2>&1; then
  echo "config cleanup forgot a removed dependency" >&2
  exit 1
fi
grep -F 'Dep' "$config_cleanup_invalidation/removal.log" >/dev/null

"$port" build "$package_name_mismatch" >/dev/null
printf 'let value = "changed"\n' \
  >"$package_name_mismatch/packages/dep/src/Dep.res"
if "$port" build "$package_name_mismatch" \
  >"$package_name_mismatch/mismatch.log" 2>&1; then
  echo "package metadata mismatch dropped a dependency edge" >&2
  exit 1
fi
grep -F 'string' "$package_name_mismatch/mismatch.log" >/dev/null

sed 's/"suffix": "\.mjs"/"suffix": ".custom"/' \
  "$suffix_removal/rescript.json" >"$suffix_removal/rescript.next"
mv "$suffix_removal/rescript.next" "$suffix_removal/rescript.json"
"$port" build "$suffix_removal" >/dev/null
test -f "$suffix_removal/src/A.custom"
rm "$suffix_removal/src/A.res"
sed 's/"suffix": "\.custom"/"suffix": ".js"/' \
  "$suffix_removal/rescript.json" >"$suffix_removal/rescript.next"
mv "$suffix_removal/rescript.next" "$suffix_removal/rescript.json"
if "$port" build "$suffix_removal" >/dev/null 2>&1; then
  echo "suffix change with a removed dependency unexpectedly succeeded" >&2
  exit 1
fi
test ! -e "$suffix_removal/src/A.custom"

sed 's/"suffix": "\.mjs"/"suffix": ".custom"/' \
  "$suffix_failure/rescript.json" >"$suffix_failure/rescript.next"
mv "$suffix_failure/rescript.next" "$suffix_failure/rescript.json"
"$port" build "$suffix_failure" >/dev/null
test -f "$suffix_failure/src/A.custom"
printf 'let value: int = "broken"\n' >"$suffix_failure/src/A.res"
if "$port" build "$suffix_failure" >/dev/null 2>&1; then
  echo "invalid source unexpectedly compiled before suffix change" >&2
  exit 1
fi
printf 'let value = 41\n' >"$suffix_failure/src/A.res"
sed 's/"suffix": "\.custom"/"suffix": ".js"/' \
  "$suffix_failure/rescript.json" >"$suffix_failure/rescript.next"
mv "$suffix_failure/rescript.next" "$suffix_failure/rescript.json"
"$port" build "$suffix_failure" >/dev/null
test ! -e "$suffix_failure/src/A.custom"
test -f "$suffix_failure/src/A.js"

# A successful parse must remain compile-dirty when another file aborts the
# same build before compilation starts.
cp "$basic/src/B.res" "$basic/src/B.backup"
printf '\nlet recoveredAfterPeerParseFailure = 42\n' >> "$basic/src/A.res"
printf 'let broken =\n' > "$basic/src/B.res"
if "$port" build "$basic" >/dev/null 2>&1; then
  echo "build with parser error unexpectedly succeeded" >&2
  exit 1
fi
mv "$basic/src/B.backup" "$basic/src/B.res"
"$port" build "$basic" >/dev/null
grep 'recoveredAfterPeerParseFailure' "$basic/src/A.mjs" >/dev/null

# Removing an interface from a lowercase-named source must rebuild the
# implementation before dependents can observe exports hidden by that interface.
printf 'let visible = 1\nlet hidden = 2\n' > "$basic/src/lower.res"
printf 'let visible: int\n' > "$basic/src/lower.resi"
printf 'let value = Lower.visible\n' > "$basic/src/LowerConsumer.res"
"$port" build "$basic" >/dev/null
rm "$basic/src/lower.resi"
printf 'let value = Lower.hidden\n' > "$basic/src/LowerConsumer.res"
"$port" build "$basic" >/dev/null
grep 'hidden' "$basic/src/LowerConsumer.mjs" >/dev/null
test ! -f "$basic/lib/ocaml/lower.cmti"
test ! -f "$basic/lib/bs/src/lower.cmti"

"$port" clean "$basic"
test ! -f "$basic/src/A.mjs"

watch_basic="$work/watch-basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$watch_basic"
rm -rf "$watch_basic/lib"
rm -f "$watch_basic/src/A.mjs" "$watch_basic/src/B.mjs" "$watch_basic/src/WithInterface.mjs"
"$port" watch "$watch_basic" >"$watch_basic/watch.log" 2>&1 &
watch_pid=$!
background_pids="$background_pids $watch_pid"
if ! wait_for_file "$watch_basic/src/A.mjs"; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
test -f "$watch_basic/lib/watch.lock"
grep '^[0-9][0-9]*$' "$watch_basic/lib/watch.lock" >/dev/null
printf '\nlet watchedValue = 1\n' >> "$watch_basic/src/B.res"
if ! wait_for_text "$watch_basic/src/B.mjs" 'watchedValue'; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
sed 's/"\.mjs"/".js"/' "$watch_basic/rescript.json" > "$watch_basic/rescript.next"
mv "$watch_basic/rescript.next" "$watch_basic/rescript.json"
if ! wait_for_file "$watch_basic/src/A.js"; then
  cat "$watch_basic/watch.log" >&2
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
test -f "$watch_basic/src/A.js"
test ! -f "$watch_basic/src/A.mjs"
printf 'let message = "new source"\n' > "$watch_basic/src/New.res"
if ! wait_for_file "$watch_basic/src/New.js"; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
test -f "$watch_basic/src/New.js"
mkdir "$watch_basic/src/new-directory"
printf 'let nested = "new directory"\n' \
  > "$watch_basic/src/new-directory/Nested.res"
if ! wait_for_file "$watch_basic/src/new-directory/Nested.js"; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
rm -f "$watch_basic/src/New.res"
if ! wait_for_file_gone "$watch_basic/src/New.js"; then
  kill -TERM "$watch_pid" 2>/dev/null || true
  wait "$watch_pid" 2>/dev/null || true
  exit 1
fi
test ! -f "$watch_basic/src/New.js"

# A successfully published interface must invalidate its dependents even when
# the corresponding implementation fails and aborts the rest of that build.
interface_failure_log="$interface_failure_recovery/watch.log"
"$port" watch "$interface_failure_recovery" \
  >"$interface_failure_log" 2>&1 &
interface_failure_pid=$!
background_pids="$background_pids $interface_failure_pid"
if ! wait_for_file "$interface_failure_recovery/src/B.mjs"; then
  cat "$interface_failure_log" >&2
  exit 1
fi
printf 'let value: string\n' \
  >"$interface_failure_recovery/src/A.resi"
if ! wait_for_text "$interface_failure_log" 'does not match the interface'; then
  cat "$interface_failure_log" >&2
  exit 1
fi
printf 'let value = "fixed"\n' \
  >"$interface_failure_recovery/src/A.res"
if ! wait_for_text "$interface_failure_log" 'This has type:'; then
  cat "$interface_failure_log" >&2
  exit 1
fi
kill -TERM "$interface_failure_pid"
wait "$interface_failure_pid" 2>/dev/null || true

# A parsed interface remains pending when another source prevents compilation.
printf 'let value: int\n' >"$interface_failure_recovery/src/A.resi"
printf 'let value = 1\n' >"$interface_failure_recovery/src/A.res"
"$port" build "$interface_failure_recovery"
printf 'let value: string\n' \
  >"$interface_failure_recovery/src/A.resi"
printf 'let =\n' >"$interface_failure_recovery/src/Broken.res"
if "$port" build "$interface_failure_recovery" \
  >"$interface_failure_recovery/pending-interface.log" 2>&1; then
  echo "build unexpectedly accepted the broken source" >&2
  exit 1
fi
printf 'let valid = true\n' >"$interface_failure_recovery/src/Broken.res"
if "$port" build "$interface_failure_recovery" \
  >>"$interface_failure_recovery/pending-interface.log" 2>&1; then
  echo "pending interface change was forgotten after a parse failure" >&2
  exit 1
fi
grep 'does not match the interface' \
  "$interface_failure_recovery/pending-interface.log" >/dev/null

"$port" build "$directory_symlink"
"$port" build "$directory_symlink" >"$directory_symlink/unchanged.log" 2>&1
grep 'Parsed 0 source files' "$directory_symlink/unchanged.log" >/dev/null
grep 'Compiled 0 modules' "$directory_symlink/unchanged.log" >/dev/null

# Record the content that triggered a direct event before building it. An
# atomic replacement made while that build is compiling must cause a second
# build rather than becoming the post-build snapshot baseline.
atomic_release="$atomic_save/release"
atomic_started="$atomic_save/compile-started"
touch "$atomic_release"
env RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/block-compile-bsc.sh" \
  REWATCH_OCAML_REAL_BSC="$RESCRIPT_BSC_EXE" \
  REWATCH_OCAML_RELEASE_FILE="$atomic_release" \
  REWATCH_OCAML_COMPILE_STARTED="$atomic_started" \
  "$port" watch "$atomic_save" >"$atomic_save/watch.log" 2>&1 &
atomic_save_pid=$!
background_pids="$background_pids $atomic_save_pid"
if ! wait_for_file "$atomic_save/src/Main.mjs"; then
  cat "$atomic_save/watch.log" >&2
  exit 1
fi
rm "$atomic_release" "$atomic_started"
printf 'let value = 2\n' >"$atomic_save/src/Main.res"
if ! wait_for_file "$atomic_started"; then
  cat "$atomic_save/watch.log" >&2
  exit 1
fi
printf 'let value = 3\n' >"$atomic_save/src/Main.next"
mv "$atomic_save/src/Main.next" "$atomic_save/src/Main.res"
touch "$atomic_release"
if ! wait_for_text "$atomic_save/src/Main.mjs" 'value = 3'; then
  cat "$atomic_save/watch.log" >&2
  cat "$atomic_save/src/Main.mjs" >&2
  exit 1
fi
kill -TERM "$atomic_save_pid"
wait "$atomic_save_pid" 2>/dev/null || true

"$port" watch "$recursive_lib" >"$recursive_lib/watch.log" 2>&1 &
recursive_lib_pid=$!
background_pids="$background_pids $recursive_lib_pid"
if ! wait_for_file "$recursive_lib/src/lib/Nested.mjs"; then
  cat "$recursive_lib/watch.log" >&2
  exit 1
fi
printf 'let value = 2\n' >"$recursive_lib/src/lib/Nested.res"
if ! wait_for_text "$recursive_lib/src/lib/Nested.mjs" 'value = 2'; then
  cat "$recursive_lib/watch.log" >&2
  exit 1
fi
kill -TERM "$recursive_lib_pid"
wait "$recursive_lib_pid" 2>/dev/null || true

"$port" watch --filter '^Main\.res$' "$filtered_dependency" \
  >"$filtered_dependency/watch.log" 2>&1 &
filtered_dependency_pid=$!
background_pids="$background_pids $filtered_dependency_pid"
if ! wait_for_file "$filtered_dependency/packages/dep/src/Helper.mjs"; then
  cat "$filtered_dependency/watch.log" >&2
  exit 1
fi
test -f "$filtered_dependency/src/Main.mjs"
test ! -f "$filtered_dependency/src/Ignored.mjs"
printf 'let value = 2\n' \
  >"$filtered_dependency/packages/dep/src/Helper.res"
if ! wait_for_text "$filtered_dependency/packages/dep/src/Helper.mjs" \
  'value = 2'; then
  cat "$filtered_dependency/watch.log" >&2
  exit 1
fi
kill -TERM "$filtered_dependency_pid"
wait "$filtered_dependency_pid" 2>/dev/null || true
printf 'let value = { let unused = 2; 1 }\n' \
  >"$filtered_dependency/packages/dep/src/Helper.res"
if "$port" build --warn-error A "$filtered_dependency" \
  >"$filtered_dependency/warn-error.log" 2>&1; then
  echo "--warn-error did not apply to a local dependency" >&2
  exit 1
fi

"$port" watch "$symlink_source" >"$symlink_source/watch.log" 2>&1 &
symlink_source_pid=$!
background_pids="$background_pids $symlink_source_pid"
if ! wait_for_file "$symlink_source/src/Linked.mjs"; then
  cat "$symlink_source/watch.log" >&2
  exit 1
fi
printf 'let value = 2\n' >"$symlink_source/shared/Source.js"
if ! wait_for_text "$symlink_source/src/Linked.mjs" 'value = 2'; then
  cat "$symlink_source/watch.log" >&2
  exit 1
fi
symlink_replacement=$(mktemp "${TMPDIR:-/tmp}/rewatch-symlink-target.XXXXXX")
printf 'let value = 3\n' >"$symlink_replacement"
mv "$symlink_replacement" "$symlink_source/shared/Source.js"
if ! wait_for_text "$symlink_source/src/Linked.mjs" 'value = 3'; then
  cat "$symlink_source/watch.log" >&2
  exit 1
fi
kill -TERM "$symlink_source_pid"
wait "$symlink_source_pid" 2>/dev/null || true

"$port" watch "$symlink_alias" >"$symlink_alias/watch.log" 2>&1 &
symlink_alias_pid=$!
background_pids="$background_pids $symlink_alias_pid"
if ! wait_for_file "$symlink_alias/src/Linked.mjs"; then
  cat "$symlink_alias/watch.log" >&2
  exit 1
fi
printf 'let value = 2\n' >"$symlink_alias/src/Original.res"
if ! wait_for_text "$symlink_alias/src/Original.mjs" 'value = 2' \
  || ! wait_for_text "$symlink_alias/src/Linked.mjs" 'value = 2'; then
  cat "$symlink_alias/watch.log" >&2
  exit 1
fi
kill -TERM "$symlink_alias_pid"
wait "$symlink_alias_pid" 2>/dev/null || true

"$port" watch "$post_build_cmi" >"$post_build_cmi/watch.log" 2>&1 &
post_build_cmi_pid=$!
background_pids="$background_pids $post_build_cmi_pid"
if ! wait_for_file "$post_build_cmi/src/B.mjs"; then
  cat "$post_build_cmi/watch.log" >&2
  exit 1
fi
rm "$post_build_cmi/allow-post-build"
printf 'let value = "changed"\n' >"$post_build_cmi/src/A.res"
if ! wait_for_text "$post_build_cmi/watch.log" \
  'js-post-build command failed'; then
  cat "$post_build_cmi/watch.log" >&2
  exit 1
fi
touch "$post_build_cmi/allow-post-build"
printf 'let value = "changed"\n\n' >"$post_build_cmi/src/A.res"
if ! wait_for_text "$post_build_cmi/watch.log" 'This has type:'; then
  cat "$post_build_cmi/watch.log" >&2
  exit 1
fi
kill -TERM "$post_build_cmi_pid"
wait "$post_build_cmi_pid" 2>/dev/null || true

"$port" build "$post_build_retry"
rm "$post_build_retry/allow-post-build"
printf 'let value = 2\n' >"$post_build_retry/src/A.res"
if "$port" build "$post_build_retry" \
  >"$post_build_retry/first-failure.log" 2>&1; then
  echo "post-build failure unexpectedly succeeded" >&2
  exit 1
fi
if "$port" build "$post_build_retry" \
  >"$post_build_retry/retry-failure.log" 2>&1; then
  echo "a fresh build forgot the failed post-build command" >&2
  exit 1
fi
grep 'js-post-build command failed' \
  "$post_build_retry/retry-failure.log" >/dev/null
touch "$post_build_retry/allow-post-build"
"$port" build "$post_build_retry"
grep 'value = 2' "$post_build_retry/src/A.mjs" >/dev/null

"$port" build "$moved_source"
mv "$moved_source/src/A.res" "$moved_source/src/nested/A.res"
"$port" build "$moved_source"
test ! -f "$moved_source/src/A.mjs"
grep 'value = 1' "$moved_source/src/nested/A.mjs" >/dev/null

"$port" watch "$parse_publication" \
  >"$parse_publication/watch.log" 2>&1 &
parse_publication_pid=$!
background_pids="$background_pids $parse_publication_pid"
if ! wait_for_file "$parse_publication/src/A.mjs"; then
  cat "$parse_publication/watch.log" >&2
  exit 1
fi
parse_destination="$parse_publication/lib/ocaml/A.res"
rm "$parse_destination"
mkdir "$parse_destination"
printf 'let value = 2\n' >"$parse_publication/src/A.res"
if ! wait_for_text "$parse_publication/watch.log" "$parse_destination"; then
  cat "$parse_publication/watch.log" >&2
  exit 1
fi
rmdir "$parse_destination"
printf 'let other = 2\n' >"$parse_publication/src/B.res"
if ! wait_for_text "$parse_publication/src/A.mjs" 'value = 2'; then
  cat "$parse_publication/watch.log" >&2
  echo "parse publication failure forgot the changed source" >&2
  exit 1
fi
kill -TERM "$parse_publication_pid"
wait "$parse_publication_pid" 2>/dev/null || true

"$port" watch "$multi_package_pending" \
  >"$multi_package_pending/watch.log" 2>&1 &
multi_package_pending_pid=$!
background_pids="$background_pids $multi_package_pending_pid"
if ! wait_for_file "$multi_package_pending/src/Main.mjs"; then
  cat "$multi_package_pending/watch.log" >&2
  exit 1
fi
dep1_destination="$multi_package_pending/packages/dep1/lib/ocaml/A.res"
rm "$dep1_destination"
mkdir "$dep1_destination"
printf 'let value = 2\n' >"$multi_package_pending/packages/dep2/src/B.res"
printf 'let value = 2\n' >"$multi_package_pending/packages/dep1/src/A.res"
if ! wait_for_text "$multi_package_pending/watch.log" "$dep1_destination"; then
  cat "$multi_package_pending/watch.log" >&2
  exit 1
fi
rmdir "$dep1_destination"
printf 'let other = 2\n' >"$multi_package_pending/src/Other.res"
if ! wait_for_text "$multi_package_pending/packages/dep2/src/B.mjs" \
  'value = 2'; then
  cat "$multi_package_pending/watch.log" >&2
  echo "an earlier package failure forgot another package's edit" >&2
  exit 1
fi
kill -TERM "$multi_package_pending_pid"
wait "$multi_package_pending_pid" 2>/dev/null || true

"$port" watch "$full_watch_recovery" \
  >"$full_watch_recovery/watch.log" 2>&1 &
full_watch_recovery_pid=$!
background_pids="$background_pids $full_watch_recovery_pid"
if ! wait_for_file "$full_watch_recovery/src/A.mjs"; then
  cat "$full_watch_recovery/watch.log" >&2
  exit 1
fi
full_watch_destination="$full_watch_recovery/lib/ocaml/C.res"
rm "$full_watch_destination"
mkdir "$full_watch_destination"
rm "$full_watch_recovery/src/B.res"
printf 'let value = 2\n' >"$full_watch_recovery/src/C.res"
if ! wait_for_text "$full_watch_recovery/watch.log" \
  "$full_watch_destination"; then
  cat "$full_watch_recovery/watch.log" >&2
  exit 1
fi
rmdir "$full_watch_destination"
printf 'let value = 3\n' >"$full_watch_recovery/src/C.res"
if ! wait_for_any_text "$full_watch_recovery/watch.log" \
  'I/O error: ../ocaml/b.cmi' \
  "B can't be found."; then
  cat "$full_watch_recovery/watch.log" >&2
  echo "full-watch parse recovery forgot deleted-dependency invalidation" >&2
  exit 1
fi
kill -TERM "$full_watch_recovery_pid"
wait "$full_watch_recovery_pid" 2>/dev/null || true

"$port" build "$deletion_retry" >/dev/null
rm "$deletion_retry/src/B.res"
printf 'let value =\n' >"$deletion_retry/src/C.res"
if "$port" build "$deletion_retry" >/dev/null 2>&1; then
  echo "deletion plus parse failure unexpectedly succeeded" >&2
  exit 1
fi
printf 'let value = 2\n' >"$deletion_retry/src/C.res"
if "$port" build "$deletion_retry" \
  >"$deletion_retry/retry.log" 2>&1; then
  echo "a fresh build forgot deleted-dependency invalidation" >&2
  exit 1
fi
grep 'b.cmi' "$deletion_retry/retry.log" >/dev/null

"$port" build "$namespace_repair" >/dev/null
namespace_repair_destination="$namespace_repair/packages/dep/lib/ocaml/Ns.cmj"
rm "$namespace_repair_destination"
mkdir "$namespace_repair_destination"
"$port" watch "$namespace_repair" >"$namespace_repair/watch.log" 2>&1 &
namespace_repair_pid=$!
background_pids="$background_pids $namespace_repair_pid"
if ! wait_for_text "$namespace_repair/watch.log" \
  "$namespace_repair_destination"; then
  cat "$namespace_repair/watch.log" >&2
  exit 1
fi
rmdir "$namespace_repair_destination"
printf 'let other = 2\n' >"$namespace_repair/src/Other.res"
if ! wait_for_file "$namespace_repair_destination" \
  || ! wait_for_text "$namespace_repair/src/Other.mjs" 'other = 2'; then
  cat "$namespace_repair/watch.log" >&2
  echo "failed namespace repair was not retried" >&2
  exit 1
fi
kill -TERM "$namespace_repair_pid"
wait "$namespace_repair_pid" 2>/dev/null || true

publication_destination="$publication_cmi/lib/bs/src/A.res"
env RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/fail-late-publication-bsc.sh" \
  REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
  REWATCH_FAIL_PUBLICATION="$publication_cmi/fail-publication" \
  REWATCH_PUBLICATION_FAILED="$publication_cmi/publication-failed" \
  REWATCH_PUBLICATION_DESTINATION="$publication_destination" \
  "$port" watch "$publication_cmi" >"$publication_cmi/watch.log" 2>&1 &
publication_cmi_pid=$!
background_pids="$background_pids $publication_cmi_pid"
if ! wait_for_file "$publication_cmi/src/B.mjs"; then
  cat "$publication_cmi/watch.log" >&2
  exit 1
fi
touch "$publication_cmi/fail-publication"
printf 'let value = "changed"\n' >"$publication_cmi/src/A.res"
if ! wait_for_file "$publication_cmi/publication-failed" || \
  ! wait_for_text "$publication_cmi/watch.log" 'A.res'; then
  cat "$publication_cmi/watch.log" >&2
  exit 1
fi
rmdir "$publication_destination"
rm "$publication_cmi/fail-publication"
printf 'let value = "changed"\n\n' >"$publication_cmi/src/A.res"
if ! wait_for_text "$publication_cmi/watch.log" 'This has type:'; then
  cat "$publication_cmi/watch.log" >&2
  exit 1
fi
kill -TERM "$publication_cmi_pid"
wait "$publication_cmi_pid" 2>/dev/null || true

"$port" watch "$retained_cycle" >"$retained_cycle/watch.log" 2>&1 &
retained_cycle_pid=$!
background_pids="$background_pids $retained_cycle_pid"
if ! wait_for_file "$retained_cycle/src/A.mjs"; then
  cat "$retained_cycle/watch.log" >&2
  exit 1
fi
printf 'let value: int = "broken"\n' >"$retained_cycle/src/A.res"
if ! wait_for_text "$retained_cycle/watch.log" 'expected to have type'; then
  cat "$retained_cycle/watch.log" >&2
  exit 1
fi
printf 'let value = B.dependent\n' >"$retained_cycle/src/A.res"
if ! wait_for_text "$retained_cycle/watch.log" 'circular dependency'; then
  cat "$retained_cycle/watch.log" >&2
  exit 1
fi
if grep -F 'from "./B.mjs"' "$retained_cycle/src/A.mjs" >/dev/null; then
  cat "$retained_cycle/src/A.mjs" >&2
  cat "$retained_cycle/watch.log" >&2
  echo "cycle member was compiled from retained dirty state" >&2
  exit 1
fi
printf 'let dependent = 1\n' >"$retained_cycle/src/B.res"
if ! wait_for_text "$retained_cycle/src/A.mjs" 'from "./B.mjs"'; then
  cat "$retained_cycle/src/A.mjs" >&2
  cat "$retained_cycle/watch.log" >&2
  echo "cycle recovery did not compile retained pending work" >&2
  exit 1
fi
kill -TERM "$retained_cycle_pid"
wait "$retained_cycle_pid" 2>/dev/null || true

# Initial watch recovery must retain freshness work discovered alongside an
# unrelated parse failure.
"$port" build "$initial_failure_freshness" >/dev/null
rm "$initial_failure_freshness/lib/ocaml/A.cmi"
printf 'let other =\n' >"$initial_failure_freshness/src/B.res"
"$port" watch "$initial_failure_freshness" \
  >"$initial_failure_freshness/watch.log" 2>&1 &
initial_failure_freshness_pid=$!
background_pids="$background_pids $initial_failure_freshness_pid"
if ! wait_for_text "$initial_failure_freshness/watch.log" \
  'Error in initial-failure-freshness'; then
  cat "$initial_failure_freshness/watch.log" >&2
  exit 1
fi
printf 'let other = 2\n' >"$initial_failure_freshness/src/B.res"
if ! wait_for_file "$initial_failure_freshness/lib/ocaml/A.cmi"; then
  cat "$initial_failure_freshness/watch.log" >&2
  echo "initial parse recovery forgot missing compile artifacts" >&2
  exit 1
fi
kill -TERM "$initial_failure_freshness_pid"
wait "$initial_failure_freshness_pid" 2>/dev/null || true

# Failed parses and parser warnings remain pending until the same source parses
# cleanly. An unrelated edit must not compile an older AST or forget diagnostics.
retained_parse_log="$retained_parse/watch.log"
retained_parse_calls="$retained_parse/bsc-calls.log"
env RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/parse-warning-bsc.sh" \
  REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
  REWATCH_BSC_CALL_LOG="$retained_parse_calls" \
  REWATCH_PARSE_WARNING_SOURCE=A.res \
  "$port" watch "$retained_parse" >"$retained_parse_log" 2>&1 &
retained_parse_pid=$!
background_pids="$background_pids $retained_parse_pid"
if ! wait_for_count "$retained_parse_log" REWATCH_PARSE_WARNING 1 || \
  ! wait_for_file "$retained_parse/src/A.mjs"; then
  cat "$retained_parse_log" >&2
  exit 1
fi
initial_a_parse_count=$(grep -c -- '-bs-ast.*A.res' "$retained_parse_calls")
printf 'let other = 2\n' >"$retained_parse/src/B.res"
if ! wait_for_count "$retained_parse_log" REWATCH_PARSE_WARNING 2; then
  cat "$retained_parse_log" >&2
  exit 1
fi
warning_a_parse_count=$(grep -c -- '-bs-ast.*A.res' "$retained_parse_calls")
test "$warning_a_parse_count" -gt "$initial_a_parse_count"
printf 'let value =\n' >"$retained_parse/src/A.res"
if ! wait_for_count "$retained_parse_log" 'Error in retained-parse' 1; then
  cat "$retained_parse_log" >&2
  exit 1
fi
failed_a_parse_count=$(grep -c -- '-bs-ast.*A.res' "$retained_parse_calls")
printf 'let other = 3\n' >"$retained_parse/src/B.res"
if ! wait_for_count "$retained_parse_log" 'Error in retained-parse' 2; then
  cat "$retained_parse_log" >&2
  exit 1
fi
retried_a_parse_count=$(grep -c -- '-bs-ast.*A.res' "$retained_parse_calls")
test "$retried_a_parse_count" -gt "$failed_a_parse_count"
grep 'value = 1' "$retained_parse/src/A.mjs" >/dev/null
printf 'let value = 2\n' >"$retained_parse/src/A.res"
if ! wait_for_text "$retained_parse/src/A.mjs" 'value = 2'; then
  cat "$retained_parse_log" >&2
  exit 1
fi
kill -TERM "$retained_parse_pid"
wait "$retained_parse_pid" 2>/dev/null || true

"$port" build "$duplicate_selection" >/dev/null
"$port" build "$external_duplicate" >"$external_duplicate/build.log" 2>&1
grep -F "$external_duplicate_base/node_modules/shared" \
  "$external_duplicate/build.log" >/dev/null
if "$port" build "$external_cycle" \
  >"$external_cycle/build.log" 2>&1; then
  echo "external dependency cycle unexpectedly built" >&2
  exit 1
fi
grep -F "circular dependency" "$external_cycle/build.log" >/dev/null
grep -F "$external_cycle_dependency/src/CycleDep.res" \
  "$external_cycle/build.log" >/dev/null
if "$port" build "$external_namespace" \
  >"$external_namespace/build.log" 2>&1; then
  echo "external namespace collision unexpectedly built" >&2
  exit 1
fi
grep -F "Namespace SharedNs is provided by both" \
  "$external_namespace/build.log" >/dev/null
grep -F "$external_diagnostics_base/store/ns-one" \
  "$external_namespace/build.log" >/dev/null
mkdir -p \
  "$duplicate_selection/node_modules/a/node_modules/shared/lib/ocaml"
printf 'preserve nested duplicate\n' \
  >"$duplicate_selection/node_modules/a/node_modules/shared/lib/ocaml/marker"
"$port" clean "$duplicate_selection" >/dev/null
test ! -f "$duplicate_selection/node_modules/shared/src/Shared.mjs"
test -f \
  "$duplicate_selection/node_modules/a/node_modules/shared/lib/ocaml/marker"
"$port" watch "$duplicate_selection" \
  >"$duplicate_selection/watch.log" 2>&1 &
duplicate_selection_pid=$!
background_pids="$background_pids $duplicate_selection_pid"
if ! wait_for_file "$duplicate_selection/src/Main.mjs"; then
  cat "$duplicate_selection/watch.log" >&2
  exit 1
fi
printf 'let value = 3\n' \
  >"$duplicate_selection/node_modules/shared/src/Shared.res"
if ! wait_for_text \
  "$duplicate_selection/node_modules/shared/src/Shared.mjs" 'value = 3'; then
  cat "$duplicate_selection/watch.log" >&2
  exit 1
fi
kill -TERM "$duplicate_selection_pid"
wait "$duplicate_selection_pid" 2>/dev/null || true

# Dependency traversal must continue through installed packages because a
# later edge can resolve back into the local workspace and therefore needs a
# source watch.
"$port" watch "$transitive_local_watch" \
  >"$transitive_local_watch/watch.log" 2>&1 &
transitive_local_watch_pid=$!
background_pids="$background_pids $transitive_local_watch_pid"
transitive_local_output="$transitive_local_watch/packages/local/src/Local.mjs"
if ! wait_for_text "$transitive_local_output" 'value = 1'; then
  cat "$transitive_local_watch/watch.log" >&2
  exit 1
fi
printf 'let value = 2\n' \
  >"$transitive_local_watch/packages/local/src/Local.res"
if ! wait_for_text "$transitive_local_output" 'value = 2'; then
  cat "$transitive_local_watch/watch.log" >&2
  exit 1
fi
kill -TERM "$transitive_local_watch_pid"
wait "$transitive_local_watch_pid" 2>/dev/null || true

dev_include_call_log="$dev_include_order/bsc-calls.log"
env RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/counting-bsc.sh" \
  REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
  REWATCH_BSC_CALL_LOG="$dev_include_call_log" \
  "$port" build "$dev_include_order" >/dev/null
node - "$dev_include_call_log" \
  "$dev_include_order/node_modules/development/lib/ocaml" \
  "$dev_include_order/node_modules/regular/lib/ocaml" <<'EOF'
const fs = require("fs");
const [log, development, regular] = process.argv.slice(2);
const command = fs.readFileSync(log, "utf8").split("\n")
  .find(line => line.includes("Test.ast") && !line.includes("-bs-ast"));
if (!command || command.indexOf(development) < 0 ||
    command.indexOf(development) > command.indexOf(regular)) {
  process.exit(1);
}
EOF

warning_call_log="$warning_replay/bsc-calls.log"
warning_watch_log="$warning_replay/watch.log"
env RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/counting-bsc.sh" \
  REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
  REWATCH_BSC_CALL_LOG="$warning_call_log" \
  "$port" watch "$warning_replay" >"$warning_watch_log" 2>&1 &
warning_watch_pid=$!
background_pids="$background_pids $warning_watch_pid"
if ! wait_for_count "$warning_watch_log" 'unused value unusedValue' 1; then
  cat "$warning_watch_log" >&2
  exit 1
fi
warning_a_calls=$(grep -c 'WarningA.ast' "$warning_call_log" || true)
test "$warning_a_calls" -gt 0
printf '\nlet changed = 1\n' >> "$warning_replay/src/B.res"
if ! wait_for_count "$warning_watch_log" 'unused value unusedValue' 2; then
  cat "$warning_watch_log" >&2
  exit 1
fi
warning_a_calls_after=$(grep -c 'WarningA.ast' "$warning_call_log" || true)
test "$warning_a_calls_after" -eq "$warning_a_calls"
kill -TERM "$warning_watch_pid"
wait "$warning_watch_pid"
kill -TERM "$watch_pid"
wait "$watch_pid"
test ! -f "$watch_basic/lib/watch.lock"

# Watch startup shares normal build initialization, so deleting a public output
# between sessions must dirty its module even when compiler artifacts are current.
rm "$watch_basic/src/A.js"
"$port" watch "$watch_basic" >"$watch_basic/restart.log" 2>&1 &
watch_restart_pid=$!
background_pids="$background_pids $watch_restart_pid"
if ! wait_for_file "$watch_basic/src/A.js"; then
  cat "$watch_basic/restart.log" >&2
  exit 1
fi
kill -TERM "$watch_restart_pid"
wait "$watch_restart_pid"
test ! -f "$watch_basic/lib/watch.lock"

interrupt_basic="$work/interrupt-basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$interrupt_basic"
cp "$root/rewatch-ocaml/tests/slow-bsc.sh" "$interrupt_basic/slow-bsc.sh"
chmod +x "$interrupt_basic/slow-bsc.sh"
child_marker="$interrupt_basic/child-started"
REWATCH_OCAML_CHILD_STARTED="$child_marker" \
REWATCH_OCAML_REAL_BSC="$RESCRIPT_BSC_EXE" \
RESCRIPT_BSC_EXE="$interrupt_basic/slow-bsc.sh" \
"$port" watch "$interrupt_basic" >"$interrupt_basic/watch.log" 2>&1 &
interrupt_pid=$!
background_pids="$background_pids $interrupt_pid"
attempts=0
while [ "$attempts" -lt 100 ] && [ ! -f "$child_marker" ]; do
  attempts=$((attempts + 1))
  sleep 0.1
done
test -f "$child_marker"
kill -TERM "$interrupt_pid"
wait "$interrupt_pid"
test ! -f "$interrupt_basic/lib/watch.lock"
test -z "$(pgrep -f "$interrupt_basic/slow-bsc.sh" || true)"
test -z "$(find "$interrupt_basic" -name '.rewatch-ocaml-*.log' -print)"

# One-shot commands must unwind through the same process and lock owners when
# the shell terminates them during compiler work.
interrupt_build="$work/interrupt-build"
cp -R "$root/rewatch-ocaml/tests/basic" "$interrupt_build"
cp "$root/rewatch-ocaml/tests/slow-bsc.sh" \
  "$interrupt_build/slow-bsc.sh"
chmod +x "$interrupt_build/slow-bsc.sh"
build_child_marker="$interrupt_build/child-started"
REWATCH_OCAML_CHILD_STARTED="$build_child_marker" \
REWATCH_OCAML_REAL_BSC="$RESCRIPT_BSC_EXE" \
RESCRIPT_BSC_EXE="$interrupt_build/slow-bsc.sh" \
"$port" build "$interrupt_build" >"$interrupt_build/build.log" 2>&1 &
interrupt_build_pid=$!
background_pids="$background_pids $interrupt_build_pid"
wait_for_file "$build_child_marker"
kill -TERM "$interrupt_build_pid"
set +e
wait "$interrupt_build_pid"
interrupt_build_status=$?
set -e
test "$interrupt_build_status" -eq 143
test ! -f "$interrupt_build/lib/build.lock"
test -z "$(pgrep -f "$interrupt_build/slow-bsc.sh" || true)"
test -z "$(find "$interrupt_build" -name '.rewatch-ocaml-*.log' -print)"

# Removing watch.lock is the shell-suite shutdown protocol. It must interrupt
# an in-progress compiler batch just as SIGTERM does, rather than waiting for
# every queued module to finish.
lock_interrupt_basic="$work/lock-interrupt-basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$lock_interrupt_basic"
cp "$root/rewatch-ocaml/tests/slow-bsc.sh" \
  "$lock_interrupt_basic/slow-bsc.sh"
chmod +x "$lock_interrupt_basic/slow-bsc.sh"
lock_child_marker="$lock_interrupt_basic/child-started"
REWATCH_OCAML_CHILD_STARTED="$lock_child_marker" \
REWATCH_OCAML_REAL_BSC="$RESCRIPT_BSC_EXE" \
RESCRIPT_BSC_EXE="$lock_interrupt_basic/slow-bsc.sh" \
"$port" watch "$lock_interrupt_basic" \
  >"$lock_interrupt_basic/watch.log" 2>&1 &
lock_interrupt_pid=$!
background_pids="$background_pids $lock_interrupt_pid"
if ! wait_for_file "$lock_child_marker"; then
  cat "$lock_interrupt_basic/watch.log" >&2
  exit 1
fi
rm -f "$lock_interrupt_basic/lib/watch.lock"
if ! wait_for_pid_gone "$lock_interrupt_pid"; then
  echo "watcher did not stop during compiler work after watch.lock removal" >&2
  exit 1
fi
wait "$lock_interrupt_pid"
test -z "$(pgrep -f "$lock_interrupt_basic/slow-bsc.sh" || true)"

lock_basic="$work/lock-basic"
cp -R "$root/rewatch-ocaml/tests/basic" "$lock_basic"
cp "$root/rewatch-ocaml/tests/slow-bsc.sh" "$lock_basic/slow-bsc.sh"
chmod +x "$lock_basic/slow-bsc.sh"
rm -rf "$lock_basic/lib"
rm -f "$lock_basic/src/A.mjs" "$lock_basic/src/B.mjs" \
  "$lock_basic/src/WithInterface.mjs"
first_marker="$lock_basic/first-child-started"
release_marker="$lock_basic/release-first-build"
REWATCH_OCAML_CHILD_STARTED="$first_marker" \
REWATCH_OCAML_RELEASE_FILE="$release_marker" \
REWATCH_OCAML_REAL_BSC="$RESCRIPT_BSC_EXE" \
RESCRIPT_BSC_EXE="$lock_basic/slow-bsc.sh" \
  "$port" build "$lock_basic" >"$lock_basic/first.log" 2>&1 &
first_build_pid=$!
background_pids="$background_pids $first_build_pid"
wait_for_file "$first_marker"
workspace_build_lock="$lock_basic/lib/build.lock"
test -f "$workspace_build_lock"
"$port" build "$lock_basic" >"$lock_basic/second.log" 2>&1 &
second_build_pid=$!
background_pids="$background_pids $second_build_pid"
wait_for_text "$lock_basic/second.log" "Waiting for other build to finish"
test ! -f "$lock_basic/src/A.mjs"
touch "$release_marker"
wait "$first_build_pid"
wait "$second_build_pid"
test -f "$lock_basic/src/A.mjs"
test ! -f "$workspace_build_lock"

"$port" build --features native "$features"
test -f "$features/native/Native.js"

"$port" build "$feature_dependencies"
test -f "$feature_dependencies/packages/dep-union/extra/UnionExtra.js"
test -f "$feature_dependencies/packages/dep-transitive/native/TransitiveNative.js"
test -f "$feature_dependencies/packages/dep-empty/src/EmptyCommon.js"
test ! -f "$feature_dependencies/packages/dep-empty/optional/EmptyOptional.js"
"$port" watch "$feature_dependencies" \
  >"$feature_dependencies/watch.log" 2>&1 &
feature_dependencies_pid=$!
background_pids="$background_pids $feature_dependencies_pid"
if ! wait_for_file "$feature_dependencies/lib/watch.lock"; then
  cat "$feature_dependencies/watch.log" >&2
  exit 1
fi
printf 'let value = 2\n' \
  >"$feature_dependencies/packages/dep-union/extra/UnionExtra.res"
if ! wait_for_text \
  "$feature_dependencies/packages/dep-union/extra/UnionExtra.js" 'value = 2'; then
  cat "$feature_dependencies/watch.log" >&2
  exit 1
fi
kill -TERM "$feature_dependencies_pid"
wait "$feature_dependencies_pid" 2>/dev/null || true

if "$port" build "$unlinked_dependency" \
  >"$unlinked_dependency/build.log" 2>&1; then
  echo "build resolved an unlinked packages dependency" >&2
  exit 1
fi
grep -q "Could not resolve dependency dep" "$unlinked_dependency/build.log"
if "$port" clean "$unlinked_dependency" \
  >"$unlinked_dependency/clean.log" 2>&1; then
  echo "clean resolved an unlinked packages dependency" >&2
  exit 1
fi
grep -q "Could not resolve dependency dep" "$unlinked_dependency/clean.log"
test -f "$unlinked_dependency/packages/dep/src/Dep.js"
test -f "$unlinked_dependency/packages/dep/lib/ocaml/marker"

"$port" clean "$feature_dependencies"
"$port" build --prod "$feature_dependencies"
test -f "$feature_dependencies/packages/dep-union/native/UnionNative.js"
test -f "$feature_dependencies/packages/dep-union/web/UnionWeb.js"
test ! -f "$feature_dependencies/packages/dep-union/extra/UnionExtra.js"

"$port" build "$gentype"
test -f "$gentype/src/Main.js"

"$port" build "$dependency"
test -f "$dependency/src/Main.js"
test -f "$dependency/node_modules/dep/src/Dep.js"
rm "$dependency/node_modules/dep/src/Dep.js"
"$port" build "$dependency"
test -f "$dependency/node_modules/dep/src/Dep.js"
"$port" clean "$dependency"
test ! -f "$dependency/src/Main.js"
test ! -f "$dependency/node_modules/dep/src/Dep.js"

"$port" build "$package_output_dependency"
grep 'export {' "$package_output_dependency/node_modules/dep/src/Dep.js" >/dev/null
sed 's/"esmodule"/"commonjs"/' \
  "$package_output_dependency/rescript.json" \
  > "$package_output_dependency/rescript.next"
mv "$package_output_dependency/rescript.next" \
  "$package_output_dependency/rescript.json"
"$port" build "$package_output_dependency"
grep 'exports.value' \
  "$package_output_dependency/node_modules/dep/src/Dep.js" >/dev/null
sed -e 's/"commonjs"/"esmodule"/' \
  -e 's/"in-source": true/"in-source": false/' \
  -e 's/"suffix": "\.js"/"suffix": "\.mjs"/' \
  "$package_output_dependency/rescript.json" \
  > "$package_output_dependency/rescript.next"
mv "$package_output_dependency/rescript.next" \
  "$package_output_dependency/rescript.json"
"$port" build "$package_output_dependency"
if [ ! -f "$package_output_dependency/node_modules/dep/lib/es6/src/Dep.mjs" ]; then
  echo "dependency was not rebuilt in its new output location" >&2
  find "$package_output_dependency/node_modules/dep" -type f -print >&2
  exit 1
fi
if [ -f "$package_output_dependency/node_modules/dep/src/Dep.js" ]; then
  echo "dependency output from the previous package spec was retained" >&2
  exit 1
fi

"$port" build "$standalone_output/node_modules/dep"
test -f "$standalone_output/node_modules/dep/src/Dep.js"
"$port" build "$standalone_output"
test -f "$standalone_output/lib/es6/src/Main.mjs"
test -f "$standalone_output/node_modules/dep/src/Dep.js"
test ! -f "$standalone_output/node_modules/dep/lib/es6/src/Dep.mjs"
node - "$standalone_output/node_modules/dep" <<'EOF'
const fs = require("fs")
const path = require("path")
const dependency = path.resolve(process.argv[2])
const info = JSON.parse(
  fs.readFileSync(path.join(dependency, "lib", "bs", "compiler-info.json"), "utf8")
)
if (path.resolve(info.build_root) !== dependency) {
  throw new Error(`standalone dependency ownership changed to ${info.build_root}`)
}
EOF
"$port" clean "$standalone_output"
test ! -d "$standalone_output/lib/bs"
test -f "$standalone_output/node_modules/dep/lib/bs/compiler-info.json"
test -f "$standalone_output/node_modules/dep/src/Dep.js"

mkdir -p "$external_boundary/project/node_modules"
ln -s ../packages/main "$external_boundary/project/node_modules/main"
ln -s ../../external "$external_boundary/project/node_modules/external"
"$port" build --warn-error A "$external_boundary/project" \
  >"$external_boundary/build.log" 2>&1
grep "Please report this to the package maintainer: https://example.com/external/issues" \
  "$external_boundary/build.log" >/dev/null
test -f "$external_boundary/external/src/Sentinel.js"
test -f "$external_boundary/external/src/Foo.mjs"
test -f "$external_boundary/external/src/Foo.mjs.map"
rm "$external_boundary/external/src/Foo.res"
rm "$external_boundary/external/src/Foo.resi"
"$port" build "$external_boundary/project"
test ! -f "$external_boundary/external/src/Foo.mjs"
test ! -f "$external_boundary/external/src/Foo.mjs.map"
test -f "$external_boundary/external/src/Foo.js"
"$port" clean "$external_boundary/project"
test -f "$external_boundary/external/src/Sentinel.js"
test -f "$external_boundary/external/src/Foo.js"

"$port" build "$post_build"
test -f "$post_build/src/Main.js"

"$port" build "$ppx_filter"
test -f "$ppx_filter/src/Main.js"

"$port" build "$out_of_source"
test -f "$out_of_source/lib/es6/src/Main.js"
rm -f "$out_of_source/src/Main.res"
"$port" build "$out_of_source"
test ! -f "$out_of_source/lib/es6/src/Main.js"

namespace_call_log="$namespace/bsc-calls.log"
env RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/counting-bsc.sh" \
  REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
  REWATCH_BSC_CALL_LOG="$namespace_call_log" \
"$port" build "$namespace"
test -f "$namespace/lib/ocaml/A-Widget.cmi"
test -f "$namespace/src/B.js"
: > "$namespace_call_log"
env RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/counting-bsc.sh" \
  REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
  REWATCH_BSC_CALL_LOG="$namespace_call_log" \
  "$port" build "$namespace"
if grep -F 'Widget.mlmap' "$namespace_call_log" >/dev/null; then
  echo "unchanged build unexpectedly recompiled its namespace" >&2
  exit 1
fi
printf '\nlet changed = 1\n' >> "$namespace/src/B.res"
env RESCRIPT_BSC_EXE="$root/rewatch-ocaml/tests/counting-bsc.sh" \
  REWATCH_REAL_BSC="$RESCRIPT_BSC_EXE" \
  REWATCH_BSC_CALL_LOG="$namespace_call_log" \
  "$port" build "$namespace"
grep -F 'Widget.mlmap' "$namespace_call_log" >/dev/null

"$port" build "$namespace_invalidation"
rm "$namespace_invalidation/packages/dep/src/A.res"
if "$port" build "$namespace_invalidation" \
  >"$namespace_invalidation/removal.log" 2>&1; then
  echo "namespace consumer was not invalidated after member removal" >&2
  exit 1
fi
grep -F 'Ns.A' "$namespace_invalidation/removal.log" >/dev/null

"$port" build "$namespace_restart"
printf 'let value = "changed"\n' \
  >"$namespace_restart/packages/dep/src/A.res"
if "$port" build "$namespace_restart" \
  >"$namespace_restart/first-failure.log" 2>&1; then
  echo "namespace consumer unexpectedly accepted a changed member CMI" >&2
  exit 1
fi
if "$port" build "$namespace_restart" \
  >"$namespace_restart/restarted-failure.log" 2>&1; then
  echo "restarted build forgot a changed namespace member CMI" >&2
  exit 1
fi
if ! grep -F 'This has type:' \
  "$namespace_restart/restarted-failure.log" >/dev/null || \
  ! grep -F 'string' "$namespace_restart/restarted-failure.log" >/dev/null; then
  cat "$namespace_restart/restarted-failure.log" >&2
  exit 1
fi

"$port" build "$namespace_entry"
test -f "$namespace_entry/src/Entry.mjs"
test -f "$namespace_entry/lib/ocaml/Entry.cmi"
test -f "$namespace_entry/lib/ocaml/Entry_alias-@EntryNamespace.cmi"

"$port" build "$qualified_namespace"
test -f "$qualified_namespace/src/Extent.js"
test -f "$qualified_namespace/src/Geometry.js"

if "$port" build "$namespace_collision" \
  >"$namespace_collision/output.log" 2>&1; then
  echo "namespace collision build unexpectedly succeeded" >&2
  exit 1
fi
grep 'Namespace SharedNamespace is provided by both' \
  "$namespace_collision/output.log" >/dev/null
grep 'namespace-one' "$namespace_collision/output.log" >/dev/null
grep 'namespace-two' "$namespace_collision/output.log" >/dev/null

"$port" build "$source_map"
test -f "$source_map/src/Main.js.map"
test -f "$source_map/lib/bs/compiler-info.json"
"$port" build "$source_map" >"$source_map/unchanged.log"
grep 'Compiled 0 modules' "$source_map/unchanged.log" >/dev/null
sed 's/"mode": "linked"/"mode": "hidden"/' "$source_map/rescript.json" \
  > "$source_map/rescript.next"
mv "$source_map/rescript.next" "$source_map/rescript.json"
"$port" build "$source_map" >"$source_map/changed.log"
grep 'Cleaned previous build due to compiler update' \
  "$source_map/changed.log" >/dev/null
grep 'Compiled 1 modules' "$source_map/changed.log" >/dev/null

sed 's/"sources":"src"/"sources":"src","dependencies":["consumer"]/' \
  "$monorepo/packages/dep/rescript.json" > "$monorepo/packages/dep/rescript.next"
mv "$monorepo/packages/dep/rescript.next" "$monorepo/packages/dep/rescript.json"
"$port" build "$monorepo"
test -f "$monorepo/src/Root.js"
test -f "$monorepo/packages/consumer/src/Consumer.js"
test -f "$monorepo/packages/dep/src/Dep.js"
"$port" clean "$monorepo"
test ! -d "$monorepo/lib/ocaml"
# This package was built directly above, so the later workspace build must not
# transfer ownership of its outputs to the parent invocation.
test -d "$monorepo/packages/consumer/lib/ocaml"
test -d "$monorepo/packages/dep/lib/ocaml"
"$port" clean "$monorepo/packages/consumer"
test ! -d "$monorepo/packages/consumer/lib/ocaml"
test ! -d "$monorepo/packages/dep/lib/ocaml"
rm -f "$features/native/Native.js"
"$port" build --features all "$features"
test -f "$features/native/Native.js"

if "$port" build "$cycle" >"$cycle/output.log" 2>&1; then
  echo "cycle build unexpectedly succeeded" >&2
  exit 1
fi
grep "circular dependency" "$cycle/output.log" >/dev/null

if "$port" build "$multiple_cycles" \
  >"$multiple_cycles/output.log" 2>&1; then
  echo "multiple-cycle build unexpectedly succeeded" >&2
  exit 1
fi
grep "circular dependency" "$multiple_cycles/output.log" >/dev/null
if grep "subprocess dependency graph contains a cycle" \
  "$multiple_cycles/output.log" >/dev/null; then
  echo "multiple cycles escaped build preparation" >&2
  exit 1
fi
test -f "$multiple_cycles/src/Valid.mjs"
test ! -f "$multiple_cycles/src/A.mjs"
test ! -f "$multiple_cycles/src/B.mjs"
test ! -f "$multiple_cycles/src/C.mjs"
test ! -f "$multiple_cycles/src/D.mjs"

if "$port" build "$failure" >"$failure/output.log" 2>&1; then
  echo "invalid build unexpectedly succeeded" >&2
  exit 1
fi
test -f "$failure/lib/bs/build.ninja"
grep "expected to have type" "$failure/output.log" >/dev/null
test ! -f "$root/lib/build.lock"

cp "$failure/Broken.fixed" "$failure/src/Broken.res"
"$port" build "$failure"
test -f "$failure/src/Broken.js"

rm -rf "$basic/lib" "$cycle/lib" "$failure/lib"
rm -rf "$legacy_config/lib"
rm -rf "$features/lib"
rm -rf "$gentype/lib"
rm -rf "$gentype/node_modules/dep/lib"
rm -rf "$dependency/lib" "$dependency/node_modules/dep/lib"
rm -rf "$post_build/lib"
rm -rf "$out_of_source/lib"
rm -rf "$namespace/lib"
rm -rf "$namespace_entry/lib"
rm -rf "$qualified_namespace/lib"
rm -rf "$namespace_collision/lib"
rm -rf "$source_map/lib"
rm -f "$source_map/unchanged.log" "$source_map/changed.log"
rm -f "$basic/src/A.mjs" "$basic/src/B.mjs" "$basic/src/WithInterface.mjs"
rm -f "$legacy_config/src/A.mjs" "$legacy_config/src/B.mjs" "$legacy_config/src/WithInterface.mjs"
rm -f "$cycle/output.log" "$failure/output.log"
rm -f "$namespace_collision/output.log"
