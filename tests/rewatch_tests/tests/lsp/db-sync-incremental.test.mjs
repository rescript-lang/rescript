import path from "node:path";
import { DatabaseSync } from "node:sqlite";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

/**
 * Open the rescript.db for the root project (read-only).
 * Returns null if the DB does not exist yet.
 */
function openDb(sandbox) {
  const dbPath = path.join(sandbox, "rescript.db");
  try {
    return new DatabaseSync(dbPath, { readOnly: true });
  } catch {
    return null;
  }
}

/**
 * Query all value names for a given module from the DB.
 */
function getValueNames(db, moduleName) {
  const rows = db
    .prepare(
      `SELECT v.name FROM [values] v
       JOIN modules m ON v.module_id = m.id
       WHERE m.name = ?
       ORDER BY v.name`,
    )
    .all(moduleName);
  return rows.map(r => r.name);
}

/**
 * Query all type names for a given module from the DB.
 */
function getTypeNames(db, moduleName) {
  const rows = db
    .prepare(
      `SELECT t.name FROM types t
       JOIN modules m ON t.module_id = m.id
       WHERE m.name = ?
       ORDER BY t.name`,
    )
    .all(moduleName);
  return rows.map(r => r.name);
}

/**
 * Poll the DB until `predicate(db)` returns true.
 * Re-opens the DB on each attempt so we see writes from the db_sync queue.
 */
async function waitForDb(sandbox, predicate, timeoutMs = 10000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const db = openDb(sandbox);
    if (db) {
      try {
        if (predicate(db)) return db;
      } finally {
        db.close();
      }
    }
    await new Promise(r => setTimeout(r, 50));
  }
  throw new Error(`waitForDb: timed out after ${timeoutMs}ms`);
}

describe("lsp incremental db sync", { timeout: 120_000 }, () => {
  it("updates module values in rescript.db after a file is saved", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Wait for the LSP's background db_sync to populate the initial DB.
      // Use a longer timeout because db_sync shells out to the analysis binary
      // for all modules, which can be slow on CI (especially Windows).
      await waitForDb(
        sandbox,
        db => getValueNames(db, "Library").includes("greet"),
        30000,
      );

      // Verify initial state: Library has greeting, admin, greet
      let db = openDb(sandbox);
      expect(db).not.toBeNull();
      let values = getValueNames(db, "Library");
      expect(values).toContain("greeting");
      expect(values).toContain("admin");
      expect(values).toContain("greet");
      db.close();

      // Modify Library.res: remove `greet`, add `farewell`
      await writeFile(
        "packages/library/src/Library.res",
        `let greeting = "hello from library"
type user = {name: string}
let admin: user = {name: "admin"}
let farewell = (name: string) => "bye " ++ name
`,
      );

      // Wait for the LSP build to finish
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Poll until db_sync has updated the DB with the new value
      await waitForDb(sandbox, db =>
        getValueNames(db, "Library").includes("farewell"),
      );

      // Re-open the DB and verify the values were updated
      db = openDb(sandbox);
      expect(db).not.toBeNull();
      values = getValueNames(db, "Library");
      expect(values).toContain("greeting");
      expect(values).toContain("admin");
      expect(values).toContain("farewell");
      expect(values).not.toContain("greet");
      db.close();
    }));

  it("updates module types in rescript.db after a file is saved", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Wait for the LSP's background db_sync to populate the initial DB.
      await waitForDb(
        sandbox,
        db => getTypeNames(db, "Library").includes("user"),
        30000,
      );

      // Verify initial state: Library has the `user` record type
      let db = openDb(sandbox);
      let types = getTypeNames(db, "Library");
      expect(types).toContain("user");
      expect(types).not.toContain("role");
      db.close();

      // Add a new variant type `role`
      await writeFile(
        "packages/library/src/Library.res",
        `let greeting = "hello from library"
type user = {name: string}
type role = Admin | Editor | Viewer
let admin: user = {name: "admin"}
let greet = (name: string) => "hello " ++ name
`,
      );

      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Poll until db_sync has updated the DB with the new type
      await waitForDb(sandbox, db =>
        getTypeNames(db, "Library").includes("role"),
      );

      db = openDb(sandbox);
      types = getTypeNames(db, "Library");
      expect(types).toContain("user");
      expect(types).toContain("role");
      db.close();
    }));
});
