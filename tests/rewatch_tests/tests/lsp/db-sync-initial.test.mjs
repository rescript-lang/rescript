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
    const db = new DatabaseSync(dbPath, { readOnly: true });
    db.exec("PRAGMA busy_timeout = 3000");
    return db;
  } catch {
    return null;
  }
}

/**
 * Poll until `predicate(db)` returns true.
 * Re-opens the DB on each attempt so we see writes from the db_sync queue.
 */
async function waitForDb(sandbox, predicate, timeoutMs = 10000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const db = openDb(sandbox);
    if (db) {
      try {
        if (predicate(db)) return db;
      } catch (err) {
        console.warn(`waitForDb: transient error, retrying: ${err.message}`);
      } finally {
        db.close();
      }
    }
    await new Promise(r => setTimeout(r, 50));
  }
  throw new Error(`waitForDb: timed out after ${timeoutMs}ms`);
}

describe("lsp initial db sync", { timeout: 120_000 }, () => {
  it("creates rescript.db after initial build without rescript sync", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri, { db_sync: true });
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // The db_sync queue should create rescript.db after the initial build.
      // Poll until the DB exists, has the schema, and has at least one module.
      await waitForDb(sandbox, db => {
        try {
          const count = db.prepare("SELECT COUNT(*) as cnt FROM modules").get();
          return count.cnt > 0;
        } catch {
          // Schema not yet applied (race between file creation and DDL)
          return false;
        }
      });

      // Verify the DB has packages and modules
      const db = openDb(sandbox);
      expect(db).not.toBeNull();

      const packages = db
        .prepare("SELECT name FROM packages ORDER BY name")
        .all();
      const packageNames = packages.map(r => r.name);
      expect(packageNames).toContain("@rescript/runtime");

      const modules = db
        .prepare(
          "SELECT m.name FROM modules m WHERE m.parent_module_id IS NULL ORDER BY m.name",
        )
        .all();
      const moduleNames = modules.map(r => r.name);
      expect(moduleNames.length).toBeGreaterThan(0);

      // Runtime modules (Stdlib, Pervasives, etc.) should be indexed
      expect(moduleNames).toContain("Stdlib");
      expect(moduleNames).toContain("Pervasives");

      // Modules with a .resi file should have has_interface = 1
      const appModule = db
        .prepare(
          "SELECT m.name, m.has_interface FROM modules m WHERE m.name = 'App' AND m.parent_module_id IS NULL",
        )
        .get();
      expect(appModule).toBeDefined();
      expect(appModule.has_interface).toBe(1);

      // Modules without a .resi file should have has_interface = 0
      const rootModule = db
        .prepare(
          "SELECT m.name, m.has_interface FROM modules m WHERE m.name = 'Root' AND m.parent_module_id IS NULL",
        )
        .get();
      expect(rootModule).toBeDefined();
      expect(rootModule.has_interface).toBe(0);

      // The usages table should exist and have entries
      const usageCount = db.prepare("SELECT COUNT(*) as cnt FROM usages").get();
      expect(usageCount.cnt).toBeGreaterThan(0);

      db.close();
    }));
});
