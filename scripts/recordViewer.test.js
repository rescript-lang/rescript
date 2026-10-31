import assert from "node:assert/strict";
import test from "node:test";

import { pageBody } from "./recordRender.js";
import { load, MAX_COMMITS, MAX_NAMED, recordOf } from "./recordViewer.js";

const SIGNED = "Signed-off-by: A Person <a@example.com>";

/**
 * A commit as GitHub's API returns it.
 * @param {string} sha
 * @param {string} message
 * @param {number} [parents]
 */
const apiCommit = (sha, message, parents = 1) => ({
  sha: sha.padEnd(40, "0"),
  parents: Array.from({ length: parents }, () => ({})),
  commit: {
    message,
    author: { name: "A Person", date: "2026-10-02T23:30:00Z" },
  },
});

/**
 * A fake API answering the paths it is given, and the paths asked.
 * @param {Record<string, unknown>} answers
 */
function api(answers) {
  /** @type {string[]} */
  const asked = [];
  /** @param {string} path */
  const fetchJson = async path => {
    asked.push(path);
    if (!(path in answers)) throw new Error(`unexpected ${path}`);
    return answers[path];
  };
  return { asked, fetchJson };
}

const query = (/** @type {string} */ s) => new URLSearchParams(s);

test("a commit's record has its short sha, author and date as the cards print them", () => {
  const r = recordOf(
    apiCommit("abcdef1234", `Fix it\n\nIt broke.\n\n${SIGNED}`),
  );
  assert.equal(r.sha, "abcdef123");
  assert.equal(r.author, "A Person");
  assert.equal(r.login, undefined);
  assert.equal(r.date, "Oct 02, 2026, 23:30 UTC");
  assert.equal(r.subject, "Fix it");
});

test("a commit's record names its author's GitHub login when the API knows it", () => {
  const r = recordOf({
    ...apiCommit("abcdef1234", `Fix it\n\nIt broke.\n\n${SIGNED}`),
    author: { login: "a-person" },
  });
  assert.equal(r.login, "a-person");
});

test("a pull request is its title and its commits, merges left out", async () => {
  const { asked, fetchJson } = api({
    "/pulls/12": {
      title: "Keep <the> node",
      base: { ref: "master" },
      head: { ref: "feature" },
    },
    "/pulls/12/commits?per_page=100&page=1": [
      apiCommit("a1", `First\n\n${SIGNED}`),
      apiCommit("b2", "Merge master", 2),
      apiCommit("c3", `Second\n\n${SIGNED}`),
    ],
  });
  const page = await load(query("pr=12"), fetchJson);
  assert.deepEqual(page.heading, { pr: "12", title: "Keep <the> node" });
  assert.equal(page.groups.length, 1);
  assert.equal(page.groups[0].range, "master..feature");
  assert.deepEqual(
    page.groups[0].records.map(r => r.subject),
    ["First", "Second"],
  );
  assert.equal(asked.length, 2);
  const html = pageBody(page.groups, page.heading);
  assert.ok(html.includes("Keep &lt;the&gt; node"));
  assert.ok(html.includes('id="c-c30000000"'));
});

test("a listing is read page after page until one comes back short", async () => {
  const full = Array.from({ length: 100 }, (_, k) =>
    apiCommit(`${k}`.padStart(4, "a"), `Commit ${k}\n\n${SIGNED}`),
  );
  const { asked, fetchJson } = api({
    "/compare/master...f00d?per_page=100&page=1": { commits: full },
    "/compare/master...f00d?per_page=100&page=2": {
      commits: [apiCommit("beef", `Last\n\n${SIGNED}`)],
    },
  });
  const page = await load(query("range=master..f00d"), fetchJson);
  assert.ok(
    await load(
      query("range=v12.0.0...feature/x-y"),
      api({
        "/compare/v12.0.0...feature/x-y?per_page=100&page=1": { commits: [] },
      }).fetchJson,
    ),
  );
  assert.equal(page.groups[0].range, "master..f00d");
  assert.equal(page.groups[0].records.length, 101);
  assert.equal(asked.length, 2);
});

test("a range under a pull request takes the pull request's heading only", async () => {
  const { fetchJson } = api({
    "/pulls/12": { title: "T", base: { ref: "master" }, head: { ref: "x" } },
    "/compare/a1...b2?per_page=100&page=1": {
      commits: [apiCommit("b2", `Old head\n\n${SIGNED}`)],
    },
  });
  const page = await load(query("pr=12&range=a1...b2"), fetchJson);
  assert.equal(page.heading.title, "T");
  assert.deepEqual(
    page.groups.map(g => g.range),
    ["a1..b2"],
  );
});

test("commits are a card each", async () => {
  const { fetchJson } = api({
    "/commits/a1b2": apiCommit("a1b2", `One\n\n${SIGNED}`),
    "/commits/c3d4": apiCommit("c3d4", `Two\n\n${SIGNED}`),
  });
  const page = await load(query("commit=a1b2,c3d4"), fetchJson);
  assert.deepEqual(
    page.groups.map(g => [g.range, g.records[0].subject]),
    [
      [undefined, "One"],
      [undefined, "Two"],
    ],
  );
});

test("a query naming nothing or something malformed asks nothing of GitHub", async () => {
  const { asked, fetchJson } = api({});
  for (const [q, message] of [
    ["", /Name what to draw/],
    ["pr=12a", /Not a pull request: 12a/],
    ["range=a;b", /Not a range: a;b/],
    ["range=../x", /Not a range/],
    ["range=a/../../b...c", /Not a range/],
    ["range=a..b..c", /Not a range/],
    ["commit=xyz", /Not a commit: xyz/],
  ])
    await assert.rejects(load(query(`${q}`), fetchJson), message);
  assert.deepEqual(asked, []);
});

test("a pull request or range of too many commits is refused before its listing", async () => {
  const { asked, fetchJson } = api({
    "/pulls/12": {
      title: "T",
      commits: MAX_COMMITS + 1,
      base: { ref: "master" },
      head: { ref: "x" },
    },
    "/compare/v11...master?per_page=100&page=1": {
      total_commits: 4000,
      commits: [],
    },
  });
  await assert.rejects(load(query("pr=12"), fetchJson), /holds 251 commits/);
  await assert.rejects(
    load(query("range=v11...master"), fetchJson),
    /holds 4000 commits; the viewer draws at most 250/,
  );
  assert.deepEqual(asked, [
    "/pulls/12",
    "/compare/v11...master?per_page=100&page=1",
  ]);
  const named = Array.from({ length: MAX_NAMED + 1 }, (_, k) =>
    `${k}`.padStart(4, "a"),
  );
  await assert.rejects(
    load(query(`commit=${named.join(",")}`), fetchJson),
    /names 21 commits/,
  );
});
