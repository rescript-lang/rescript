// @ts-check

/* The Semantic Diff viewer: draws the commit records of a pull request, a
 * range or commits of this repository in the browser, reading them from
 * GitHub's REST API and drawing them with the page's renderer
 * (recordRender.js). recordViewer.html is the page;
 * .github/workflows/semantic_diff_viewer.yml publishes both to the CDN.
 *
 *   ?pr=8958                the pull request's commits
 *   ?pr=8958&range=a...b    the commits of a range, headed by the pull request
 *   ?range=a...b            the commits b has and a lacks
 *   ?commit=a1b2c3d,e4f5a6b commits, one card each
 *
 * Without a token, GitHub answers 60 requests an hour for each address; a
 * pull request takes two, and one more for each 100 commits past the first.
 * A pull request or range holds at most 250 commits (GitHub lists no more
 * of a pull request's), and a query names at most 20 commits, so a page
 * takes at most 4 requests or 20.
 */

import { CSS, esc, pageBody, pageTitle, parseRecord } from "./recordRender.js";

/** @import { CommitRecord, Group } from "./recordRender.js" */

/**
 * A GitHub API request: the path under the repository, answered as JSON.
 * @typedef {(path: string) => Promise<any>} FetchJson
 */

/**
 * A commit as GitHub's API returns it, the fields the viewer reads.
 * @typedef {{
 *   sha: string,
 *   parents: unknown[],
 *   author?: { login: string } | null,
 *   commit: { message: string, author: { name: string, date: string } },
 * }} ApiCommit
 */

const REPO = "rescript-lang/rescript";
const API = `https://api.github.com/repos/${REPO}`;
const MONTHS = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split(" ");
const PAGE = 100;
/** The commits a pull request or range holds at most. */
export const MAX_COMMITS = 250;
/** The commits a query names at most. */
export const MAX_NAMED = 20;

/** A commit's sha, full or abbreviated. */
const SHA = /^[0-9a-f]{4,40}$/i;
const USAGE =
  "Name what to draw: ?pr=<number>, ?range=<base>...<tip> or ?commit=<sha>[,<sha>].";

/**
 * A range `base..tip` or `base...tip` of shas, tags or branch names: words
 * joined by single dots or slashes, so a range names no other API path.
 */
const RANGE = /^(\w[\w-]*(?:[./]\w[\w-]*)*)\.{2,3}(\w[\w-]*(?:[./]\w[\w-]*)*)$/;

/**
 * A commit's record, its author by GitHub login when the API knows the
 * account, and its date as the cards print it, `Oct 23, 2026, 14:41 UTC`.
 * @param {ApiCommit} c
 * @returns {CommitRecord}
 */
export function recordOf(c) {
  const d = new Date(c.commit.author.date);
  const two = (/** @type {number} */ n) => String(n).padStart(2, "0");
  return parseRecord({
    sha: c.sha.slice(0, 9),
    author: c.commit.author.name,
    login: c.author?.login,
    date: `${MONTHS[d.getUTCMonth()]} ${two(d.getUTCDate())}, ${d.getUTCFullYear()}, ${two(d.getUTCHours())}:${two(d.getUTCMinutes())} UTC`,
    message: c.commit.message,
  });
}

/**
 * The commits a paginated listing holds, page after page until one comes
 * back short. A listing answers either an array or `{ commits }`.
 * @param {FetchJson} fetchJson
 * @param {string} path
 * @returns {Promise<ApiCommit[]>}
 */
async function listing(fetchJson, path) {
  /** @type {ApiCommit[]} */
  const all = [];
  for (let page = 1; ; page++) {
    const answer = await fetchJson(
      `${path}${path.includes("?") ? "&" : "?"}per_page=${PAGE}&page=${page}`,
    );
    /** @type {ApiCommit[]} */
    const commits = Array.isArray(answer) ? answer : answer.commits;
    tooMany(answer.total_commits);
    all.push(...commits);
    if (commits.length < PAGE) return all;
  }
}

/**
 * Refuses a pull request or range of more commits than the viewer draws.
 * @param {number | undefined} count
 */
function tooMany(count) {
  if (count !== undefined && count > MAX_COMMITS)
    throw new Error(
      `It holds ${count} commits; the viewer draws at most ${MAX_COMMITS}.`,
    );
}

/**
 * Records of the commits, merge commits left out as `git rev-list
 * --no-merges` leaves them.
 * @param {ApiCommit[]} commits
 */
const records = commits =>
  commits.filter(c => c.parents.length < 2).map(recordOf);

/**
 * The page the query asks for: its heading and its groups of records.
 * @param {URLSearchParams} query
 * @param {FetchJson} fetchJson
 * @returns {Promise<{ heading: { pr?: string, title?: string }, groups: Group[] }>}
 */
export async function load(query, fetchJson) {
  const pr = query.get("pr") ?? "";
  const range = query.get("range") ?? "";
  const commits = (query.get("commit") ?? "").split(",").filter(Boolean);
  if (pr && !/^\d+$/.test(pr)) throw new Error(`Not a pull request: ${pr}`);
  const ends = range ? RANGE.exec(range) : null;
  if (range && !ends) throw new Error(`Not a range: ${range}`);
  const bad = commits.find(c => !SHA.test(c));
  if (bad) throw new Error(`Not a commit: ${bad}`);
  if (commits.length > MAX_NAMED)
    throw new Error(
      `The query names ${commits.length} commits; the viewer draws at most ${MAX_NAMED}.`,
    );
  if (!pr && !ends && !commits.length) throw new Error(USAGE);

  /** @type {Group[]} */
  const groups = [];
  /** @type {{ pr?: string, title?: string }} */
  let heading = {};
  if (pr) {
    const pull = await fetchJson(`/pulls/${pr}`);
    heading = { pr, title: pull.title };
    if (!ends) {
      tooMany(pull.commits);
      groups.push({
        range: `${pull.base.ref}..${pull.head.ref}`,
        records: records(await listing(fetchJson, `/pulls/${pr}/commits`)),
      });
    }
  }
  if (ends) {
    const [, base, tip] = ends;
    groups.push({
      range: `${base}..${tip}`,
      records: records(await listing(fetchJson, `/compare/${base}...${tip}`)),
    });
  }
  for (const sha of commits)
    groups.push({ records: [recordOf(await fetchJson(`/commits/${sha}`))] });
  return { heading, groups };
}

/**
 * A request to GitHub's API, its failures as errors a reader can act on.
 * @type {FetchJson}
 */
async function fetchJson(path) {
  const response = await fetch(`${API}${path}`, {
    headers: { Accept: "application/vnd.github+json" },
  });
  if (response.ok) return response.json();
  if (response.headers.get("x-ratelimit-remaining") === "0") {
    const reset = new Date(
      Number(response.headers.get("x-ratelimit-reset")) * 1000,
    );
    throw new Error(
      `GitHub's API allows this address no more requests until ${reset.toLocaleTimeString()}.`,
    );
  }
  throw new Error(
    response.status === 404
      ? `GitHub found nothing at ${REPO}${path.replace(/\?.*/, "")}.`
      : `GitHub answered ${response.status} ${response.statusText}.`,
  );
}

/**
 * Draws the page the address asks for into the document, or why it can't.
 */
export async function show() {
  const style = document.createElement("style");
  style.textContent = CSS;
  document.head.append(style);
  try {
    const page = await load(new URLSearchParams(location.search), fetchJson);
    document.title = pageTitle(page.heading);
    document.body.innerHTML = pageBody(page.groups, page.heading);
    if (location.hash)
      document.getElementById(location.hash.slice(1))?.scrollIntoView();
  } catch (e) {
    document.body.innerHTML = `<main><header><h1 class="brand">Semantic <span>Diff</span></h1><p class="lede">${esc(e instanceof Error ? e.message : String(e))}</p></header></main>`;
  }
}
