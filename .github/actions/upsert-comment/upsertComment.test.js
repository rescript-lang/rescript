import assert from "node:assert/strict";
import test from "node:test";
import { bodyOf, markerOf, upsertComment } from "./upsertComment.cjs";

const bot = "github-actions[bot]";

/**
 * A fake of the comments API that records the calls that change comments.
 *
 * @param {{ id: number, body?: string, user: { login: string } | null }[]} comments
 */
function fakeClient(comments) {
  /** @type {unknown[]} */
  const calls = [];
  return {
    calls,
    listComments: async () => comments,
    createComment: async (/** @type {any} */ params) => {
      calls.push({ create: params });
    },
    updateComment: async (/** @type {any} */ params) => {
      calls.push({ update: params });
    },
  };
}

const options = {
  owner: "rescript-lang",
  repo: "rescript",
  issueNumber: 42,
  tag: "compiler-benchmarks",
  text: "## Report\n\nAll good.\n\n",
  author: bot,
};

test("creates the comment when there is none", async () => {
  const client = fakeClient([
    { id: 1, body: "Looks good to me.", user: { login: "someone" } },
  ]);
  assert.equal(await upsertComment(client, options), "created");
  assert.deepEqual(client.calls, [
    {
      create: {
        owner: "rescript-lang",
        repo: "rescript",
        issue_number: 42,
        body: `## Report\n\nAll good.\n\n${markerOf("compiler-benchmarks")}\n`,
      },
    },
  ]);
});

test("updates the workflow's comment with the same tag", async () => {
  const client = fakeClient([
    { id: 1, body: bodyOf("Old", "semantic-diff"), user: { login: bot } },
    { id: 2, body: bodyOf("Old", "compiler-benchmarks"), user: { login: bot } },
  ]);
  assert.equal(await upsertComment(client, options), "updated");
  assert.deepEqual(client.calls, [
    {
      update: {
        owner: "rescript-lang",
        repo: "rescript",
        comment_id: 2,
        body: bodyOf(options.text, "compiler-benchmarks"),
      },
    },
  ]);
});

test("leaves an unchanged comment alone", async () => {
  const client = fakeClient([
    {
      id: 2,
      body: bodyOf(options.text, "compiler-benchmarks"),
      user: { login: bot },
    },
  ]);
  assert.equal(await upsertComment(client, options), "unchanged");
  assert.deepEqual(client.calls, []);
});

test("ignores a person's comment that quotes the marker", async () => {
  const client = fakeClient([
    { id: 3, body: bodyOf("Quoted", "compiler-benchmarks"), user: null },
    {
      id: 4,
      body: bodyOf("Quoted", "compiler-benchmarks"),
      user: { login: "someone" },
    },
  ]);
  assert.equal(await upsertComment(client, options), "created");
});

test("ignores a tag that only starts like this one", async () => {
  const client = fakeClient([
    {
      id: 5,
      body: bodyOf("Old", "compiler-benchmarks-old"),
      user: { login: bot },
    },
  ]);
  assert.equal(await upsertComment(client, options), "created");
});

test("rejects tags that aren't words and missing pull request numbers", async () => {
  const client = fakeClient([]);
  await assert.rejects(
    upsertComment(client, { ...options, tag: 'a" -->' }),
    /isn't a word/,
  );
  await assert.rejects(
    upsertComment(client, { ...options, issueNumber: Number("") }),
    /isn't a pull request number/,
  );
  assert.deepEqual(client.calls, []);
});
