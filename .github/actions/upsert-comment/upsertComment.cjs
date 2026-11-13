// @ts-check

// Keeps one comment of the workflow's on a pull request, found by a hidden
// marker with its tag: edits it in place when its body changed, or creates
// it. Edits notify no one, so a workflow can update its comment on every
// push. Run by the upsert-comment action through actions/github-script.

/**
 * The marker that ends a comment with this tag.
 *
 * @param {string} tag
 */
const markerOf = tag =>
  `<!-- rescript-lang/rescript upsert-comment "${tag}" -->`;

/**
 * The comment's whole body: the text, then the marker on a line of its own.
 *
 * @param {string} text
 * @param {string} tag
 */
const bodyOf = (text, tag) => `${text.trimEnd()}\n\n${markerOf(tag)}\n`;

/**
 * @typedef {{ id: number, body?: string, user: { login: string } | null }} Comment
 * @typedef {{
 *   listComments: (params: { owner: string, repo: string, issue_number: number }) => Promise<Comment[]>,
 *   createComment: (params: { owner: string, repo: string, issue_number: number, body: string }) => Promise<unknown>,
 *   updateComment: (params: { owner: string, repo: string, comment_id: number, body: string }) => Promise<unknown>,
 * }} Client
 */

/**
 * Creates or updates the pull request's comment with this tag. Only comments
 * by `author` count, so that a person quoting the marker doesn't get their
 * comment overwritten. Returns what it did.
 *
 * @param {Client} client
 * @param {{
 *   owner: string,
 *   repo: string,
 *   issueNumber: number,
 *   tag: string,
 *   text: string,
 *   author: string,
 * }} options
 * @returns {Promise<"created" | "updated" | "unchanged">}
 */
async function upsertComment(
  client,
  { owner, repo, issueNumber, tag, text, author },
) {
  if (!/^[\w.-]+$/.test(tag)) {
    throw new Error(`The comment tag ${JSON.stringify(tag)} isn't a word.`);
  }
  if (!Number.isInteger(issueNumber) || issueNumber <= 0) {
    throw new Error(`${issueNumber} isn't a pull request number.`);
  }
  const marker = markerOf(tag);
  const body = bodyOf(text, tag);
  const comments = await client.listComments({
    owner,
    repo,
    issue_number: issueNumber,
  });
  const existing = comments.find(
    comment =>
      comment.user?.login === author &&
      (comment.body ?? "").trimEnd().endsWith(marker),
  );
  if (!existing) {
    await client.createComment({
      owner,
      repo,
      issue_number: issueNumber,
      body,
    });
    return "created";
  }
  if (existing.body === body) return "unchanged";
  await client.updateComment({ owner, repo, comment_id: existing.id, body });
  return "updated";
}

module.exports = { upsertComment, markerOf, bodyOf };
