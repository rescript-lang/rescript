import fs from "node:fs";

const releaseFile = process.argv[2];
if (releaseFile === undefined) {
  throw new Error("Expected the release-file path as the first argument");
}

console.log("REWATCH_AFTER_BUILD_READY");
const timer = setInterval(() => {
  if (fs.existsSync(releaseFile)) {
    clearInterval(timer);
  }
}, 50);
