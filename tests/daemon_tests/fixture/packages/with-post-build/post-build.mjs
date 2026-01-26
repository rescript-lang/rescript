// Simple post-build script that writes to stdout
// The JS file path is passed as the first argument
const jsFile = process.argv[2];
console.log(`post-build-output: ${jsFile}`);
