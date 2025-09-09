Minimum example for an error with Deno and ReScript v12 beta.

```
Task build rescript
[1/7] 📦 Built package tree in 0.00s
[2/7] 👀 Found source files in 0.00s
[3/7] 📝 Read compile state 0.00s
[4/7] 🧹 Cleaned 0/0 0.00s
[5/7] 🧱 Parsed 1 source files in 0.18s
[6/7] 🌴 Collected deps in 0.00s
[7/7] ❌ Compiled 1 modules in 0.01s

  We've found a bug for you!
  command line

  The module or file Pervasives can't be found.
  - If it's a third-party dependency:
    - Did you add it to the "dependencies" or "dev-dependencies" in rescript.json?
  - Did you include the file's directory to the "sources" in rescript.json?
  


  ❌ Failed to Compile. See Errors Above
```

To reproduce:
```
deno i
deno task build
```
