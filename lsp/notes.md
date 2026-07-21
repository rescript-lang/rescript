# Notes

## Some strange behaviors with diagnostics when testing the server

Below I describe the scenarios. I don't know if this is the expected behavior.

1. I have three files: A, B, and C. Only A is open in the editor. I changed the
   type of a variable in A that B and C use. The compiler reported the errors in
   files B and C. All good. I edited A to introduce a syntax error but did not
   save it. The server now reports three errors: the syntax error in A and the
   two type errors in B and C. Then I saved file A with the syntax error. The
   server publishes only the syntax diagnostic. The other two are cleared.
   - This happens because, when a syntax error is introduced, `.compiler.log` is
     changed, and the compiler emits only the syntax error.
   - How can we improve this?

2. Considering the example described above, I now fixed the syntax error in A.
   The server publishes the two type errors in B and C. I close A, and the
   server does not clear the diagnostics. In this case, all files are closed.
   The compiler is running.
   - Should we clear diagnostics when closing a document?

3. I have an open file. I added a syntax error, and the server reports it. All
   good. I closed the file, and the server clears the syntax error diagnostic.
   This is the opposite behavior from the case above. The compiler is running.
   - This happens because we ignore syntax errors coming from the compiler.

### Codex suggestion

This is mostly a diagnostic ownership problem. Right now the server mixes two
different sources:

1. **Editor syntax diagnostics** from open, unsaved documents.
2. **Compiler diagnostics** from `.compiler.log`.

Those sources have different freshness guarantees, so they should be tracked
separately.

**1. Saved Syntax Error Clears B/C Type Errors** This happens because
`.compiler.log` becomes incomplete when the compiler stops at the syntax error.
If the server treats that log as a full compiler snapshot, it overwrites
previous compiler diagnostics and clears B/C.

I would fix this by making compiler-log updates conditional:

```text
If compiler log contains syntax errors:
  parse syntax diagnostics, but do not replace previous compiler diagnostics.

If compiler log contains non-syntax diagnostics:
  replace previous compiler diagnostics with the new snapshot.

If compiler log is empty / successful:
  clear previous compiler diagnostics.
```

Since you already ignore compiler syntax errors, the important part is: **do not
let a syntax-only compiler log clear the previous type-check diagnostics**.

Tradeoff: B/C diagnostics may be stale while A has a syntax error. But that is
better than flickering/clearing useful errors. Once syntax is fixed and compiler
produces a real type-check log, replace them.

**2. Close A While B/C Diagnostics Remain** Do not clear compiler diagnostics on
document close. Compiler diagnostics are workspace/build diagnostics, not
open-document diagnostics.

If B and C have errors from the compiler, those should remain even when no files
are open, because the workspace still has build errors.

So the rule should be:

```text
On didClose:
  clear only diagnostics owned by the open-document syntax checker for that URI.
  keep compiler diagnostics unchanged.
```

**3. Closing File Clears Syntax Error** This behavior is reasonable if the
syntax error only exists in the unsaved buffer. Once the document closes, the
server no longer owns the in-memory text, so it should remove in-memory syntax
diagnostics.

But if the file was saved with the syntax error, the compiler should own that
diagnostic. Since you currently ignore compiler syntax diagnostics, saved syntax
errors disappear on close.

The fix is to distinguish syntax source:

```text
Open unsaved file:
  use in-memory syntax diagnostics.

Saved file / closed file:
  allow compiler syntax diagnostics to be published.
```

Or more simply:

```text
Ignore compiler syntax diagnostics only for currently open documents.
Keep compiler syntax diagnostics for closed documents.
```

**Suggested Model** Maintain separate diagnostic maps:

```ocaml
type diagnostics = {
  syntax_by_uri: Diagnostic.t list Uri_map.t;
  compiler_by_uri: Diagnostic.t list Uri_map.t;
}
```

Publishing becomes:

```text
published(uri) = syntax_by_uri(uri) + compiler_by_uri(uri)
```

Then events become predictable:

```text
didChange:
  update syntax_by_uri for that open document
  publish merged diagnostics

compiler log changed:
  if syntax-only/incomplete log:
    update compiler syntax diagnostics for closed files only
    keep previous compiler type diagnostics
  else:
    replace compiler_by_uri with new compiler diagnostics
  publish merged diagnostics

didClose:
  remove syntax_by_uri for that document
  keep compiler_by_uri
  publish merged diagnostics
```

Main practical fix: stop treating every `.compiler.log` change as a complete
replacement. Syntax-error logs are incomplete build results, so they need
special handling.
