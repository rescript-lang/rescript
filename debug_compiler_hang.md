# Compiler Hang Fix: Infinite Recursion in E.econd

## Problem Description

The ReScript compiler was hanging indefinitely during compilation when processing certain nested conditional expressions. The hang occurred during the JavaScript code generation phase, specifically in the `E.econd` function which optimizes nested conditional expressions.

## Problematic Code Pattern

The issue was triggered by code patterns that create nested conditional expressions with specific structures. Two examples from `Player.res`:

### Example 1: Lines 40-48

```rescript
let isYAxis = !(gameObj.direction.y == 0.)
switch key {
| Space => (if isYAxis {
    Thundershock.cast(gameObj)
  } else {
    ()->ignore
  })
| _ => ()
}
```

### Example 2: Lines 51-59

```rescript
k->Context.onKeyRelease(key => {
  let isYAxis = !(gameObj.direction.y == 0.)
  switch key {
  | Space => if isYAxis {
      Thundershock.cast(gameObj)->ignore
    }
  | _ => ()
  }
})
```

## Root Cause

The infinite recursion occurred in `compiler/core/js_exp_make.ml` in the `econd` function (which creates optimized conditional expressions). The function attempts to optimize nested conditionals by recursively calling itself:

```ocaml
| _, Cond (pred1, ifso1, ifnot1), _
  when Js_analyzer.eq_expression ifnot1 ifnot ->
    econd (and_ pred pred1) ifso1 ifnot
```

**The Problem:**
1. When `ifso1` is a `Seq` (sequence expression) or another `Cond`, the recursive call `econd (and_ pred pred1) ifso1 ifnot` creates a new `Cond` structure
2. This new structure gets processed by `S.if_` (statement creation), which calls `E.econd` again
3. If `ifso1` contains nested structures (like `Seq`), the `eq_expression` function can loop infinitely when comparing deeply nested sequences
4. This creates an infinite cycle: `E.econd` → creates `Cond` → `S.if_` processes it → calls `E.econd` again → repeat

## The Fix

The fix prevents infinite recursion by skipping the optimization when `ifso1` is a `Cond` or `Seq`:

**Location:** `compiler/core/js_exp_make.ml`, lines 1222-1254

```ocaml
(match ifso1.expression_desc with
 | Cond _ ->
   (* If ifso1 is a Cond, skip this optimization to prevent infinite recursion *)
   {expression_desc = Cond (pred, ifso, ifnot); comment}
 | _ ->
   (* Also check if ifso1 equals ifnot, which would make the result equivalent *)
   (if Js_analyzer.eq_expression ifso1 ifnot then (
      {expression_desc = Cond (pred, ifso, ifnot); comment})
    else (
      (* If ifso1 is a Seq, it might contain nested structures that cause infinite recursion
         in eq_expression. Skip this optimization to prevent that. *)
      (match ifso1.expression_desc with
       | Seq _ ->
         {expression_desc = Cond (pred, ifso, ifnot); comment}
       | _ ->
         econd (and_ pred pred1) ifso1 ifnot)))
```

**Key Changes:**
1. **Line 1223-1227**: Skip optimization when `ifso1` is a `Cond` - prevents creating structures that match the same pattern
2. **Line 1237-1241**: Skip optimization when `ifso1` is a `Seq` - prevents `eq_expression` from looping on nested sequences
3. **Line 1230-1233**: Skip optimization when `ifso1 == ifnot` - prevents creating equivalent structures

Similar guards were added to other patterns in `econd`:
- When `ifnot1` is a `Cond` (lines 1262-1266, 1277-1281)
- When `ifso1` is a `Cond` in other patterns (line 1292-1296)

## How to Reproduce

1. Create a ReScript file with nested conditionals that result in `Seq` or `Cond` structures in the `ifso1` position
2. Compile with the ReScript compiler
3. The compiler will hang indefinitely during compilation

In my project you can try https://github.com/nojaf/rescript-kaplay/commit/2f34e581f346bff016bcc99126ba9656565f7ca6

```rescript
let isYAxis = !(gameObj.direction.y == 0.)
switch key {
| Space => (if isYAxis {
    Thundershock.cast(gameObj)  // This creates a Seq structure
  } else {
    ()->ignore
  })
| _ => ()
}
```

Regular v12

```shell
✨ Finished Compilation in 0.52s
(base) nojaf@nojaf-mbp rescript-kaplay % bunx rescript                                                                                                   
[1/3] 🧹 Cleaned previous build due to compiler update
[1/3] 🧹 Cleaned 0/0 in 0.06s
[2/3] 🧱 Parsed 116 source files in 0.10s
[3/3] 🤺 Compiling... ⠐ 109/119      
```
(bsc.exe gets stuck)

This PR

```shell
(base) nojaf@nojaf-mbp rescript-kaplay % RESCRIPT_BSC_EXE=/Users/nojaf/Projects/rescript/_build/default/compiler/bsc/rescript_compiler_main.exe bunx rescript
[1/3] 🧹 Cleaned previous build due to compiler update
[1/3] 🧹 Cleaned 0/0 in 0.05s
[2/3] 🧱 Parsed 116 source files in 0.10s
[3/3] 🤺 Compiled 116 modules in 0.33s
```

## Files Modified

- `compiler/core/js_exp_make.ml` - Added guards to prevent infinite recursion in `econd` function

## Related Code

The fix is in the `econd` function which optimizes conditional expressions:

```ocaml
let rec econd ?comment (pred : t) (ifso : t) (ifnot : t) : t =
  (* ... *)
  match (pred.expression_desc, ifso.expression_desc, ifnot.expression_desc) with
  | _, Cond (pred1, ifso1, ifnot1), _
    when Js_analyzer.eq_expression ifnot1 ifnot ->
    (* Optimization: if b then (if p1 then branch_code0 else branch_code1) else branch_code1
       is equivalent to: if b && p1 then branch_code0 else branch_code1 *)
    (* FIX: Skip optimization if ifso1 is Cond or Seq to prevent infinite recursion *)
    (match ifso1.expression_desc with
     | Cond _ | Seq _ -> {expression_desc = Cond (pred, ifso, ifnot); comment}
     | _ -> econd (and_ pred pred1) ifso1 ifnot)
```

## Testing

- Git clone https://github.com/nojaf/rescript-kaplay/commit/2f34e581f346bff016bcc99126ba9656565f7ca6
- Build compiler in branch
- RESCRIPT_BSC_EXE=/Users/nojaf/Projects/rescript/_build/default/compiler/bsc/rescript_compiler_main.exe bunx rescript

## Notes

- The fix is conservative: it skips the optimization when there's a risk of infinite recursion
- This is safe because skipping the optimization still produces correct code, just potentially less optimized
- The fix applies to all similar patterns in `econd` to ensure consistency
- Sorry for the AI-ness here, I'm a bit out of my league here.