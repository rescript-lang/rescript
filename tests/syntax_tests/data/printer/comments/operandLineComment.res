// A line comment in the operand of a block construct stays in the operand

switch obj // c
.x {
| _ => ()
}

switch obj // c
.x.y {
| _ => ()
}

switch obj // c
.x // d
.y {
| _ => ()
}

switch arr[// c
0] {
| _ => ()
}

switch obj[// c
"member"] {
| _ => ()
}

switch obj["member"] // c
{
| _ => ()
}

switch obj // c
->f {
| _ => ()
}

switch a // c
+ b {
| _ => ()
}

switch f(// c
x) {
| _ => ()
}

switch x { // c
| _ => ()
}

if obj // c
.x {
  ()
} else if obj // d
.y {
  ()
}

while obj // c
.x {
  ()
}

for x of obj // c
.x {
  ()
}

let f = async () => {
  for await x of obj // c
  .x {
    ()
  }
}

for i in 0 // c
to n {
  ()
}

for i in 0 to n // c
{
  ()
}

try obj // c
.x catch {
| _ => ()
}

// An operand printed in its own parentheses keeps the comment inside them
switch (f(
  a, // c
  b,
) :> int) {
| _ => ()
}

switch (f(
  a, // c
  b,
): int) {
| _ => ()
}

if (f(
  a, // c
  b,
) :> bool) {
  ()
}

while (f(
  a, // c
  b,
) :> bool) {
  ()
}

for x of (f(
  a, // c
  b,
) :> array<int>) {
  ()
}

try (f(
  a, // c
  b,
) :> int) catch {
| _ => ()
}

// A comment after an item of the operand
switch [
  a, // c
  b, // d
] {
| _ => ()
}

switch list{
  a, // c
  ...b,
  ...c,
} {
| _ => ()
}

switch (
  a, // c
  b,
) {
| _ => ()
}

switch {
  x: a, // c
  y: b,
} {
| _ => ()
}

switch a->f(
  x, // c
  y,
) {
| _ => ()
}

switch a // c
->f(
  x, // d
  y,
) {
| _ => ()
}

if f(
  a, // c
  b,
) {
  ()
}

while f(
  a, // c
  b,
) + 1 > 0 {
  ()
}

for x of [
  a, // c
  b,
] {
  ()
}

try [
  ...a, // c
  b,
] catch {
| _ => ()
}

// A binary expression or pipe in an item of the operand
switch [a // c
+ b, d] {
| _ => ()
}

if f(a // c
&& b) {
  ()
}

for x of [a // c
->f(y)] {
  ()
}
