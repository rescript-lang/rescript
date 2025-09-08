for p in 0 to 10 {
   () 
}

for p in 10 downto 0 {
   () 
}

for (p in a to b) {
  ()
}

for (p in a to b) {
  let a = 1
  let b = 2
  a + b
}

// (x +1) { -> not an arrow expression
for p in 0 to (x + 1) {
  ()
}
