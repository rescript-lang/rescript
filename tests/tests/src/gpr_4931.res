@@config({
  flags: ["-w", "-103"],
})

%%raw(`
if (import.meta.hot){
  console.log('es6')
}
`)
