let foo = async z => {
  module Utils = await ModuleWithAlias

  Utils.x + z
}
