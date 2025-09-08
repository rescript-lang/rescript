module type Ext = {
  %%item.extension

  %%item.extension.with.args("argument")

  %%item.extension.with.args(x => f(x))

  @withAttr
  %%item.extension
}
