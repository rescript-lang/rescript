module type StandaloneAttribute = {
  @@standaloneAttribute

  @@standaloneAttribute(withPayload)

  @@standaloneAttribute(x => x)
}
