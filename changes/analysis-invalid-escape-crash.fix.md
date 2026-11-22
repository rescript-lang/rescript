- Fix editor completion and code actions crashing on an invalid escape in a
  template string inside an attribute, such as ``@module(`\xZZ`)``.
