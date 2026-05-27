open PlaygroundConfig

type tab =
  | Parsetree
  | Typedtree
  | Lambda
  | Lam
  | JavaScript
  | Settings

type compilerStatus =
  | Loading
  | Ready
  | Compiling
  | Failed(string)

type sourcePosition = {
  line: int,
  col: int,
}

let tabs: array<tab> = [Parsetree, Typedtree, Lambda, Lam, JavaScript, Settings]
let moduleSystems: array<moduleSystem> = [Esmodule, Commonjs]

let defaultSource = `type person = {
  name: string,
  age: int,
}

let greet = person =>
  switch person.age {
  | age if age < 18 => "Hi " ++ person.name
  | _ => "Hello " ++ person.name
  }

let message = greet({name: "Ada", age: 36})
Console.log(message)`

let tabLabel = tab =>
  switch tab {
  | Parsetree => "parsetree"
  | Typedtree => "typedtree"
  | Lambda => "lambda"
  | Lam => "lam"
  | JavaScript => "js"
  | Settings => "settings"
  }

let statusLabel = status =>
  switch status {
  | Loading => "loading compiler"
  | Ready => "ready"
  | Compiling => "compiling"
  | Failed(_) => "compiler error"
  }

let jsErrorMessage = obj =>
  switch JsExn.message(obj) {
  | Some(message) => message
  | None => "Unknown JavaScript error"
  }

let insertTabIndent = (event: Dom.event): option<string> =>
  if event->Event.key !== "Tab" {
    None
  } else {
    let target = event->Event.target
    let value = target->EventTarget.value
    let start = target->EventTarget.selectionStart
    let end_ = target->EventTarget.selectionEnd
    let nextValue =
      value->String.slice(~start=0, ~end=start) ++ "  " ++ value->String.slice(~start=end_)
    let cursor = start + 2

    event->Event.preventDefault
    target->EventTarget.setValue(nextValue)
    target->EventTarget.setSelectionRange(cursor, cursor)

    Some(nextValue)
  }

let configureSourceEditor = (scrollHandler: Dom.event => unit): unit =>
  Window.requestAnimationFrame(() =>
    switch Document.current->Document.getElementById("source-editor") {
    | None => ()
    | Some(editor) =>
      editor->Element.setAttribute("wrap", "off")
      switch editor->Element.getScrollHandler {
      | Some(existingHandler) if existingHandler === scrollHandler => ()
      | existingHandler =>
        switch existingHandler {
        | Some(existingHandler) => editor->Element.removeEventListener("scroll", existingHandler)
        | None => ()
        }
        editor->Element.setScrollHandler(scrollHandler)
        editor->Element.addEventListener("scroll", scrollHandler)
      }
    }
  )

let lineNumbersText = source => {
  let lineCount = source->String.split("\n")->Array.length
  Array.make(~length=lineCount, 0)
  ->Array.mapWithIndex((_, index) => (index + 1)->Int.toString)
  ->Array.join("\n")
}

let cursorPositionForOffset = (source, offset): sourcePosition => {
  let sourceLength = String.length(source)
  let boundedOffset = if offset < 0 {
    0
  } else if offset > sourceLength {
    sourceLength
  } else {
    offset
  }

  let rec walk = (index, line, col) =>
    if index >= boundedOffset {
      {line, col}
    } else if source->String.charAt(index) === "\n" {
      walk(index + 1, line + 1, 0)
    } else {
      walk(index + 1, line, col + 1)
    }

  walk(0, 1, 0)
}

let editorShellStyle = (activeLine, scrollTop, scrollLeft) => {
  let activeLineIndex = activeLine <= 1 ? 0 : activeLine - 1
  let activeLineTop = 18 + activeLineIndex * 22 - scrollTop
  `--active-line-top: ${activeLineTop->Int.toString}px; --editor-scroll-y: -${scrollTop->Int.toString}px; --editor-scroll-x: -${scrollLeft->Int.toString}px;`
}

type tokenKind =
  | TokenPlain
  | TokenKeyword
  | TokenBuiltin
  | TokenConstructor
  | TokenString
  | TokenNumber
  | TokenComment
  | TokenAttribute
  | TokenOperator

type highlightToken = {
  kind: tokenKind,
  text: string,
}

let syntaxKeywords = [
  "and",
  "as",
  "async",
  "await",
  "catch",
  "constraint",
  "else",
  "exception",
  "external",
  "false",
  "for",
  "if",
  "in",
  "include",
  "let",
  "module",
  "mutable",
  "open",
  "private",
  "rec",
  "switch",
  "to",
  "true",
  "try",
  "type",
  "when",
  "while",
]

let syntaxBuiltins = [
  "array",
  "bigint",
  "bool",
  "dict",
  "float",
  "int",
  "list",
  "option",
  "promise",
  "result",
  "string",
  "unit",
]

let charAt = (source, index) => source->String.charAt(index)

let isLower = char => char >= "a" && char <= "z"
let isUpper = char => char >= "A" && char <= "Z"
let isDigit = char => char >= "0" && char <= "9"
let isAlpha = char => isLower(char) || isUpper(char)
let isIdentStart = char => isAlpha(char) || char === "_"
let isIdentPart = char => isIdentStart(char) || isDigit(char) || char === "'"

let isOperatorChar = char =>
  switch char {
  | "="
  | ">"
  | "<"
  | "+"
  | "-"
  | "*"
  | "/"
  | "|"
  | "&"
  | "!"
  | "?"
  | ":"
  | "."
  | "~"
  | "^"
  | "%"
  | "#" => true
  | _ => false
  }

let startsWithAt = (source, index, prefix) =>
  source->String.slice(~start=index, ~end=index + String.length(prefix)) === prefix

let rec findLineEnd = (source, index, length) =>
  if index >= length || charAt(source, index) === "\n" {
    index
  } else {
    findLineEnd(source, index + 1, length)
  }

let rec findBlockEnd = (source, index, length, closing) =>
  if index >= length {
    length
  } else if startsWithAt(source, index, closing) {
    index + String.length(closing)
  } else {
    findBlockEnd(source, index + 1, length, closing)
  }

let rec findStringEnd = (source, index, length, delimiter, escaped) =>
  if index >= length {
    length
  } else {
    let char = charAt(source, index)
    if escaped {
      findStringEnd(source, index + 1, length, delimiter, false)
    } else if char === "\\" {
      findStringEnd(source, index + 1, length, delimiter, true)
    } else if char === delimiter {
      index + 1
    } else {
      findStringEnd(source, index + 1, length, delimiter, false)
    }
  }

let rec findIdentEnd = (source, index, length) =>
  if index < length && isIdentPart(charAt(source, index)) {
    findIdentEnd(source, index + 1, length)
  } else {
    index
  }

let rec findAttributeEnd = (source, index, length) =>
  if index < length {
    let char = charAt(source, index)
    if isIdentPart(char) || char === "." || char === "@" {
      findAttributeEnd(source, index + 1, length)
    } else {
      index
    }
  } else {
    index
  }

let rec findNumberEnd = (source, index, length) =>
  if index < length {
    let char = charAt(source, index)
    if isDigit(char) || isAlpha(char) || char === "_" || char === "." {
      findNumberEnd(source, index + 1, length)
    } else {
      index
    }
  } else {
    index
  }

let rec findOperatorEnd = (source, index, length) =>
  if index < length && isOperatorChar(charAt(source, index)) {
    findOperatorEnd(source, index + 1, length)
  } else {
    index
  }

let tokenKindForIdent = word =>
  if syntaxKeywords->Array.includes(word) {
    TokenKeyword
  } else if syntaxBuiltins->Array.includes(word) {
    TokenBuiltin
  } else if String.length(word) > 0 && isUpper(charAt(word, 0)) {
    TokenConstructor
  } else {
    TokenPlain
  }

let tokenizeRescript = source => {
  let tokens: array<highlightToken> = []
  let length = String.length(source)
  let index = ref(0)

  while index.contents < length {
    let start = index.contents
    let char = charAt(source, start)
    let (next, kind) = if startsWithAt(source, start, "//") {
      (findLineEnd(source, start, length), TokenComment)
    } else if startsWithAt(source, start, "/*") {
      (findBlockEnd(source, start + 2, length, "*/"), TokenComment)
    } else if char === "\"" || char === "`" {
      (findStringEnd(source, start + 1, length, char, false), TokenString)
    } else if char === "@" {
      (findAttributeEnd(source, start + 1, length), TokenAttribute)
    } else if isDigit(char) {
      (findNumberEnd(source, start + 1, length), TokenNumber)
    } else if isIdentStart(char) {
      let next = findIdentEnd(source, start + 1, length)
      let word = source->String.slice(~start, ~end=next)
      (next, tokenKindForIdent(word))
    } else if isOperatorChar(char) {
      (findOperatorEnd(source, start + 1, length), TokenOperator)
    } else {
      (start + 1, TokenPlain)
    }

    tokens->Array.push({
      kind,
      text: source->String.slice(~start, ~end=next),
    })
    index := next
  }

  tokens
}

let tokenClass = kind =>
  switch kind {
  | TokenPlain => "syntax-token"
  | TokenKeyword => "syntax-token syntax-keyword"
  | TokenBuiltin => "syntax-token syntax-builtin"
  | TokenConstructor => "syntax-token syntax-constructor"
  | TokenString => "syntax-token syntax-string"
  | TokenNumber => "syntax-token syntax-number"
  | TokenComment => "syntax-token syntax-comment"
  | TokenAttribute => "syntax-token syntax-attribute"
  | TokenOperator => "syntax-token syntax-operator"
  }

let highlightNodes = source =>
  tokenizeRescript(source)->Array.map(token =>
    <span class={tokenClass(token.kind)}> {Node.text(token.text)} </span>
  )

let hasFeature = (features: array<experimentalFeature>, feature: experimentalFeature) =>
  features->Array.includes(feature)

let toggleFeature = (features: array<experimentalFeature>, feature: experimentalFeature) =>
  hasFeature(features, feature)
    ? features->Array.filter(item => item !== feature)
    : Array.concat(features, [feature])

let selectedOutput = (result: option<CompilerApi.compileResult>, activeTab: tab) =>
  switch result {
  | None => "The compiler is loading. Results will appear here after the first compile."
  | Some(Error(result)) =>
    let errors = result.errors->Array.join("\n")
    errors === "" ? result.message : errors
  | Some(Ok(result)) =>
    switch activeTab {
    | Parsetree => result.parsetree
    | Typedtree => result.typedtree
    | Lambda => result.lambda
    | Lam => result.lam
    | JavaScript => result.jsCode
    | Settings => ""
    }
  }

let resultSummary = (result: option<CompilerApi.compileResult>) =>
  switch result {
  | None => "No compile result yet"
  | Some(Ok(result)) =>
    let warningCount = result.warnings->Array.length
    let warningText = warningCount === 0 ? "no warnings" : `${warningCount->Int.toString} warnings`
    `Compiled in ${result.time->Float.toFixed(~digits=1)}ms with ${warningText}`
  | Some(Error(result)) => result.message
  }

module TabButton = {
  @jsx.component
  let make = (~tab, ~activeTab: Signal.t<tab>, ~onSelect: tab => unit) => {
    <button
      class={() => Signal.get(activeTab) === tab ? "tab-button tab-button-active" : "tab-button"}
      onClick={_ => onSelect(tab)}
    >
      {Node.text(tabLabel(tab))}
    </button>
  }
}

module Problems = {
  @jsx.component
  let make = (~compileResult: Signal.t<option<CompilerApi.compileResult>>) => {
    <div class="problems">
      <div class="problems-title"> {Node.text("Problems")} </div>
      <pre class="problems-output">
        {Node.signalText(() =>
          switch Signal.get(compileResult) {
          | Some(Ok({warnings})) if warnings->Array.length > 0 => warnings->Array.join("\n")
          | Some(Error({warnings})) if warnings->Array.length > 0 => warnings->Array.join("\n")
          | Some(Error({errors})) if errors->Array.length > 0 => errors->Array.join("\n")
          | Some(Error({message})) => message
          | _ => "No problems reported."
          }
        )}
      </pre>
    </div>
  }
}

module SettingsPanel = {
  @jsx.component
  let make = (
    ~activeTab: Signal.t<tab>,
    ~compilerInfo: Signal.t<option<CompilerApi.info>>,
    ~config: Signal.t<PlaygroundConfig.t>,
    ~switchCompiler: string => unit,
    ~compileNow: unit => unit,
    ~scheduleCompile: unit => unit,
    ~scheduleUrlSync: unit => unit,
  ) => {
    let updateConfig = f => Signal.update(config, f)

    <div
      class={() =>
        Signal.get(activeTab) === Settings ? "settings-panel" : "settings-panel hidden-panel"}
    >
      <section class="settings-section">
        <label class="setting-label" for_="compiler-version">
          {Node.text("Compiler Version")}
        </label>
        <select
          id="compiler-version"
          value={() => Signal.get(config).compilerVersion}
          onChange={event => {
            let nextVersion = Event.value(event)
            updateConfig(config => {...config, compilerVersion: nextVersion})
            switchCompiler(nextVersion)
          }}
        >
          {Node.fragment(
            CompilerApi.availableCompilerVersions->Array.map(version =>
              <option value=version.id> {Node.text(version.label)} </option>
            ),
          )}
        </select>
      </section>
      <section class="settings-section">
        <label class="setting-label"> {Node.text("Loaded Compiler")} </label>
        <div class="setting-value">
          {Node.signalText(() =>
            switch Signal.get(compilerInfo) {
            | Some(info) => `${info.version} / API ${info.apiVersion} / ${info.bundleId}`
            | None => "loading"
            }
          )}
        </div>
      </section>
      <section class="settings-section">
        <label class="setting-label" for_="module-system"> {Node.text("Module System")} </label>
        <select
          id="module-system"
          value={() => (Signal.get(config).moduleSystem :> string)}
          onChange={event => {
            switch event->Event.value->parseModuleSystem {
            | Some(nextModuleSystem) =>
              updateConfig(config => {...config, moduleSystem: nextModuleSystem})
              scheduleUrlSync()
              compileNow()
            | None => ()
            }
          }}
        >
          {Node.fragment(
            moduleSystems->Array.map(moduleSystem => {
              let value = (moduleSystem :> string)
              <option value> {Node.text(value)} </option>
            }),
          )}
        </select>
      </section>
      <section class="settings-section">
        <label class="setting-label" for_="warning-flags"> {Node.text("Warning Flags")} </label>
        <input
          id="warning-flags"
          value={() => Signal.get(config).warnFlags}
          spellcheck=false
          onInput={event => {
            updateConfig(config => {...config, warnFlags: Event.value(event)})
            scheduleUrlSync()
            scheduleCompile()
          }}
        />
        <button
          class="secondary-action"
          onClick={_ => {
            updateConfig(config => {...config, warnFlags: CompilerApi.defaultConfig.warnFlags})
            scheduleUrlSync()
            compileNow()
          }}
        >
          {Node.text("Reset")}
        </button>
      </section>
      <section class="settings-section setting-row">
        <input
          id="jsx-preserve"
          type_="checkbox"
          checked={() => Signal.get(config).jsxPreserveMode}
          onChange={event => {
            updateConfig(config => {...config, jsxPreserveMode: Event.checked(event)})
            scheduleUrlSync()
            compileNow()
          }}
        />
        <label for_="jsx-preserve"> {Node.text("Preserve JSX output")} </label>
      </section>
      <section class="settings-section setting-row">
        <input
          id="feature-let-unwrap"
          type_="checkbox"
          checked={() => Signal.get(config).experimentalFeatures->hasFeature(LetUnwrap)}
          onChange={_ => {
            updateConfig(config => {
              ...config,
              experimentalFeatures: toggleFeature(config.experimentalFeatures, LetUnwrap),
            })
            scheduleUrlSync()
            compileNow()
          }}
        />
        <label for_="feature-let-unwrap"> {Node.text("Experimental: let?")} </label>
      </section>
      <section class="settings-section">
        <label class="setting-label"> {Node.text("Loaded Libraries")} </label>
        <div class="setting-value">
          {Node.signalText(() =>
            switch Signal.get(compilerInfo) {
            | Some(info) => info.libraries->Array.join(", ")
            | None => "loading"
            }
          )}
        </div>
      </section>
    </div>
  }
}

module StatusBadge = {
  @jsx.component
  let make = (~status: Signal.t<compilerStatus>) => {
    <div
      class={() =>
        switch Signal.get(status) {
        | Failed(_) => "status status-error"
        | Compiling | Loading => "status status-busy"
        | Ready => "status"
        }}
    >
      {Node.signalText(() =>
        switch Signal.get(status) {
        | Failed(message) => message
        | other => statusLabel(other)
        }
      )}
    </div>
  }
}

module App = {
  @jsx.component
  let make = () => {
    let source = Signal.make(defaultSource)
    let activeTab = Signal.make(JavaScript)
    let status = Signal.make(Loading)
    let compilerInfo: Signal.t<option<CompilerApi.info>> = Signal.make(None)
    let compileResult: Signal.t<option<CompilerApi.compileResult>> = Signal.make(None)
    let config = Signal.make(CompilerApi.defaultConfig)
    let activeLine = Signal.make(1)
    let editorScrollTop = Signal.make(0)
    let editorScrollLeft = Signal.make(0)
    let highlightedSource: Signal.t<array<Node.node>> = Obj.magic(
      Computed.make(() => highlightNodes(Signal.get(source))),
    )
    let timerId: ref<option<int>> = ref(None)
    let urlTimerId: ref<option<int>> = ref(None)
    let toastTimerId: ref<option<int>> = ref(None)
    let firstLoadConfig: ref<option<PlaygroundConfig.t>> = ref(None)
    let compilerLoadSequence = ref(0)
    let compileSequence = ref(0)
    let shareToast: Signal.t<option<string>> = Signal.make(None)

    let syncEditorState = event => {
      let currentSource = Event.value(event)
      let cursorPosition = cursorPositionForOffset(currentSource, Event.selectionStart(event))

      Signal.set(editorScrollTop, Event.scrollTop(event))
      Signal.set(editorScrollLeft, Event.scrollLeft(event))
      Signal.set(activeLine, cursorPosition.line)
    }

    let syncEditorScroll = event => {
      Signal.set(editorScrollTop, Event.scrollTop(event))
      Signal.set(editorScrollLeft, Event.scrollLeft(event))
    }

    let compileNow = () => {
      compileSequence := compileSequence.contents + 1
      let sequence = compileSequence.contents

      let run = async () => {
        switch Signal.peek(status) {
        | Loading => ()
        | Failed(_) => ()
        | Ready | Compiling =>
          Signal.set(status, Compiling)
          try {
            let result = await CompilerApi.compile(Signal.peek(source), Signal.peek(config))
            if sequence === compileSequence.contents {
              Signal.set(compileResult, Some(result))
              Signal.set(status, Ready)
            }
          } catch {
          | JsExn(obj) =>
            if sequence === compileSequence.contents {
              Signal.set(status, Failed(jsErrorMessage(obj)))
            }
          | _ =>
            if sequence === compileSequence.contents {
              Signal.set(status, Failed("Compilation failed"))
            }
          }
        }
      }

      run()->ignore
    }

    let scheduleCompile = () => {
      switch timerId.contents {
      | Some(id) => Window.clearTimeout(id)
      | None => ()
      }
      timerId := Some(Window.setTimeout(compileNow, 280))
    }

    let syncUrlNow = () =>
      UrlState.replace(~source=Signal.peek(source), ~config=Signal.peek(config))->Promise.ignore

    let scheduleUrlSync = () => {
      switch urlTimerId.contents {
      | Some(id) => Window.clearTimeout(id)
      | None => ()
      }
      urlTimerId := Some(Window.setTimeout(syncUrlNow, 360))
    }

    let formatSource = () => {
      compileSequence := compileSequence.contents + 1
      let sequence = compileSequence.contents
      let sourceBeforeFormat = Signal.peek(source)

      let run = async () => {
        switch Signal.peek(status) {
        | Loading => ()
        | Failed(_) => ()
        | Ready | Compiling =>
          Signal.set(status, Compiling)
          try {
            switch await CompilerApi.format(sourceBeforeFormat, Signal.peek(config)) {
            | Ok(formattedSource) =>
              if sequence === compileSequence.contents {
                if Signal.peek(source) === sourceBeforeFormat {
                  Signal.set(source, formattedSource)
                  Signal.set(activeLine, 1)
                  Signal.set(editorScrollTop, 0)
                  Signal.set(editorScrollLeft, 0)
                  scheduleUrlSync()
                  Signal.set(status, Ready)
                  compileNow()
                } else {
                  Signal.set(status, Ready)
                }
              }
            | Error(failure) =>
              if sequence === compileSequence.contents {
                Signal.set(compileResult, Some(Error(failure)))
                Signal.set(status, Ready)
              }
            }
          } catch {
          | JsExn(obj) =>
            if sequence === compileSequence.contents {
              Signal.set(status, Failed(jsErrorMessage(obj)))
            }
          | _ =>
            if sequence === compileSequence.contents {
              Signal.set(status, Failed("Formatting failed"))
            }
          }
        }
      }

      run()->ignore
    }

    let showToast = message => {
      switch toastTimerId.contents {
      | Some(id) => Window.clearTimeout(id)
      | None => ()
      }
      Signal.set(shareToast, Some(message))
      toastTimerId := Some(Window.setTimeout(() => Signal.set(shareToast, None), 1800))
    }

    let shareCurrentUrl = () => {
      switch urlTimerId.contents {
      | Some(id) => Window.clearTimeout(id)
      | None => ()
      }

      let share = async () => {
        switch await UrlState.copy(~source=Signal.peek(source), ~config=Signal.peek(config)) {
        | Ok() => showToast("Link copied")
        | Error(message) => showToast(message)
        }
      }

      share()->Promise.ignore
    }

    let loadCompiler = (version, compileAfterLoad) => {
      compilerLoadSequence := compilerLoadSequence.contents + 1
      compileSequence := compileSequence.contents + 1
      let sequence = compilerLoadSequence.contents

      let load = async () => {
        try {
          Signal.set(status, Loading)
          Signal.set(compileResult, None)
          let info = await CompilerApi.init(version)
          if sequence === compilerLoadSequence.contents {
            let firstLoadConfigValue = firstLoadConfig.contents
            firstLoadConfig := None
            let nextConfig = switch firstLoadConfigValue {
            | Some(config) => {...config, compilerVersion: info.bundleId}
            | None => {
                PlaygroundConfig.compilerVersion: info.bundleId,
                moduleSystem: info.moduleSystem,
                warnFlags: info.warnFlags,
                jsxPreserveMode: info.jsxPreserveMode,
                experimentalFeatures: info.experimentalFeatures,
              }
            }
            Signal.set(compilerInfo, Some(info))
            Signal.set(config, nextConfig)
            Signal.set(status, Ready)
            switch firstLoadConfigValue {
            | Some(_) => ()
            | None => scheduleUrlSync()
            }
            if compileAfterLoad {
              compileNow()
            }
          }
        } catch {
        | JsExn(obj) =>
          if sequence === compilerLoadSequence.contents {
            Signal.set(status, Failed(jsErrorMessage(obj)))
          }
        | _ =>
          if sequence === compilerLoadSequence.contents {
            Signal.set(status, Failed("Compiler failed to load"))
          }
        }
      }

      load()->ignore
    }

    let switchCompiler = version => loadCompiler(version, true)

    Effect.run(() => {
      let start = async () => {
        let urlState = await UrlState.init(~defaultSource)
        Signal.set(source, urlState.source)
        Signal.set(config, urlState.config)
        Signal.set(activeLine, 1)
        Signal.set(editorScrollTop, 0)
        Signal.set(editorScrollLeft, 0)
        firstLoadConfig := Some(urlState.config)
        loadCompiler(urlState.config.compilerVersion, true)
      }

      start()->ignore
      None
    })

    Effect.run(() => {
      configureSourceEditor(syncEditorScroll)
      None
    })

    <main class="app-shell">
      <header class="topbar">
        <div>
          <h1> {Node.text("ReScript Developer Playground")} </h1>
        </div>
        <StatusBadge status />
      </header>
      <section class="workspace">
        <div class="source-column">
          <div class="column-header">
            <h2> {Node.text("Source")} </h2>
            <div class="actions">
              <button class="secondary-action" onClick={_ => formatSource()}>
                {Node.text("Format")}
              </button>
              <button
                class="secondary-action"
                onClick={_ => {
                  Signal.set(source, defaultSource)
                  Signal.set(activeLine, 1)
                  Signal.set(editorScrollTop, 0)
                  Signal.set(editorScrollLeft, 0)
                  scheduleUrlSync()
                  scheduleCompile()
                }}
              >
                {Node.text("Reset")}
              </button>
              <button class="secondary-action" onClick={_ => shareCurrentUrl()}>
                {Node.text("Share")}
              </button>
            </div>
          </div>
          <div
            class="editor-shell"
            style={() =>
              editorShellStyle(
                Signal.get(activeLine),
                Signal.get(editorScrollTop),
                Signal.get(editorScrollLeft),
              )}
          >
            <div class="active-line" ariaHidden=true />
            <div class="line-number-gutter" ariaHidden=true>
              <pre class="line-numbers">
                {Node.signalText(() => lineNumbersText(Signal.get(source)))}
              </pre>
            </div>
            <pre class="syntax-layer" ariaHidden=true>
              {Node.signalFragment(highlightedSource)}
            </pre>
            <textarea
              id="source-editor"
              class="editor"
              value={ReactiveProp.reactive(source)}
              spellcheck=false
              onInput={event => {
                Signal.set(source, Event.value(event))
                syncEditorState(event)
                scheduleUrlSync()
                scheduleCompile()
              }}
              onClick={syncEditorState}
              onMouseUp={syncEditorState}
              onKeyUp={syncEditorState}
              onFocus={syncEditorState}
              onKeyDown={event =>
                switch insertTabIndent(event) {
                | Some(nextSource) =>
                  Signal.set(source, nextSource)
                  syncEditorState(event)
                  scheduleUrlSync()
                  scheduleCompile()
                | None => ()
                }}
            />
          </div>
        </div>
        <div class="result-column">
          <div class="tabs">
            {Node.fragment(
              tabs->Array.map(tab =>
                <TabButton tab activeTab onSelect={tab => Signal.set(activeTab, tab)} />
              ),
            )}
          </div>
          <div
            class={() =>
              Signal.get(activeTab) === Settings ? "output-panel hidden-panel" : "output-panel"}
          >
            <div class="result-meta">
              {Node.signalText(() => resultSummary(Signal.get(compileResult)))}
            </div>
            <div class="output-shell">
              <pre class="output">
                {Node.signalText(() =>
                  selectedOutput(Signal.get(compileResult), Signal.get(activeTab))
                )}
              </pre>
            </div>
            <Problems compileResult />
          </div>
          <SettingsPanel
            activeTab compilerInfo config switchCompiler compileNow scheduleCompile scheduleUrlSync
          />
        </div>
      </section>
      <div
        class={() =>
          switch Signal.get(shareToast) {
          | Some(_) => "toast toast-visible"
          | None => "toast"
          }}
      >
        {Node.signalText(() =>
          switch Signal.get(shareToast) {
          | Some(message) => message
          | None => ""
          }
        )}
      </div>
    </main>
  }
}

Node.mountById(<App />, "app")
