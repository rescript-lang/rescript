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

module Browser = {
  @val external setTimeout: (unit => unit, int) => int = "setTimeout"
  @val external clearTimeout: int => unit = "clearTimeout"

  let eventValue = (event: Dom.event): string => {
    ignore(event)
    let value: string = %raw(`event.target.value`)
    value
  }

  let eventChecked = (event: Dom.event): bool => {
    ignore(event)
    let checked: bool = %raw(`event.target.checked`)
    checked
  }

  let eventSelectionStart = (event: Dom.event): int => {
    ignore(event)
    let selectionStart: int = %raw(`event.target.selectionStart || 0`)
    selectionStart
  }

  let eventScrollTop = (event: Dom.event): int => {
    ignore(event)
    let scrollTop: int = %raw(`Math.round(event.target.scrollTop || 0)`)
    scrollTop
  }

  let eventScrollLeft = (event: Dom.event): int => {
    ignore(event)
    let scrollLeft: int = %raw(`Math.round(event.target.scrollLeft || 0)`)
    scrollLeft
  }

  let insertTabIndent = (event: Dom.event): option<string> => {
    ignore(event)
    let value: option<string> = %raw(`
      (() => {
        if (event.key !== "Tab") {
          return undefined;
        }

        const target = event.target;
        if (target == null || typeof target.value !== "string") {
          return undefined;
        }

        event.preventDefault();

        const start = target.selectionStart || 0;
        const end = target.selectionEnd || start;
        const nextValue = target.value.slice(0, start) + "  " + target.value.slice(end);
        const cursor = start + 2;

        target.value = nextValue;
        target.setSelectionRange(cursor, cursor);

        return nextValue;
      })()
    `)
    value
  }

  let configureSourceEditor = (scrollHandler: Dom.event => unit): unit => {
    ignore(scrollHandler)
    %raw(`
      window.requestAnimationFrame(() => {
        const editor = document.getElementById("source-editor");
        if (editor == null) {
          return;
        }

        editor.setAttribute("wrap", "off");

        if (editor.__devPlaygroundScrollHandler === scrollHandler) {
          return;
        }
        if (editor.__devPlaygroundScrollHandler != null) {
          editor.removeEventListener("scroll", editor.__devPlaygroundScrollHandler);
        }
        editor.__devPlaygroundScrollHandler = scrollHandler;
        editor.addEventListener("scroll", scrollHandler);
      })
    `)
  }

  let jsErrorMessage = obj =>
    switch JsExn.message(obj) {
    | Some(message) => message
    | None => "Unknown JavaScript error"
    }
}

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
  char === "=" ||
  char === ">" ||
  char === "<" ||
  char === "+" ||
  char === "-" ||
  char === "*" ||
  char === "/" ||
  char === "|" ||
  char === "&" ||
  char === "!" ||
  char === "?" ||
  char === ":" ||
  char === "." ||
  char === "~" ||
  char === "^" ||
  char === "%" ||
  char === "#"

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

let hasFeature = (
  features: array<experimentalFeature>,
  feature: experimentalFeature,
) => features->Array.includes(feature)

let toggleFeature = (
  features: array<experimentalFeature>,
  feature: experimentalFeature,
) =>
  hasFeature(features, feature)
    ? features->Array.filter(item => item !== feature)
    : Array.concat(features, [feature])

let debugOutputUnavailable = "This local compiler bundle does not expose this debug dump yet."

let debugOutput = output => output->Option.getOr(debugOutputUnavailable)

let selectedOutput = (result: option<CompilerApi.result>, activeTab: tab) =>
  switch result {
  | None => "The compiler is loading. Results will appear here after the first compile."
  | Some(CompilerApi.Failure(result)) =>
    let errors = result.errors->Array.join("\n")
    errors === "" ? result.message : errors
  | Some(CompilerApi.Success(result)) =>
    switch activeTab {
    | Parsetree => result.parsetree->debugOutput
    | Typedtree => result.typedtree->debugOutput
    | Lambda => result.lambda_->debugOutput
    | Lam => result.lam->debugOutput
    | JavaScript => result.jsCode
    | Settings => ""
    }
  }

let resultSummary = (result: option<CompilerApi.result>) =>
  switch result {
  | None => "No compile result yet"
  | Some(CompilerApi.Success(result)) =>
    let warningCount = result.warnings->Array.length
    let warningText = warningCount === 0 ? "no warnings" : `${warningCount->Int.toString} warnings`
    `Compiled in ${result.time->Float.toFixed(~digits=1)}ms with ${warningText}`
  | Some(CompilerApi.Failure(result)) => result.message
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
  let make = (~compileResult: Signal.t<option<CompilerApi.result>>) => {
    <div class="problems">
      <div class="problems-title"> {Node.text("Problems")} </div>
      <pre class="problems-output">
        {Node.signalText(() =>
          switch Signal.get(compileResult) {
          | Some(CompilerApi.Success({warnings})) if warnings->Array.length > 0 =>
            warnings->Array.join("\n")
          | Some(CompilerApi.Failure({warnings})) if warnings->Array.length > 0 =>
            warnings->Array.join("\n")
          | Some(CompilerApi.Failure({errors})) if errors->Array.length > 0 =>
            errors->Array.join("\n")
          | Some(CompilerApi.Failure({message})) => message
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
    ~compilerVersion: Signal.t<string>,
    ~moduleSystem: Signal.t<moduleSystem>,
    ~warnFlags: Signal.t<string>,
    ~jsxPreserveMode: Signal.t<bool>,
    ~experimentalFeatures: Signal.t<array<experimentalFeature>>,
    ~switchCompiler: string => unit,
    ~compileNow: unit => unit,
    ~scheduleCompile: unit => unit,
    ~scheduleUrlSync: unit => unit,
  ) => {
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
          value={ReactiveProp.reactive(compilerVersion)}
          onChange={event => {
            let nextVersion = Browser.eventValue(event)
            Signal.set(compilerVersion, nextVersion)
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
          value={() => (Signal.get(moduleSystem) :> string)}
          onChange={event => {
            switch event->Browser.eventValue->parseModuleSystem {
            | Some(nextModuleSystem) =>
              Signal.set(moduleSystem, nextModuleSystem)
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
          value={ReactiveProp.reactive(warnFlags)}
          spellcheck=false
          onInput={event => {
            Signal.set(warnFlags, Browser.eventValue(event))
            scheduleUrlSync()
            scheduleCompile()
          }}
        />
        <button
          class="secondary-action"
          onClick={_ => {
            Signal.set(warnFlags, CompilerApi.defaultWarnFlags)
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
          checked={ReactiveProp.reactive(jsxPreserveMode)}
          onChange={event => {
            Signal.set(jsxPreserveMode, Browser.eventChecked(event))
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
          checked={() => Signal.get(experimentalFeatures)->hasFeature(LetUnwrap)}
          onChange={_ => {
            Signal.update(experimentalFeatures, features => toggleFeature(features, LetUnwrap))
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
    let requestedCompilerVersion = UrlState.queryCompilerVersion(CompilerApi.defaultCompilerVersion)
    let initialCompilerVersion =
      CompilerApi.availableCompilerVersions->Array.some(version =>
        version.id === requestedCompilerVersion
      )
        ? requestedCompilerVersion
        : CompilerApi.defaultCompilerVersion
    let initialModuleSystem = UrlState.queryModuleSystem(Esmodule)
    let initialWarnFlags = UrlState.queryWarnFlags(CompilerApi.defaultWarnFlags)
    let initialJsxPreserveMode = UrlState.queryJsxPreserveMode(false)
    let initialExperimentalFeatures = UrlState.queryExperimentalFeatures()

    let source = Signal.make(defaultSource)
    let activeTab = Signal.make(JavaScript)
    let status = Signal.make(Loading)
    let compilerInfo: Signal.t<option<CompilerApi.info>> = Signal.make(None)
    let compileResult: Signal.t<option<CompilerApi.result>> = Signal.make(None)
    let compilerVersion = Signal.make(initialCompilerVersion)
    let moduleSystem = Signal.make(initialModuleSystem)
    let warnFlags = Signal.make(initialWarnFlags)
    let jsxPreserveMode = Signal.make(initialJsxPreserveMode)
    let experimentalFeatures: Signal.t<array<experimentalFeature>> = Signal.make(
      initialExperimentalFeatures,
    )
    let activeLine = Signal.make(1)
    let editorScrollTop = Signal.make(0)
    let editorScrollLeft = Signal.make(0)
    let highlightedSource: Signal.t<array<Node.node>> = Obj.magic(
      Computed.make(() => highlightNodes(Signal.get(source))),
    )
    let timerId: ref<option<int>> = ref(None)
    let urlTimerId: ref<option<int>> = ref(None)
    let toastTimerId: ref<option<int>> = ref(None)
    let firstCompilerLoad = ref(true)
    let compilerLoadSequence = ref(0)
    let compileSequence = ref(0)
    let shareToast: Signal.t<option<string>> = Signal.make(None)

    let syncEditorState = event => {
      let currentSource = Browser.eventValue(event)
      let cursorPosition = cursorPositionForOffset(
        currentSource,
        Browser.eventSelectionStart(event),
      )
      Signal.set(editorScrollTop, Browser.eventScrollTop(event))
      Signal.set(editorScrollLeft, Browser.eventScrollLeft(event))
      Signal.set(activeLine, cursorPosition.line)
    }

    let syncEditorScroll = event => {
      Signal.set(editorScrollTop, Browser.eventScrollTop(event))
      Signal.set(editorScrollLeft, Browser.eventScrollLeft(event))
    }

    let currentConfig = (): CompilerApi.config => {
      compilerVersion: Signal.peek(compilerVersion),
      moduleSystem: Signal.peek(moduleSystem),
      warnFlags: Signal.peek(warnFlags),
      jsxPreserveMode: Signal.peek(jsxPreserveMode),
      experimentalFeatures: Signal.peek(experimentalFeatures),
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
            let result = await CompilerApi.compile(Signal.peek(source), currentConfig())
            if sequence === compileSequence.contents {
              Signal.set(compileResult, Some(result))
              Signal.set(status, Ready)
            }
          } catch {
          | JsExn(obj) =>
            if sequence === compileSequence.contents {
              Signal.set(status, Failed(Browser.jsErrorMessage(obj)))
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
      | Some(id) => Browser.clearTimeout(id)
      | None => ()
      }
      timerId := Some(Browser.setTimeout(compileNow, 280))
    }

    let syncUrlNow = () => {
      UrlState.replaceUrlState(
        Signal.peek(source),
        Signal.peek(compilerVersion),
        Signal.peek(moduleSystem),
        Signal.peek(warnFlags),
        Signal.peek(jsxPreserveMode),
        Signal.peek(experimentalFeatures),
      )->ignore
    }

    let scheduleUrlSync = () => {
      switch urlTimerId.contents {
      | Some(id) => Browser.clearTimeout(id)
      | None => ()
      }
      urlTimerId := Some(Browser.setTimeout(syncUrlNow, 360))
    }

    let showToast = message => {
      switch toastTimerId.contents {
      | Some(id) => Browser.clearTimeout(id)
      | None => ()
      }
      Signal.set(shareToast, Some(message))
      toastTimerId := Some(Browser.setTimeout(() => Signal.set(shareToast, None), 1800))
    }

    let shareCurrentUrl = () => {
      switch urlTimerId.contents {
      | Some(id) => Browser.clearTimeout(id)
      | None => ()
      }

      let share = async () => {
        try {
          let _ = await UrlState.copyUrlState(
            Signal.peek(source),
            Signal.peek(compilerVersion),
            Signal.peek(moduleSystem),
            Signal.peek(warnFlags),
            Signal.peek(jsxPreserveMode),
            Signal.peek(experimentalFeatures),
          )
          showToast("Link copied")
        } catch {
        | JsExn(_) => showToast("Could not copy link")
        | _ => showToast("Could not copy link")
        }
      }

      share()->ignore
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
            let useInitialSettings = firstCompilerLoad.contents
            firstCompilerLoad := false
            Signal.set(compilerInfo, Some(info))
            Signal.set(compilerVersion, info.bundleId)
            Signal.set(moduleSystem, useInitialSettings ? initialModuleSystem : info.moduleSystem)
            Signal.set(warnFlags, useInitialSettings ? initialWarnFlags : info.warnFlags)
            Signal.set(
              jsxPreserveMode,
              useInitialSettings ? initialJsxPreserveMode : info.jsxPreserveMode,
            )
            Signal.set(
              experimentalFeatures,
              useInitialSettings ? initialExperimentalFeatures : info.experimentalFeatures,
            )
            Signal.set(status, Ready)
            if !useInitialSettings {
              scheduleUrlSync()
            }
            if compileAfterLoad {
              compileNow()
            }
          }
        } catch {
        | JsExn(obj) =>
          if sequence === compilerLoadSequence.contents {
            Signal.set(status, Failed(Browser.jsErrorMessage(obj)))
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
        let initialSource = await UrlState.initialSource(defaultSource)
        Signal.set(source, initialSource)
        Signal.set(activeLine, 1)
        Signal.set(editorScrollTop, 0)
        Signal.set(editorScrollLeft, 0)
        loadCompiler(Signal.peek(compilerVersion), true)
      }

      start()->ignore
      None
    })

    Effect.run(() => {
      Browser.configureSourceEditor(syncEditorScroll)
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
                Signal.set(source, Browser.eventValue(event))
                syncEditorState(event)
                scheduleUrlSync()
                scheduleCompile()
              }}
              onClick={syncEditorState}
              onMouseUp={syncEditorState}
              onKeyUp={syncEditorState}
              onFocus={syncEditorState}
              onKeyDown={event =>
                switch Browser.insertTabIndent(event) {
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
            activeTab
            compilerInfo
            compilerVersion
            moduleSystem
            warnFlags
            jsxPreserveMode
            experimentalFeatures
            switchCompiler
            compileNow
            scheduleCompile
            scheduleUrlSync
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
