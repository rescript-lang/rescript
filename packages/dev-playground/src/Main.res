open PlaygroundConfig

type tab =
  | Parsetree
  | Typedtree
  | Lambda
  | JavaScript
  | GenType
  | SourceMap
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

type compileSnapshot = {
  source: string,
  result: CompilerApi.compileResult,
}

let baseTabs: array<tab> = [Parsetree, Typedtree, Lambda, JavaScript]
let moduleSystems: array<moduleSystem> = [Esmodule, Commonjs]
let sourceMapModes: array<sourceMapMode> = [Disabled, Linked, Inline, Hidden]

let tabIsVisible = (config: PlaygroundConfig.t, tab) =>
  switch tab {
  | GenType => config.gentypeEnabled
  | SourceMap => config.sourceMapMode !== Disabled
  | Parsetree | Typedtree | Lambda | JavaScript | Settings => true
  }

let tabsForConfig = (config: PlaygroundConfig.t) => {
  let withGentype = config.gentypeEnabled ? Array.concat(baseTabs, [GenType]) : baseTabs
  let withSourceMap =
    config.sourceMapMode !== Disabled ? Array.concat(withGentype, [SourceMap]) : withGentype
  Array.concat(withSourceMap, [Settings])
}

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
  | JavaScript => "js"
  | GenType => "gentype"
  | SourceMap => "source map"
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

let configureSourceEditor = (editorId, scrollHandler: Dom.event => unit): unit =>
  Window.requestAnimationFrame(() =>
    switch Document.current->Document.getElementById(editorId) {
    | None => ()
    | Some(editor) =>
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

let keyMovesCursor = key =>
  switch key {
  | "ArrowDown"
  | "ArrowLeft"
  | "ArrowRight"
  | "ArrowUp"
  | "End"
  | "Home"
  | "PageDown"
  | "PageUp" => true
  | _ => false
  }

let editorShellStyle = scrollTop => `--editor-scroll-y: -${scrollTop->Int.toString}px;`

let syncSourceOverlayWidth = editor =>
  switch editor->Element.parentElement {
  | Some(editorShell) =>
    editorShell
    ->Element.style
    ->CssStyle.setProperty(
      "--editor-overlay-width",
      `${editor->TextAreaElement.clientWidth->Int.toString}px`,
    )
  | None => ()
  }

let scheduleSourceOverlayWidthSync = editor =>
  Window.requestAnimationFrame(() => syncSourceOverlayWidth(editor))

let updateActiveSourceLine = (editor, line) =>
  Window.requestAnimationFrame(() =>
    switch editor->Element.parentElement {
    | Some(editorShell) => {
        switch editorShell->Element.querySelector(".syntax-line-current") {
        | Some(currentLine) =>
          currentLine->Element.classList->ClassList.remove("syntax-line-current")
        | None => ()
        }
        switch editorShell->Element.querySelector(
          `.syntax-line[data-line="${line->Int.toString}"]`,
        ) {
        | Some(activeLine) => activeLine->Element.classList->ClassList.add("syntax-line-current")
        | None => ()
        }
      }
    | None => ()
    }
  )

let hasFeature = (features: array<experimentalFeature>, feature: experimentalFeature) =>
  features->Array.includes(feature)

let toggleFeature = (features: array<experimentalFeature>, feature: experimentalFeature) =>
  hasFeature(features, feature)
    ? features->Array.filter(item => item !== feature)
    : Array.concat(features, [feature])

let optionalOutput = (output, fallback) =>
  switch output {
  | Some(output) => output
  | None => fallback
  }

let prettyPrintJson = value =>
  try value->JSON.parseOrThrow->JSON.stringify(~space=2) catch {
  | _ => value
  }

let sourceMapDirective = "//# sourceMappingURL="

let offsetForPosition = (source, position: SourceMapNavigation.position) => {
  let index = ref(0)
  let line = ref(1)
  let col = ref(0)
  let length = source->String.length

  while (
    index.contents < length &&
      (line.contents < position.line ||
        (line.contents === position.line && col.contents < position.col))
  ) {
    if source->String.charAt(index.contents) === "\n" {
      line := line.contents + 1
      col := 0
    } else {
      col := col.contents + 1
    }
    index := index.contents + 1
  }

  index.contents
}

let selectedOutput = (snapshot: option<compileSnapshot>, activeTab: tab) =>
  switch snapshot {
  | None => "The compiler is loading. Results will appear here after the first compile."
  | Some({result: Error(result)}) =>
    let errors = result.errors->Array.join("\n")
    errors === "" ? result.message : errors
  | Some({result: Ok(result)}) =>
    switch activeTab {
    | Parsetree => result.parsetree
    | Typedtree => result.typedtree
    | Lambda => result.lambda
    | JavaScript => result.jsCode
    | GenType =>
      optionalOutput(result.gentype, "This compiler bundle does not expose gentype output yet.")
    | SourceMap =>
      result.sourceMap
      ->optionalOutput("This compiler bundle does not expose source map output yet.")
      ->prettyPrintJson
    | Settings => ""
    }
  }

let outputNode = (output, activeTab, onSourceMapSelect): View.node => {
  let directiveIndex = output->String.indexOf(sourceMapDirective)
  if activeTab !== JavaScript || directiveIndex < 0 {
    View.text(output)
  } else {
    let directiveEnd = directiveIndex + sourceMapDirective->String.length
    View.fragment([
      View.text(output->String.slice(~start=0, ~end=directiveIndex)),
      <a
        class="source-map-output-link"
        href="#source-map"
        title="View decoded source map"
        onClick={event => {
          event->Event.preventDefault
          onSourceMapSelect()
        }}
      >
        {View.text(sourceMapDirective)}
      </a>,
      View.text(output->String.slice(~start=directiveEnd)),
    ])
  }
}

let pushOutputText = (nodes: array<View.node>, text, onSourceMapSelect) => {
  let directiveIndex = text->String.indexOf(sourceMapDirective)
  if directiveIndex < 0 {
    nodes->Array.push(View.text(text))
  } else {
    let directiveEnd = directiveIndex + sourceMapDirective->String.length
    nodes->Array.push(View.text(text->String.slice(~start=0, ~end=directiveIndex)))
    nodes->Array.push(
      <a
        class="source-map-output-link"
        href="#source-map"
        title="View decoded source map"
        onClick={event => {
          event->Event.preventDefault
          onSourceMapSelect()
        }}
      >
        {View.text(sourceMapDirective)}
      </a>,
    )
    nodes->Array.push(View.text(text->String.slice(~start=directiveEnd)))
  }
}

let mappedJavaScriptNode = (
  output,
  mappings: array<SourceMapNavigation.mapping>,
  selectedPosition: option<SourceMapNavigation.position>,
  onMappingSelect,
  onSourceMapSelect,
): View.node => {
  let nodes: array<View.node> = []
  let lines = output->String.split("\n")
  let mappingsByLine = SourceMapNavigation.groupByGeneratedLine(mappings, lines->Array.length)
  lines->Array.forEachWithIndex((lineText, lineIndex) => {
    let lineLength = lineText->String.length
    let lineMappings = switch mappingsByLine->Array.get(lineIndex) {
    | Some(lineMappings) => lineMappings
    | None => []
    }
    let cursor = ref(0)

    lineMappings->Array.forEachWithIndex((mapping, mappingIndex) => {
      let start = Math.Int.max(0, Math.Int.min(mapping.generated.col, lineLength))
      if start > cursor.contents {
        pushOutputText(
          nodes,
          lineText->String.slice(~start=cursor.contents, ~end=start),
          onSourceMapSelect,
        )
      }

      let nextColumn = switch lineMappings->Array.get(mappingIndex + 1) {
      | Some(nextMapping) => nextMapping.generated.col
      | None => lineLength
      }
      let end_ = Math.Int.max(start, Math.Int.min(nextColumn, lineLength))
      if end_ > start {
        let text = lineText->String.slice(~start, ~end=end_)
        switch mapping.original {
        | Some(original) => {
            let isSelected = switch selectedPosition {
            | Some(position) =>
              position.line === mapping.generated.line && position.col === mapping.generated.col
            | None => false
            }
            let className = isSelected
              ? "source-map-mapped-segment source-map-mapped-segment-active"
              : "source-map-mapped-segment"
            let title = `${original.source}:${original.position.line->Int.toString}:${(original.position.col + 1)
                ->Int.toString} — click to reveal in source`
            nodes->Array.push(
              <span
                id={isSelected ? "generated-map-selection" : ""}
                class={className}
                title
                onClick={_ => {
                  let shouldNavigate = switch WindowSelection.get() {
                  | Some(selection) => selection->WindowSelection.isCollapsed
                  | None => true
                  }
                  if shouldNavigate {
                    onMappingSelect(mapping)
                  }
                }}
              >
                {View.text(text)}
              </span>,
            )
          }
        | None => pushOutputText(nodes, text, onSourceMapSelect)
        }
      }
      cursor := Math.Int.max(cursor.contents, end_)
    })

    if cursor.contents < lineLength {
      pushOutputText(nodes, lineText->String.slice(~start=cursor.contents), onSourceMapSelect)
    }
    if lineIndex < lines->Array.length - 1 {
      nodes->Array.push(View.text("\n"))
    }
  })
  View.fragment(nodes)
}

let interactiveOutputNode = (
  snapshot: option<compileSnapshot>,
  currentSource,
  activeTab,
  selectedPosition,
  onMappingSelect,
  onSourceMapSelect,
) => {
  let output = selectedOutput(snapshot, activeTab)
  switch (snapshot, activeTab) {
  | (Some({source: compiledSource, result: Ok({sourceMap: Some(sourceMap)})}), JavaScript) => {
      let mappings = SourceMapNavigation.decodeForSource(sourceMap, compiledSource, currentSource)
      mappings->Array.length > 0
        ? mappedJavaScriptNode(
            output,
            mappings,
            selectedPosition,
            onMappingSelect,
            onSourceMapSelect,
          )
        : outputNode(output, activeTab, onSourceMapSelect)
    }
  | _ => outputNode(output, activeTab, onSourceMapSelect)
  }
}

let resultSummary = (snapshot: option<compileSnapshot>) =>
  switch snapshot {
  | None => "No compile result yet"
  | Some({result: Ok(result)}) =>
    let warningCount = result.warnings->Array.length
    let warningText = warningCount === 0 ? "no warnings" : `${warningCount->Int.toString} warnings`
    `Compiled in ${result.time->Float.toFixed(~digits=1)}ms with ${warningText}`
  | Some({result: Error(result)}) => result.message
  }

module TabButton = {
  @jsx.component
  let make = (~tab, ~activeTab: Signal.t<tab>, ~onSelect: tab => unit) => {
    <button
      class={() => Signal.get(activeTab) === tab ? "tab-button tab-button-active" : "tab-button"}
      onClick={_ => onSelect(tab)}
    >
      {View.text(tabLabel(tab))}
    </button>
  }
}

module Problems = {
  @jsx.component
  let make = (~compileResult: Signal.t<option<compileSnapshot>>) => {
    <div class="problems">
      <div class="problems-title"> {View.text("Problems")} </div>
      <pre class="problems-output">
        {View.signalText(() =>
          switch Signal.get(compileResult) {
          | Some({result: Ok({warnings})}) if warnings->Array.length > 0 =>
            warnings->Array.join("\n")
          | Some({result: Error({warnings})}) if warnings->Array.length > 0 =>
            warnings->Array.join("\n")
          | Some({result: Error({errors})}) if errors->Array.length > 0 => errors->Array.join("\n")
          | Some({result: Error({message})}) => message
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
    let updateConfig = f => {
      let nextConfig = f(Signal.peek(config))
      Signal.set(config, nextConfig)
      if !tabIsVisible(nextConfig, Signal.peek(activeTab)) {
        Signal.set(activeTab, JavaScript)
      }
    }
    let compilerVersionOptions: Signal.t<array<View.node>> = Obj.magic(
      Computed.make(() =>
        CompilerApi.selectableCompilerVersions(
          Signal.get(config).compilerVersion,
        )->Array.map(version => <option value=version.id> {View.text(version.label)} </option>)
      ),
    )

    <div
      class={() =>
        Signal.get(activeTab) === Settings ? "settings-panel" : "settings-panel hidden-panel"}
    >
      <section class="settings-section">
        <label class="setting-label" for_="compiler-version">
          {View.text("Compiler Version")}
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
          {View.signalFragment(compilerVersionOptions)}
        </select>
      </section>
      <section class="settings-section">
        <label class="setting-label"> {View.text("Loaded Compiler")} </label>
        <div class="setting-value">
          {View.signalText(() =>
            switch Signal.get(compilerInfo) {
            | Some(info) => `${info.version} / API ${info.apiVersion} / ${info.bundleId}`
            | None => "loading"
            }
          )}
        </div>
      </section>
      <section class="settings-section">
        <label class="setting-label" for_="module-system"> {View.text("Module System")} </label>
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
          {View.fragment(
            moduleSystems->Array.map(moduleSystem => {
              let value = (moduleSystem :> string)
              <option value> {View.text(value)} </option>
            }),
          )}
        </select>
      </section>
      <section class="settings-section">
        <label class="setting-label" for_="warning-flags"> {View.text("Warning Flags")} </label>
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
          {View.text("Reset")}
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
        <label for_="jsx-preserve"> {View.text("Preserve JSX output")} </label>
      </section>
      <section class="settings-section setting-row">
        <input
          id="gentype-enabled"
          type_="checkbox"
          checked={() => Signal.get(config).gentypeEnabled}
          onChange={event => {
            updateConfig(config => {...config, gentypeEnabled: Event.checked(event)})
            scheduleUrlSync()
            compileNow()
          }}
        />
        <label for_="gentype-enabled"> {View.text("gentype")} </label>
      </section>
      <section class="settings-section source-map-settings">
        <div class="setting-label"> {View.text("Source Map")} </div>
        <div class="source-map-controls">
          <div class="source-map-control">
            <label for_="source-map-mode"> {View.text("Mode")} </label>
            <select
              id="source-map-mode"
              value={() => (Signal.get(config).sourceMapMode :> string)}
              onChange={event => {
                switch event->Event.value->parseSourceMapMode {
                | Some(nextSourceMapMode) =>
                  updateConfig(config => {...config, sourceMapMode: nextSourceMapMode})
                  scheduleUrlSync()
                  compileNow()
                | None => ()
                }
              }}
            >
              {View.fragment(
                sourceMapModes->Array.map(sourceMapMode => {
                  let value = (sourceMapMode :> string)
                  <option value> {View.text(value)} </option>
                }),
              )}
            </select>
          </div>
          <div
            class={() =>
              Signal.get(config).sourceMapMode === Disabled
                ? "source-map-options source-map-options-disabled"
                : "source-map-options"}
          >
            <label class="source-map-checkbox" for_="source-map-sources-content">
              <input
                id="source-map-sources-content"
                type_="checkbox"
                disabled={() => Signal.get(config).sourceMapMode === Disabled}
                checked={() => Signal.get(config).sourceMapSourcesContent}
                onChange={event => {
                  updateConfig(config => {
                    ...config,
                    sourceMapSourcesContent: Event.checked(event),
                  })
                  scheduleUrlSync()
                  compileNow()
                }}
              />
              {View.text("Include sources content")}
            </label>
            <div class="source-map-control">
              <label for_="source-map-root"> {View.text("Source Root")} </label>
              <input
                id="source-map-root"
                disabled={() => Signal.get(config).sourceMapMode === Disabled}
                value={() => Signal.get(config).sourceMapRoot}
                spellcheck=false
                onInput={event => {
                  updateConfig(config => {...config, sourceMapRoot: Event.value(event)})
                  scheduleUrlSync()
                  scheduleCompile()
                }}
              />
            </div>
          </div>
        </div>
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
        <label for_="feature-let-unwrap"> {View.text("Experimental: let?")} </label>
      </section>
      <section class="settings-section">
        <label class="setting-label"> {View.text("Loaded Libraries")} </label>
        <div class="setting-value">
          {View.signalText(() =>
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
      {View.signalText(() =>
        switch Signal.get(status) {
        | Failed(message) => message
        | other => statusLabel(other)
        }
      )}
    </div>
  }
}

module PaneSeparator = {
  let minSourceColumn = 360.0
  let minResultColumn = 420.0
  let minSourceRow = 180.0
  let minResultRow = 180.0

  let metrics = (orientation, rect: boundingRect) =>
    switch orientation {
    | PaneLayout.Columns => (rect.width, minSourceColumn, minResultColumn)
    | PaneLayout.Rows => (rect.height, minSourceRow, minResultRow)
    }

  let position = (orientation, event, rect: boundingRect) =>
    switch orientation {
    | PaneLayout.Columns => event->Event.clientX -. rect.left
    | PaneLayout.Rows => event->Event.clientY -. rect.top
    }

  let withContainer = (event, callback) =>
    switch event->Event.currentTarget->Element.parentElement {
    | Some(container) => callback(event->Event.currentTarget, container)
    | None => ()
    }

  @jsx.component
  let make = (~layout: PaneLayout.t) => {
    let beginDrag = event =>
      if event->Event.button === 0 {
        withContainer(event, (separator, container) => {
          let rect = container->Element.getBoundingClientRect
          let orientation = PaneLayout.orientationForWidth(rect.width)
          let (size, minFirst, minSecond) = metrics(orientation, rect)
          let pointerId = event->Event.pointerId

          PaneLayout.setOrientation(layout, orientation)
          separator->Element.setPointerCapture(pointerId)
          PaneLayout.startDrag(
            layout,
            ~pointerId,
            ~position=position(orientation, event, rect),
            ~size,
            ~minFirst,
            ~minSecond,
          )
          event->Event.preventDefault
        })
      }

    let moveDrag = event =>
      withContainer(event, (_, container) => {
        let rect = container->Element.getBoundingClientRect
        let orientation = PaneLayout.current(layout).orientation
        PaneLayout.moveDrag(
          layout,
          ~pointerId=event->Event.pointerId,
          ~position=position(orientation, event, rect),
        )
      })

    let finishDrag = event => {
      let separator = event->Event.currentTarget
      let pointerId = event->Event.pointerId
      if separator->Element.hasPointerCapture(pointerId) {
        separator->Element.releasePointerCapture(pointerId)
      }
      PaneLayout.finishDrag(layout, ~pointerId)
    }

    let handleKeyDown = event =>
      withContainer(event, (_, container) => {
        let rect = container->Element.getBoundingClientRect
        let orientation = PaneLayout.orientationForWidth(rect.width)
        let delta = switch (orientation, event->Event.key) {
        | (PaneLayout.Columns, "ArrowLeft") | (PaneLayout.Rows, "ArrowUp") =>
          Some(-.PaneLayout.keyboardStep)
        | (PaneLayout.Columns, "ArrowRight") | (PaneLayout.Rows, "ArrowDown") =>
          Some(PaneLayout.keyboardStep)
        | _ => None
        }

        switch delta {
        | Some(delta) => {
            let (size, minFirst, minSecond) = metrics(orientation, rect)
            PaneLayout.setOrientation(layout, orientation)
            PaneLayout.nudge(layout, delta, ~size, ~minFirst, ~minSecond)
            event->Event.preventDefault
          }
        | None => ()
        }
      })

    <div
      class="pane-separator"
      role="separator"
      tabIndex=0
      ariaLabel="Resize source and output panes"
      attrs={[
        ("aria-orientation", () => PaneLayout.ariaOrientation(Signal.get(layout.state))),
        ("aria-valuemin", () => "0"),
        ("aria-valuemax", () => "100"),
        ("aria-valuenow", () => PaneLayout.ariaValueNow(Signal.get(layout.state))),
      ]}
      onPointerDown={beginDrag}
      onPointerMove={moveDrag}
      onPointerUp={finishDrag}
      onPointerCancel={finishDrag}
      onLostPointerCapture={event =>
        PaneLayout.finishDrag(layout, ~pointerId=event->Event.pointerId)}
      onClick={event =>
        if event->Event.detail === 2 {
          PaneLayout.reset(layout)
          event->Event.preventDefault
        }}
      onKeyDown={handleKeyDown}
    />
  }
}

module App = {
  @jsx.component
  let make = () => {
    let source = Signal.make(defaultSource)
    let activeTab = Signal.make(JavaScript)
    let mappedSourcePosition: Signal.t<option<SourceMapNavigation.position>> = Signal.make(None)
    let mappedGeneratedPosition: Signal.t<option<SourceMapNavigation.position>> = Signal.make(None)
    let status = Signal.make(Loading)
    let compilerInfo: Signal.t<option<CompilerApi.info>> = Signal.make(None)
    let compileResult: Signal.t<option<compileSnapshot>> = Signal.make(None)
    let config = Signal.make(CompilerApi.defaultConfig)
    let visibleTabNodes: Signal.t<array<View.node>> = Obj.magic(
      Computed.make(() =>
        tabsForConfig(Signal.get(config))->Array.map(tab =>
          <TabButton tab activeTab onSelect={tab => Signal.set(activeTab, tab)} />
        )
      ),
    )
    let activeLine = Signal.make(1)
    let editorScrollTop = Signal.make(0)
    let highlightedSource: Signal.t<array<View.node>> = Obj.magic(
      Computed.make(() =>
        SourceHighlight.render(Signal.get(source), ~activeLine=Signal.peek(activeLine))
      ),
    )
    let timerId: ref<option<int>> = ref(None)
    let urlTimerId: ref<option<int>> = ref(None)
    let toastTimerId: ref<option<int>> = ref(None)
    let firstLoadConfig: ref<option<PlaygroundConfig.t>> = ref(None)
    let compilerLoadSequence = ref(0)
    let compileSequence = ref(0)
    let shareToast: Signal.t<option<string>> = Signal.make(None)
    let paneLayout = PaneLayout.make()
    let sourceEditorId = paneLayout.containerId ++ "-source-editor"

    let clearMappedPositions = () => {
      Signal.set(mappedSourcePosition, None)
      Signal.set(mappedGeneratedPosition, None)
    }

    let syncEditorState = event => {
      let currentSource = Event.value(event)
      let cursorPosition = cursorPositionForOffset(currentSource, Event.selectionStart(event))

      Signal.set(editorScrollTop, Event.scrollTop(event))
      Signal.set(activeLine, cursorPosition.line)
      updateActiveSourceLine(event->Event.currentTarget, cursorPosition.line)
    }

    let syncEditorScroll = event => {
      Signal.set(editorScrollTop, Event.scrollTop(event))
    }

    let scrollToGeneratedMapping = () =>
      Window.requestAnimationFrame(() =>
        switch Document.current->Document.getElementById("generated-map-selection") {
        | Some(element) => element->Element.scrollIntoView({block: "center", inline: "nearest"})
        | None => ()
        }
      )

    let revealOriginalMapping = (mapping: SourceMapNavigation.mapping) =>
      switch Signal.peek(compileResult) {
      | Some({source: compiledSource})
        if SourceMapNavigation.isCurrentSource(compiledSource, Signal.peek(source)) =>
        switch mapping.original {
        | Some(original) => {
            Signal.set(mappedSourcePosition, Some(original.position))
            Signal.set(mappedGeneratedPosition, Some(mapping.generated))
            Signal.set(activeLine, original.position.line)
            Window.requestAnimationFrame(() =>
              switch Document.current->Document.getElementById(sourceEditorId) {
              | Some(editor) => {
                  let offset = offsetForPosition(Signal.peek(source), original.position)
                  editor->TextAreaElement.setSelectionRange(offset, offset)
                  editor->Element.focus
                  Signal.set(activeLine, original.position.line)
                  updateActiveSourceLine(editor, original.position.line)
                  switch editor->Element.parentElement {
                  | Some(editorShell) =>
                    switch editorShell->Element.querySelector(
                      `.syntax-line[data-line="${original.position.line->Int.toString}"]`,
                    ) {
                    | Some(line) => {
                        let editorRect = editor->Element.getBoundingClientRect
                        let lineRect = line->Element.getBoundingClientRect
                        let centeredScrollTop =
                          Signal.peek(editorScrollTop)->Int.toFloat +.
                          lineRect.top -.
                          editorRect.top -.
                          (editor->TextAreaElement.clientHeight->Int.toFloat -.
                            lineRect.height) /. 2.0
                        let scrollTop = Math.Int.max(0, centeredScrollTop->Math.round->Float.toInt)
                        editor->TextAreaElement.setScrollTop(scrollTop)
                        Signal.set(editorScrollTop, editor->TextAreaElement.scrollTop)
                      }
                    | None => ()
                    }
                  | None => ()
                  }
                }
              | None => ()
              }
            )
          }
        | None => ()
        }
      | _ => clearMappedPositions()
      }

    let navigateFromSource = event => {
      syncEditorState(event)
      let selectionStart = Event.selectionStart(event)
      if !SourceMapNavigation.isCollapsedSelection(selectionStart, Event.selectionEnd(event)) {
        clearMappedPositions()
      } else {
        let currentSource = Event.value(event)
        let position = cursorPositionForOffset(currentSource, selectionStart)
        switch Signal.peek(compileResult) {
        | Some({source: compiledSource, result: Ok({sourceMap: Some(sourceMap)})}) => {
            let mappings = SourceMapNavigation.decodeForSource(
              sourceMap,
              compiledSource,
              currentSource,
            )
            switch SourceMapNavigation.generatedForOriginal(
              mappings,
              {
                line: position.line,
                col: position.col,
              },
            ) {
            | Some(mapping) => {
                Signal.set(mappedSourcePosition, Some({line: position.line, col: position.col}))
                Signal.set(mappedGeneratedPosition, Some(mapping.generated))
                Signal.set(activeTab, JavaScript)
                scrollToGeneratedMapping()
              }
            | None => clearMappedPositions()
            }
          }
        | _ => clearMappedPositions()
        }
      }
    }

    let compileNow = () => {
      compileSequence := compileSequence.contents + 1
      let sequence = compileSequence.contents
      let sourceToCompile = Signal.peek(source)

      let run = async () => {
        switch Signal.peek(status) {
        | Loading => ()
        | Failed(_) => ()
        | Ready | Compiling =>
          Signal.set(status, Compiling)
          try {
            let result = await CompilerApi.compile(sourceToCompile, Signal.peek(config))
            if sequence === compileSequence.contents {
              clearMappedPositions()
              Signal.set(compileResult, Some({source: sourceToCompile, result}))
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
                  Signal.set(activeLine, 1)
                  Signal.set(source, formattedSource)
                  clearMappedPositions()
                  Signal.set(editorScrollTop, 0)
                  scheduleUrlSync()
                  Signal.set(status, Ready)
                  compileNow()
                } else {
                  Signal.set(status, Ready)
                }
              }
            | Error(failure) =>
              if sequence === compileSequence.contents {
                Signal.set(
                  compileResult,
                  Some({source: sourceBeforeFormat, result: Error(failure)}),
                )
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
                gentypeEnabled: info.gentypeEnabled,
                sourceMapMode: info.sourceMapMode,
                sourceMapSourcesContent: info.sourceMapSourcesContent,
                sourceMapRoot: info.sourceMapRoot,
              }
            }
            Signal.set(compilerInfo, Some(info))
            Signal.set(config, nextConfig)
            if !tabIsVisible(nextConfig, Signal.peek(activeTab)) {
              Signal.set(activeTab, JavaScript)
            }
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
        Signal.set(activeLine, 1)
        Signal.set(source, urlState.source)
        Signal.set(config, urlState.config)
        Signal.set(editorScrollTop, 0)
        firstLoadConfig := Some(urlState.config)
        loadCompiler(urlState.config.compilerVersion, true)
      }

      start()->ignore
      None
    })

    Effect.run(() => {
      configureSourceEditor(sourceEditorId, syncEditorScroll)
      None
    })

    Effect.run(() => {
      let disposed = ref(false)
      let observer: ref<option<resizeObserver>> = ref(None)

      Window.requestAnimationFrame(() =>
        if !disposed.contents {
          switch Document.current->Document.getElementById(sourceEditorId) {
          | Some(editor) => {
              syncSourceOverlayWidth(editor)
              switch ResizeObserver.supported {
              | Some(_) => {
                  let nextObserver = ResizeObserver.make(_ => syncSourceOverlayWidth(editor))
                  observer := Some(nextObserver)
                  nextObserver->ResizeObserver.observe(editor)
                }
              | None => ()
              }
            }
          | None => ()
          }
        }
      )

      Some(
        () => {
          disposed := true
          switch observer.contents {
          | Some(observer) => observer->ResizeObserver.disconnect
          | None => ()
          }
        },
      )
    })

    Effect.run(() => {
      let disposed = ref(false)
      let observer: ref<option<resizeObserver>> = ref(None)

      Window.requestAnimationFrame(() =>
        if !disposed.contents {
          switch Document.current->Document.getElementById(paneLayout.containerId) {
          | Some(container) => {
              let updateOrientation = width =>
                PaneLayout.setOrientation(paneLayout, PaneLayout.orientationForWidth(width))

              updateOrientation((container->Element.getBoundingClientRect).width)
              switch ResizeObserver.supported {
              | Some(_) => {
                  let nextObserver = ResizeObserver.make(
                    entries =>
                      switch entries->Array.get(0) {
                      | Some(entry) =>
                        updateOrientation((entry->ResizeObserverEntry.contentRect).width)
                      | None => ()
                      },
                  )
                  observer := Some(nextObserver)
                  nextObserver->ResizeObserver.observe(container)
                }
              | None => ()
              }
            }
          | None => ()
          }
        }
      )

      Some(
        () => {
          disposed := true
          switch observer.contents {
          | Some(observer) => observer->ResizeObserver.disconnect
          | None => ()
          }
        },
      )
    })

    <main class="app-shell">
      <header class="topbar">
        <div>
          <h1> {View.text("ReScript Developer Playground")} </h1>
        </div>
        <StatusBadge status />
      </header>
      <section
        id={paneLayout.containerId}
        class={() => PaneLayout.workspaceClass(Signal.get(paneLayout.state))}
        style={() => PaneLayout.workspaceStyle(Signal.get(paneLayout.state))}
      >
        <div class="source-column">
          <div class="column-header">
            <h2> {View.text("Source")} </h2>
            <div class="actions">
              <button class="secondary-action" onClick={_ => formatSource()}>
                {View.text("Format")}
              </button>
              <button
                class="secondary-action"
                onClick={_ => {
                  Signal.set(activeLine, 1)
                  Signal.set(source, defaultSource)
                  clearMappedPositions()
                  Signal.set(editorScrollTop, 0)
                  scheduleUrlSync()
                  scheduleCompile()
                }}
              >
                {View.text("Reset")}
              </button>
              <button class="secondary-action" onClick={_ => shareCurrentUrl()}>
                {View.text("Share")}
              </button>
            </div>
          </div>
          <div
            class={() =>
              switch Signal.get(mappedSourcePosition) {
              | Some(_) => "editor-shell source-map-source-active"
              | None => "editor-shell"
              }}
            style={() => editorShellStyle(Signal.get(editorScrollTop))}
          >
            <div class="line-number-gutter" ariaHidden=true />
            <pre class="syntax-layer" ariaHidden=true>
              {View.signalFragment(highlightedSource)}
            </pre>
            <textarea
              id={sourceEditorId}
              class="editor"
              value={MaybeSignal.reactive(source)}
              spellcheck=false
              attrs={[("wrap", "soft")]}
              onInput={event => {
                scheduleSourceOverlayWidthSync(event->Event.currentTarget)
                Signal.set(source, Event.value(event))
                clearMappedPositions()
                syncEditorState(event)
                scheduleUrlSync()
                scheduleCompile()
              }}
              onClick={navigateFromSource}
              onMouseUp={syncEditorState}
              onKeyUp={event => {
                if event->Event.key->keyMovesCursor {
                  navigateFromSource(event)
                } else {
                  syncEditorState(event)
                }
              }}
              onFocus={syncEditorState}
              onKeyDown={event =>
                switch insertTabIndent(event) {
                | Some(nextSource) =>
                  Signal.set(source, nextSource)
                  clearMappedPositions()
                  syncEditorState(event)
                  scheduleUrlSync()
                  scheduleCompile()
                | None => ()
                }}
            />
          </div>
        </div>
        <PaneSeparator layout={paneLayout} />
        <div class="result-column">
          <div class="tabs"> {View.signalFragment(visibleTabNodes)} </div>
          <div
            class={() =>
              Signal.get(activeTab) === Settings ? "output-panel hidden-panel" : "output-panel"}
          >
            <div class="result-meta">
              {View.signalText(() => resultSummary(Signal.get(compileResult)))}
            </div>
            <div class="output-shell">
              <pre class="output">
                {View.tracked(() => {
                  let selectedTab = Signal.get(activeTab)
                  interactiveOutputNode(
                    Signal.get(compileResult),
                    Signal.get(source),
                    selectedTab,
                    Signal.get(mappedGeneratedPosition),
                    revealOriginalMapping,
                    () => Signal.set(activeTab, SourceMap),
                  )
                })}
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
        {View.signalText(() =>
          switch Signal.get(shareToast) {
          | Some(message) => message
          | None => ""
          }
        )}
      </div>
    </main>
  }
}

View.mountById(<App />, "app")
