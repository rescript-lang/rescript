type mode = Before | After | Diff
type input = {before: string, after: option<string>}

let sameInput = (left, right) =>
  switch (left, right) {
  | (None, None) => true
  | (Some(left), Some(right)) => left.before === right.before && left.after === right.after
  | (None, Some(_)) | (Some(_), None) => false
  }

let label = mode =>
  switch mode {
  | Before => "Before optimization"
  | After => "After optimization"
  | Diff => "Diff"
  }

let renderLine = (highlights, line: LambdaDiff.line) => {
  let (className, marker) = switch line.kind {
  | Same => ("lambda-diff-line", " ")
  | Removed => ("lambda-diff-line lambda-diff-removed", "−")
  | Added => ("lambda-diff-line lambda-diff-added", "+")
  }
  let number = number => number->Option.map(value => Int.toString(value))->Option.getOr("")
  <span class={className}>
    <span class="lambda-diff-number" ariaHidden=true>{View.text(number(line.before))}</span>
    <span class="lambda-diff-number" ariaHidden=true>{View.text(number(line.after))}</span>
    <span class="lambda-diff-marker">{View.text(marker)}</span>
    <span class="lambda-diff-text">
      {switch LambdaInlineDiff.forLine(highlights, line) {
      | None => View.text(line.text)
      | Some(parts) =>
        View.fragment(
          parts->Array.map(part =>
            part.changed
              ? <span class="lambda-diff-token">{View.text(part.text)}</span>
              : View.text(part.text)
          ),
        )
      }}
      {View.text("\n")}
    </span>
  </span>
}

let renderLines = (highlights, lines) =>
  View.fragment(Array.map(lines, line => renderLine(highlights, line)))

let renderDiff = (contextState, result) =>
  switch result {
  | LambdaDiff.Identical =>
    <p class="lambda-notice">{View.text("No changes in the printed Lambda after optimization.")}</p>
  | TooLarge =>
    <p class="lambda-notice">
      {View.text(
        "This Lambda comparison exceeds the diff work limit. Use Before optimization and After optimization to inspect the full dumps.",
      )}
    </p>
  | Changes(lines) =>
    let highlights = LambdaInlineDiff.highlight(lines)
    let previous = contextState.contents
    let current = Dict.make()
    let occurrences = Dict.make()
    let sections = LambdaDiff.sections(lines)->Array.map(section =>
      switch section {
      | Visible(lines) => <pre class="lambda-diff-block">{renderLines(highlights, lines)}</pre>
      | Collapsed(lines) =>
        let text = lines->Array.map(line => line.text)->Array.join("\n")
        let occurrence = Dict.get(occurrences, text)->Option.getOr(0)
        Dict.set(occurrences, text, occurrence + 1)
        let key = `${occurrence->Int.toString}:${text}`
        let expanded = switch Dict.get(previous, key) {
        | Some(expanded) => expanded
        | None => Signal.make(false)
        }
        Dict.set(current, key, expanded)
        <details
          class="lambda-diff-context"
          attrs={[View.optionalComputedAttr("open", () => Signal.get(expanded) ? Some("") : None)]}
        >
          <summary
            onClick={event => {
              event->Event.preventDefault
              Signal.update(expanded, open_ => !open_)
            }}
          >
            {View.text(`Show ${Array.length(lines)->Int.toString} unchanged lines`)}
          </summary>
          {View.tracked(() =>
            Signal.get(expanded)
              ? <pre class="lambda-diff-block">{renderLines(highlights, lines)}</pre>
              : View.empty()
          )}
        </details>
      }
    )
    // Retain expansion only for regions still present in this comparison.
    contextState := current
    <div class="lambda-diff">
      <p class="lambda-notice">
        {View.text(
          "− removed from Before · + added in After. Stronger shading marks changed tokens in similar lines. Line numbers show Before / After.",
        )}
      </p>
      {View.fragment(sections)}
    </div>
  }

@jsx.component
let make = (~input: unit => option<input>, ~visible, ~mode: Signal.t<mode>) => {
  let contextState = ref(Dict.make())
  let unavailable = () =>
    <p class="lambda-notice">
      {View.text(
        "This compiler bundle does not provide optimized Lambda. Select a compiler with playground API v9 or newer.",
      )}
    </p>
  let shownInput = ref(None)
  let panelInput = Computed.make(() => {
    if visible() {
      shownInput := input()
    }
    shownInput.contents
  })
  let diffInput = Signal.make(None)
  let wasActive = ref(false)
  Effect.run(() => {
    let active = visible() && Signal.get(mode) === Diff
    let entering = active && !wasActive.contents
    wasActive := active
    if active {
      let current = input()
      if entering {
        // Opening Diff is immediate; edits wait for a short pause in typing.
        Signal.set(diffInput, current)
        None
      } else if sameInput(Signal.peek(diffInput), current) {
        None
      } else {
        let timer = Window.setTimeout(() => Signal.set(diffInput, current), 200)
        Some(() => Window.clearTimeout(timer))
      }
    } else {
      None
    }
  })
  // Cache the complete rendered diff by text, including inline highlights and
  // sections. Hidden tabs keep their last result and defer new comparison work.
  let cached = ref((None, View.empty()))
  let difference = Computed.make(() => {
    let (previous, rendered) = cached.contents
    let current = Signal.get(diffInput)
    if sameInput(previous, current) {
      rendered
    } else {
      let rendered = switch current {
      | Some({before, after: Some(after)}) =>
        renderDiff(contextState, LambdaDiff.compare(before, after))
      | Some({after: None}) => unavailable()
      | None => View.empty()
      }
      cached := (current, rendered)
      rendered
    }
  })
  <div class={() => visible() ? "lambda-view" : "lambda-view hidden-panel"}>
    <div class="lambda-subtabs" role="group" ariaLabel="Lambda stage">
      {View.fragment(
        [Before, After, Diff]->Array.map(tab =>
          <button
            class={() =>
              Signal.get(mode) === tab ? "lambda-subtab lambda-subtab-active" : "lambda-subtab"}
            attrs={[("aria-pressed", () => Signal.get(mode) === tab ? "true" : "false")]}
            onClick={_ => Signal.set(mode, tab)}
          >
            {View.text(label(tab))}
          </button>
        ),
      )}
    </div>
    <pre class={() => Signal.get(mode) === Before ? "output" : "output hidden-panel"}>
      {View.signalText(() =>
        Signal.get(panelInput)->Option.map(value => value.before)->Option.getOr("")
      )}
    </pre>
    <div class={() => Signal.get(mode) === After ? "" : "hidden-panel"}>
      {View.tracked(() =>
        switch Signal.get(panelInput)->Option.flatMap(value => value.after) {
        | Some(after) => <pre class="output">{View.text(after)}</pre>
        | None => unavailable()
        }
      )}
    </div>
    <div class={() => Signal.get(mode) === Diff ? "" : "hidden-panel"}>
      {View.tracked(() => {
        // Reuse the DOM too when reactive dependencies are reevaluated.
        let child = Signal.get(difference)
        View.Keyed({key: "lambda-diff", identity: Obj.magic(child), child})
      })}
    </div>
  </div>
}
