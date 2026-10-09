type mode = Before | After | Diff

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
    <span class="lambda-diff-number" ariaHidden=true> {View.text(number(line.before))} </span>
    <span class="lambda-diff-number" ariaHidden=true> {View.text(number(line.after))} </span>
    <span class="lambda-diff-marker"> {View.text(marker)} </span>
    <span class="lambda-diff-text">
      {switch LambdaInlineDiff.forLine(highlights, line) {
      | None => View.text(line.text)
      | Some(parts) =>
        View.fragment(
          parts->Array.map(part =>
            part.changed
              ? <span class="lambda-diff-token"> {View.text(part.text)} </span>
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

let renderDiff = result =>
  switch result {
  | LambdaDiff.Identical =>
    <p class="lambda-notice">
      {View.text("No changes in the printed Lambda after optimization.")}
    </p>
  | TooLarge =>
    <p class="lambda-notice">
      {View.text(
        "This Lambda comparison exceeds the diff work limit. Use Before optimization and After optimization to inspect the full dumps.",
      )}
    </p>
  | Changes(lines) =>
    let highlights = LambdaInlineDiff.highlight(lines)
    <div class="lambda-diff">
      <p class="lambda-notice">
        {View.text(
          "− removed from Before · + added in After. Stronger shading marks changed tokens in similar lines. Line numbers show Before / After.",
        )}
      </p>
      {View.fragment(
        LambdaDiff.sections(lines)->Array.map(section =>
          switch section {
          | Visible(lines) =>
            <pre class="lambda-diff-block"> {renderLines(highlights, lines)} </pre>
          | Collapsed(lines) =>
            <details class="lambda-diff-context">
              <summary>
                {View.text(`Show ${Array.length(lines)->Int.toString} unchanged lines`)}
              </summary>
              <pre class="lambda-diff-block"> {renderLines(highlights, lines)} </pre>
            </details>
          }
        ),
      )}
    </div>
  }

@jsx.component
let make = (~before, ~after: option<string>, ~mode: Signal.t<mode>) => {
  let difference = Lazy.make(() =>
    switch after {
    | Some(after) => LambdaDiff.compare(before, after)
    | None => LambdaDiff.Identical
    }
  )
  <div class="lambda-view">
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
    {View.tracked(() =>
      switch (Signal.get(mode), after) {
      | (Before, _) => <pre class="output"> {View.text(before)} </pre>
      | (After | Diff, None) =>
        <p class="lambda-notice">
          {View.text(
            "This compiler bundle does not provide optimized Lambda. Select a compiler with playground API v9 or newer.",
          )}
        </p>
      | (After, Some(after)) => <pre class="output"> {View.text(after)} </pre>
      | (Diff, Some(_)) => renderDiff(Lazy.get(difference))
      }
    )}
  </div>
}
