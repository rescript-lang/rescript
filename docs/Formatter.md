# ReScript Formatter

## Philosophy

The ReScript formatter is **opinionated**. Formatting decisions are made by the core team based on our collective judgment and vision for the language. We do not aim to accommodate every stylistic preference or engage in extended debates about formatting choices.

The formatter currently has **no configuration settings**, and we aspire to keep it that way. This ensures that ReScript code looks consistent across all projects and teams, eliminating style debates and configuration overhead.

## Decision Making

- **Core team consensus is final**: When the core team reaches consensus on a formatting decision, that decision stands. There is no requirement for community-wide agreement or extensive discussion.

- **Community input is welcome but not binding**: We appreciate suggestions and feedback from the community, but these can be closed without extensive justification if the core team is not aligned with the proposal.

- **No endless style discussions**: We are not interested in protracted debates about formatting preferences. The formatter exists to provide consistent, automated formatting—not to serve as a platform for style negotiations.

## Prior Decisions

The following are examples of formatting decisions the core team has made. This list is not exhaustive, and these decisions do not create binding precedents for future discussions. The core team retains full discretion to make different decisions in similar cases.

- **Smart linebreaks for pipe chains**: The formatter preserves user-introduced linebreaks in pipe chains (`->`), allowing users to control multiline formatting. See [forum announcement](https://forum.rescript-lang.org/t/ann-smart-linebreaks-for-pipe-chains/4734).

- **Preserve multilineness for records**: The formatter preserves multiline formatting for record types and values when users introduce linebreaks. See [issue #7961](https://github.com/rescript-lang/rescript/issues/7961).

**Important**: These examples are provided for reference only. They do not establish rules or precedents that constrain future formatting decisions. The core team may choose different approaches in similar situations based on current consensus.

## Guidelines for Contributors

### Submitting Formatting Issues

- You may open issues to report bugs or propose improvements
- Understand that proposals may be closed if they don't align with core team vision
- Avoid reopening closed issues unless there's new technical information
- Respect that "the core team isn't feeling it" is a valid reason for closure

### What We Consider

- Technical correctness and consistency
- Alignment with ReScript's design philosophy
- Maintainability and simplicity of the formatter implementation
- Core team consensus

### What We Generally Avoid

- Style preferences that don't align with our vision
- Using comparisons to other formatters as the sole justification for changes (while we may align with other formatters on many decisions, we make choices based on our own judgment, not because another formatter does it)
- Requests that would significantly complicate the formatter implementation
- Debates about subjective formatting choices
