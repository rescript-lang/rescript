<h1 align="center">
  <a href="https://rescript-lang.org/">
    ReScript
  </a>
</h1>

<p align="center">
  <strong>Fast, Simple, Fully Typed JavaScript from the Future.</strong>
</p>

<p align="center">
  <a href="https://www.npmjs.org/package/rescript"><img src="https://img.shields.io/npm/v/rescript?color=brightgreen&label=npm%20package" alt="Current npm package version." /></a>
  <a href="https://github.com/rescript-lang/rescript/actions"><img src="https://github.com//rescript-lang/rescript/workflows/CI/badge.svg" alt="Current Github Actions workflow status." /></a>
  <a href="https://github.com/rescript-lang/rescript/blob/HEAD/LICENSE"><img src="https://img.shields.io/badge/License-LGPL%20v3-blue.svg" alt="ReScript is released under the LGPL license." /></a>
  <a href="https://x.com/intent/follow?screen_name=rescriptlang"><img src="https://img.shields.io/badge/X-000000?style=flat&logo=x&logoColor=white" alt="Follow @rescriptlang on X" /></a>
  <a href="https://bsky.app/profile/rescript-lang.org"><img src="https://img.shields.io/badge/Bluesky-0285FF?logo=bluesky&logoColor=fff&style=flat" alt="Follow @rescriptlang on Bluesky" /></a>
</p>

<h3 align="center">
  <a href="https://rescript-lang.org/docs/manual/latest/introduction">Introduction</a>
  <span> · </span>
  <a href="https://rescript-lang.org/docs/manual/latest/installation">Installation</a>
  <span> · </span>
  <a href="https://rescript-lang.org/try">Try Online</a>
  <span> · </span>
  <a href="https://forum.rescript-lang.org/">Forum</a>
  <span> · </span>
  <a href="CONTRIBUTING.md">Contribute</a>
</h3>

ReScript is a robustly typed language that compiles to efficient and human-readable JavaScript. It comes with a lightning fast compiler toolchain that scales to any codebase size.

- **Fast and Simple.** ReScript cares about a consistent and fast feedback loop for any codebase size. Refactor code, pull complex changes, or switch to feature branches as you please. No sluggish CI builds, stale caches, wrong type hints, or memory hungry language servers that slow you down.
- **A Robust Type System.** Every ReScript app is fully typed and provides reliable type information for any given value in your program. We prioritize simpler types over complex types for the sake of clarity and easy debugability. No `any`, no magic types, no surprise `undefined`.
- **Seamless Integration.** Use any library from JavaScript, export ReScript libraries to JavaScript, automatically generate TypeScript types. It's like you've never left the good parts of JavaScript at all.
- **Tooling that just works out of the box.** A builtin pretty printer, memory friendly [VSCode](https://github.com/rescript-lang/rescript-vscode) & [Vim](https://github.com/rescript-lang/vim-rescript) plugins, a stable type system and compiler that doesn't require lots of extra configuration. ReScript brings all the tools you need to build reliable JavaScript, Node and ReactJS applications.
- **Easy to adopt — without any lock-in.** ReScript was made with gradual adoption in mind. If you ever want to go back to plain JavaScript, just remove all source files and keep its clean JavaScript output. Tell your coworkers that your project will keep functioning with or without ReScript!

ReScript is used by many companies to ship and maintain mission-critical products and is maintained by the [ReScript community](https://rescript-lang.org/community).

## Contents

- [Getting Started](#-getting-started)
- [Documentation](#-documentation)
- [Upgrading](#-upgrading)
- [How to Contribute](#-how-to-contribute)
- [License](#-license)
- [Acknowledgments](#-Acknowledgments)

## 🎉 Getting Started

Follow the [Installation Guide](https://rescript-lang.org/docs/manual/latest/installation) to set up a new ReScript project or integrate ReScript into your existing JavaScript project.

For more information on building React apps with ReScript, see the [rescript-react documentation](https://rescript-lang.org/docs/react/latest/installation).

For React Native apps, see the [rescript-react-native website](https://rescript-react-native.github.io/).

## 📖 Documentation

The full documentation for the ReScript language can be found on our [website](https://rescript-lang.org/).

The source for the ReScript documentation and website is hosted in a [separate repo](https://github.com/rescript-association/rescript-lang.org).

## 🚀 Upgrading

See the [Upgrading Guide](https://rescript-lang.org/docs/manual/latest/installation) for instructions on upgrading to newer ReScript versions.

## 👏 How to Contribute

### [Contributing Guide](CONTRIBUTING.md)

Read our [Contributing Guide](CONTRIBUTING.md) to learn about our development process, how to propose bugfixes and improvements, and how to build and test your changes to ReScript.

### [Code of Conduct][code]

The ReScript community has adopted a Code of Conduct that we expect project participants to adhere to.
Please read the [full text][code] so that you can understand what actions will and will not be tolerated.

[code]: https://rescript-lang.org/community/code-of-conduct

### [Roadmap][roadmap]

You can learn more about our vision for ReScript in the [Roadmap][roadmap].

[roadmap]: https://rescript-lang.org/community/roadmap

### [Discussions][discussions]

For discussions on ongoing development, see the [Development](https://forum.rescript-lang.org/c/development/8) section of the ReScript forum.

[discussions]: https://rescript-lang.org/community/roadmap

## 📄 License

ReScript is licensed under LGPL version 3, with relaxed rules about creating and distributing combined work. See the [LICENSE](LICENSE) file for details.

The ReScript parser (subdirectory `compiler/syntax`) is licensed under the [MIT License](compiler/syntax/LICENSE).

The `ninja` subdirectory contains the vendored [ninja](https://github.com/ninja-build/ninja) build system.
Refer to its copyright and license notices for information about its licensing.

## 🏅 Acknowledgments

ReScript was originally created by [Hongbo Zhang](https://github.com/bobzhang) in 2015.

See [CREDITS.md](CREDITS.md) for further acknowledgements and project history.
