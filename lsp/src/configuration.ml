type hover = {
  support_markdown_links: bool; [@key "supportMarkdownLinks"] [@default false]
}
[@@deriving yojson {strict = false}]

type inlay_hints = {
  enable: bool; [@default false]
  max_length: int option; [@key "maxLength"] [@default Some 25]
}
[@@deriving yojson {strict = false}]

type t = {
  hover: hover; [@default {support_markdown_links = false}]
  code_lens: bool; [@key "codeLens"] [@default false]
  inlay_hints: inlay_hints;
      [@key "inlayHints"] [@default {enable = false; max_length = Some 25}]
}
[@@deriving yojson {strict = false}]

let default =
  {
    hover = {support_markdown_links = false};
    code_lens = false;
    inlay_hints = {enable = false; max_length = Some 25};
  }
