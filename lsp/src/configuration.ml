type hover = {
  support_markdown_links: bool; [@key "supportMarkdownLinks"] [@default false]
}
[@@deriving yojson]

type inlay_hints = {
  enable: bool; [@default false]
  max_length: int option; [@key "maxLength"] [@default Some 25]
}
[@@deriving yojson]

type signature_help = {
  enable: bool; [@default true]
  for_constructor_payloads: bool; [@key "forConstructorPayloads"] [@default true]
}
[@@deriving yojson]

type t = {
  hover: hover; [@default {support_markdown_links = false}]
  code_lens: bool; [@key "codeLens"] [@default false]
  inlay_hints: inlay_hints;
      [@key "inlayHints"] [@default {enable = false; max_length = Some 25}]
  signature_help: signature_help;
      [@key "signatureHelp"]
      [@default {enable = true; for_constructor_payloads = true}]
}
[@@deriving yojson]

let default =
  {
    hover = {support_markdown_links = false};
    code_lens = false;
    inlay_hints = {enable = false; max_length = Some 25};
    signature_help = {enable = true; for_constructor_payloads = true};
  }
