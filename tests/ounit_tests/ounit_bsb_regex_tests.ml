let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

let test_eq x y =
  Bsb_regex.global_substitute ~reg:"\\${rescript:\\([-a-zA-Z0-9]+\\)}" x
    (fun _ groups ->
      match groups with
      | x :: _ -> x
      | _ -> assert false)
  =~ y

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           test_eq
             {| hi hi hi ${rescript:name}
        ${rescript:x}
        ${rescript:u}
        |}
             {| hi hi hi name
        x
        u
        |} );
         ( __LOC__ >:: fun _ ->
           test_eq "xx" "xx";
           test_eq "${rescript:x}" "x";
           test_eq "a${rescript:x}" "ax" );
         (__LOC__ >:: fun _ -> test_eq "${rescript:x}x" "xx");
         ( __LOC__ >:: fun _ ->
           test_eq
             {|
{
  "name": "${rescript:name}",
  "version": "${rescript:proj-version}",
  "sources": [
    "src"
  ],
  "dependencies": [
  ]
}
|}
             {|
{
  "name": "name",
  "version": "proj-version",
  "sources": [
    "src"
  ],
  "dependencies": [
  ]
}
|}
         );
         ( __LOC__ >:: fun _ ->
           test_eq
             {|
{
  "name": "${rescript:name}",
  "version": "${rescript:proj-version}",
  "scripts": {
    "clean": "bsb -clean",
    "clean:all": "bsb -clean-world",
    "build": "bsb",
    "build:all": "bsb -make-world",
    "watch": "bsb -w",
  },
  "keywords": [
    "ReScript"
  ],
  "license": "MIT",
  "devDependencies": {
    "bs-platform": "${rescript:bs-version}"
  }
}
|}
             {|
{
  "name": "name",
  "version": "proj-version",
  "scripts": {
    "clean": "bsb -clean",
    "clean:all": "bsb -clean-world",
    "build": "bsb",
    "build:all": "bsb -make-world",
    "watch": "bsb -w",
  },
  "keywords": [
    "ReScript"
  ],
  "license": "MIT",
  "devDependencies": {
    "bs-platform": "bs-version"
  }
}
|}
         );
         ( __LOC__ >:: fun _ ->
           test_eq
             {|
{
    "version": "0.1.0",
    "command": "${rescript:bsb}",
    "options": {
        "cwd": "${workspaceRoot}"
    },
    "isShellCommand": true,
    "args": [
        "-w"
    ],
    "showOutput": "always",
    "isWatching": true,
    "problemMatcher": {
        "fileLocation": "absolute",
        "owner": "ocaml",
        "watching": {
            "activeOnStart": true,
            "beginsPattern": ">>>> Start compiling",
            "endsPattern": ">>>> Finish compiling"
        },
        "pattern": [
            {
                "regexp": "^File \"(.*)\", line (\\d+)(?:, characters (\\d+)-(\\d+))?:$",
                "file": 1,
                "line": 2,
                "column": 3,
                "endColumn": 4
            },
            {
                "regexp": "^(?:(?:Parse\\s+)?(Warning|[Ee]rror)(?:\\s+\\d+)?:)?\\s+(.*)$",
                "severity": 1,
                "message": 2,
                "loop": true
            }
        ]
    }
}
|}
             {|
{
    "version": "0.1.0",
    "command": "bsb",
    "options": {
        "cwd": "${workspaceRoot}"
    },
    "isShellCommand": true,
    "args": [
        "-w"
    ],
    "showOutput": "always",
    "isWatching": true,
    "problemMatcher": {
        "fileLocation": "absolute",
        "owner": "ocaml",
        "watching": {
            "activeOnStart": true,
            "beginsPattern": ">>>> Start compiling",
            "endsPattern": ">>>> Finish compiling"
        },
        "pattern": [
            {
                "regexp": "^File \"(.*)\", line (\\d+)(?:, characters (\\d+)-(\\d+))?:$",
                "file": 1,
                "line": 2,
                "column": 3,
                "endColumn": 4
            },
            {
                "regexp": "^(?:(?:Parse\\s+)?(Warning|[Ee]rror)(?:\\s+\\d+)?:)?\\s+(.*)$",
                "severity": 1,
                "message": 2,
                "loop": true
            }
        ]
    }
}
|}
         );
       ]
