// Used to check completions across files

// CompletionFromModule.n.
//                        ^com

// CompletionFromModule.nn.
//                         ^com

// CompletionFromModule.nnn->
//                           ^com

open CompletionFromModule.SomeOtherModule
// CompletionFromModule.nnn->
//                           ^com
