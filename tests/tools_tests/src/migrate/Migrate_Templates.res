// Uses the deprecated items of DeprecatedTemplates to validate each kind of migration template.

// %replace.type
let value: DeprecatedTemplates.t = DeprecatedTemplates.Target.make(1)

// A fixed labelled argument
let fromZero1 = DeprecatedTemplates.makeFromZero(1)
let fromZero2 = 1->DeprecatedTemplates.makeFromZero

// An unlabelled argument inserted as a labelled one
let options1 = DeprecatedTemplates.fromOptionsPositional(2, {step: 3})
let options2 = 2->DeprecatedTemplates.fromOptionsPositional({step: 3})

// An unlabelled argument inserted into a function
let delayed = DeprecatedTemplates.ofValue(4)

// An operator built from unlabelled arguments
let sum1 = DeprecatedTemplates.add(1, 2)

// %replace.constructor
let caught = switch throw(DeprecatedTemplates.Target.Failed("boom")) {
| exception DeprecatedTemplates.Failed(message) => message
| () => ""
}

let matched = switch DeprecatedTemplates.Target.Failed("boom") {
| DeprecatedTemplates.Failed(message) => message
| _ => ""
}
