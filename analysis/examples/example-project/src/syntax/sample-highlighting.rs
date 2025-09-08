// Bindings
fn some_function(param: usize) -> usize {
    let innerBinding = param + 2;
    innerBinding
}

// Types
struct someRecord<typeParameter> {
    someField: usize,
    someOtherField: String,
    theParam: typeParameter,
}

enum someEnum {
    SomeMember,
    AnotherMember,
    SomeMemberWithPayload(someRecord<usize>),
}

// Destructuring
fn destructuring() -> usize {
    let someVar = (1, 2, 3);
    let (one, two, three) = someVar;
    let someObj = someRecord::<usize> {
        someField: 1,
        someOtherField: String::new("HEllo"),
        theParam: 2,
    };

    someObj.someField
}
