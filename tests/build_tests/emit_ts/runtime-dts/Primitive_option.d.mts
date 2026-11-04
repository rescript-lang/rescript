// External contract for Primitive_option.valFromOption, the unboxing of a
// value already tested to be Some. Stated for an element type that does not
// itself include undefined: there the runtime value of Some(v) is v, and
// valFromOption returns it unchanged.
export declare function valFromOption<A>(x: A): A;
