// Constructors in another order are reported with the expected order, next to
// the differences of each constructor
module M: {
  type t = A(int) | B(string) | C
} = {
  type t = B(int) | A(int) | C
}
