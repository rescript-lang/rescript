// The installation tests in .github/workflows/ci.yml also run this suite on
// the oldest supported Node.js version, 22. Tests of APIs that Node.js 22
// lacks belong in a separate entry point that only scripts/test.js runs.

include Stdlib_TestTests
include Stdlib_PromiseTest
include Stdlib_ErrorTests
include Stdlib_ArrayTests
include Stdlib_ListTests
include Stdlib_IntTests
include Stdlib_ObjectTests
include Stdlib_ResultTests
include Stdlib_TypedArrayTests
include Stdlib_FloatTests
include Stdlib_JsonTests
include Stdlib_NullableTests
include Stdlib_DictTests
include Stdlib_IteratorTests
include Stdlib_RegExpTest
include Stdlib_StringTests
include Stdlib_ImportTests
