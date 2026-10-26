/***
    This module  provides a type `Lazy.t` and functions to create and
    manipulate lazy values.  A lazy value is a value that is not
    computed until it is needed.  This is useful for deferring
    computations that may be expensive or unnecessary.
 */

/**
    The type of a lazy value.  `Lazy.t<'a>` represents a lazy value
    that will eventually yield a value of type `'a` when accessed.
    The value is computed only once, and the result is cached for
    subsequent accesses.  If the computation throws an exception,
    the same exception is thrown again on subsequent accesses.
 */
@notUndefined
type t<+'a>

/** 
    `Lazy.make(f)` creates a lazy value from `f` which is the
    computation to be deferred of type `unit => 'a`. 
    The function returns a lazy value of type `Lazy.t<'a>`.  
    The computation is not executed until the lazy value is accessed.

    ## Examples 
    ```rescript 
    let lazyValue = Lazy.make(() => {
      // Some expensive computation
      Console.log("Computing...")
      42
    });
    lazyValue->Lazy.get->assertEqual(42)
    ```
*/
let make: (unit => 'a) => t<'a>

/**
    `Lazy.get(x)` forces the suspension `x` and returns its result.
    If `x` has already been forced, `Lazy.get(x)` returns the
    same value again without recomputing it.  If it threw an
    exception, the same exception is thrown again.
    Throw `Undefined` if the forcing of `x` tries to force `x`  itself
    recursively.  This is a runtime error.
 */
let get: t<'a> => 'a

/**
    `Lazy.isEvaluated(x)` returns `true` if the suspension `x` has
    already been forced and did not throw an exception.  Otherwise, 
    it returns `false`.  This is useful for checking if a lazy value
    has been computed before accessing it.

    ## Examples
    ```rescript 
    let lazyValue = Lazy.make(() => {
      // Some expensive computation
      Console.log("Computing...")
      42
    })
    Lazy.isEvaluated(lazyValue)->assertEqual(false)
    lazyValue->Lazy.get->assertEqual(42)
    lazyValue->Lazy.isEvaluated->assertEqual(true)
    ```
 */
let isEvaluated: t<'a> => bool

exception Undefined

/**
  `ignore(lazyValue)` ignores the provided lazy value and returns unit.

  This helper is useful when you want to discard a lazy value (for example, when only its side effects matter)
  without having to store or process it further.
*/
external ignore: t<'a> => unit = "%ignore"
