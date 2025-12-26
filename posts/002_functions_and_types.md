# Functions and Types

In Haskell, every value has a type. For example

* `1.0` is a `Fractional`
* `1` can be either an `Integer` or an `Int` (annoyingly, these are different)
* `[1,2,3]` is either a `list of Integer` or a `list of Int` (annoyingly, these are different)
* `('a', 'c')` is a tuple of Charaters
* `'a'` is a `Char`
* `"a"`' is a `String`, with only one character in it
* `"hello"` is a `String`
  
A difference between Python and Haskell is that in Python, a list can contain many different types. In Haskell, a list can only contain one type:

* `[1,2,3]`: Okay in both
* `['a', 'b', 'c']`: Okay in both
* `[1, 2, 'a']`: Okay in Python, not allowed in Haskell

A function takes exactly one argument, and returns a single value