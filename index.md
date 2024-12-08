# Pure-Fun - Purely Functional Data Structures for OCaml

## Overview

`Pure-Fun` is an SML-to-[OCaml](http://www.ocaml.org) translation of source
examples from the book:

```text
Purely Functional Data Structures
Chris Okasaki
Cambridge University Press, 1998
Copyright © 1998 Cambridge University Press
```

## Translation Notes

The first nine chapters are complete. The remaining chapters require
polymorphic recursion, a feature not available in OCaml at the time.
Contributions for these chapters are welcome.

This translation follows the original code, with some necessary adjustments:

### No Base Module

Each module copies relevant contents from the base module for easier testing,
eliminating inter-module dependencies.

### Naming Conventions

- **Module Types**: Written in uppercase, with words separated by underscores.
- **Exceptions**: Follow the same rule as module types.
- **Module Implementations**: Uses PascalCase by capitalizing the first letter
  of each word without using underscores. For example, `ModuleName`.

### Currying

The implementation curries parameters where appropriate. Functions hidden by
signature restrictions do not curry tuples representing named types, aiding
comprehension.

### Unused Parameters

The implementation prefixes unused parameters with an underscore (`_`).

### Lazy Evaluation

The syntax for lazy evaluation differs from the original. Expressions requiring
lazy evaluation have type `lazy`. The code utilizes OCaml's pattern matching
on lazy values and a prefix operator (`!$`) to force evaluation. The `lazy`
keyword creates lazy expressions.

Chapter 4 includes a test function at the end, introducing lazy evaluation
and streams. Uncomment it to explore lazy evaluation.

### Efficiency Notes

Optimize purely functional data structures by adjusting garbage collector
settings. If performance is lacking, consider increasing the garbage
collector's memory overhead parameter. Generally, the implemention is highly
efficient.

## Contributing

Submit bug reports, feature requests, and contributions via the
[GitHub issue tracker](https://github.com/mmottl/pure-fun/issues).

For the latest updates, visit: <https://mmottl.github.io/pure-fun>
