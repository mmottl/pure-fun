# Purely Functional Data Structures

This directory contains the Standard ML source code from

```text
Purely Functional Data Structures
Chris Okasaki
Cambridge University Press, 1998
```

The code is organized into files according to chapter, from `chp2.sml` to
`chp11.sml`. Each file is self-contained, except for a few miscellaneous
definitions in `base.sml`.

The code in the book assumes two non-standard language extensions: support for
lazy evaluation and support for polymorphic recursion. I have modified the
on-line code to work around the lack of polymorphic recursion, but I have made
only minor changes regarding lazy evaluation. IN PARTICULAR, IF YOU COMPILE THE
CODE "AS IS", IT WILL NOT USE LAZY EVALUATION, AND SO WILL NOT ACHIEVE THE
RUNNING TIMES CLAIMED IN THE BOOK.

In the book, I assumed that lazy evaluation was supported in the language with a
`$` operator: `$ exp` would create a suspension for the expression `exp`, and
matching that suspension against a pattern of the form `$ pat` would evaluate
and memoize the suspension. In the on-line code, I simulate this with the
following definition in `base.sml`:

```sml
datatype 'a susp = $ of 'a
```

But, of course, this `$` constructor is not lazy!

There are two further differences related to lazy evaluation. First, the code
in the book assumes that `$` parses with a lower precedence than an ordinary
constructor. Therefore, in the on-line code, I have replaced some occurrences
of `$ exp` with `$ (exp)`. Second, the code in the book assumes the ability to
write lazy functions using a special `fun lazy` syntax. In the on-line code, I
have eliminated dependence on this form.

Note that Standard ML of New Jersey now supports lazy evaluation using a
similar, but not quite identical, syntax. Updating the on-line code to use
their syntax requires the following changes:

- replace each occurrence of

  ```sml
  val s = $ (exp)
  ```

  with

  ```sml
  val lazy s = $ (exp)
  ```

- replace each remaining occurrence of

  ```sml
  ... $ (exp) ...
  ```

  with

  ```sml
  let val lazy s = $ (exp)
  in ... s ... end
  ```

Chris Okasaki <cdo@cs.columbia.edu>
