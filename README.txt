These files contain an SML-to-OCaml translation of source examples taken
from the following book:

     Purely Functional Data Structures
     Chris Okasaki
     Cambridge University Press, 1998
     Copyright (c) 1998 Cambridge University Press


Some short notes regarding my port:

I have tried to stick as close as possible to the original code, but sometimes
this cannot be done.

The first nine chapters are translated now.  There are two further chapters,
whose implementation requires polymorphic recursion.  This feature was not
available in OCaml for a while, which is why they have not been translated yet.
Feel free to contribute them!


Notes on efficiency:

Because the data structures are purely functional, they profit a lot from
garbage collector settings.  In case you find that some of them are not
efficient enough, you might want to raise the memory overhead parameter of
the garbage collector.  Performance is in general excellent.


The following rules / differences to the original sources exist:

* No base module

Since there is hardly anything indispensable in the base module, I copied the
few relevant declarations into each module.  This allows for easier testing,
because the modules do not depend on others.


* Syntax

Names are created by the following rules:

  * Module types are written in capitals.  If they consist of more than a
    word, an underscore ('_') is placed between the words.

  * Names of exceptions follow the same rule as modules types.

  * Module implementations have to start with a capital letter, the rest of
    the name is lowercase - except if it consists of more than one word.
    In this case the first letter of the following word is uppercase.
    There is no underscore between words.


* Currying of function parameters

Currying is not used anywhere in the original source.  I have tried to
curry parameters where it makes sense.  Tuples that represent a named type
(e.g. some data structure) are *not* curried in functions that are hidden by
a signature restriction.  This seems to aid comprehension.  Functions offered
via the module interface (signature) do not reveal such implementation details
(i.e. the concrete type) anyway.


* Superfluous bindings

If a parameter is never used in a following expression, it is not bound to
any name.  '_' will hold its place.


* Lazy evaluation

The syntax for lazy evaluation used to implement the datastructures
and algorithms that require them is quite different from the original.
The "lazy" type is used to specify datastructures that need lazy evaluation.
Since recently, OCaml now also supports pattern matching on lazy values,
which is used throughout.

To make the syntax at least a bit more similar to the original, I have also
introduced the prefix operator '!$', which stands for 'force" - it forces
evaluation of a lazy expression.  To make an expression lazy, the expression
'lazy' is used.

There is a test function at the end of the translation of chapter 4, the
chapter in which lazy evaluation and streams (= lazy lists) are introduced.
Uncomment it to see how lazy evaluation works.

---------------------------------------------------------------------------

Enjoy the data structures!

Rutherford, March 28, 2012
Markus Mottl (markus.mottl@gmail.com)
