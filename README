These files contain an SML-to-OCAML translation of source examples taken
from the following book:

     Purely Functional Data Structures
     Chris Okasaki
     Cambridge University Press, 1998
     Copyright (c) 1998 Cambridge University Press


Some short notes regarding my port:

I have tried to stick as close as possible to the original code, but
sometimes this cannot be done.

The first nine chapters are translated now. Although there are two further
chapters, I will not translate them anymore, because of restrictions
that exist not only in SML but also in OCAML:

The basic idea of chapter 10 and 11 is the application of techniques
called "structural decomposition" and "structural abstraction" (10)
+ their combination and generalization with ideas in chapter 9 (lazy
redundant binary numbers) in a framework called "implicit recursive
slowdown" (11).

Structural decomposition and abstraction require socalled "polymorphic
recursion". Type inference is unfortunately undecidable in the presence
of the latter, so neither OCAML nor SML support it.

Thus, the author presents two versions for his examples in these chapters:
one which does not pass the type checker because of polymorphic recursion
(for demonstration purposes only), anotherone for showing how to work
around this restriction.

In a question posted to the OCAML-list, I learnt from the OCAML-developers
that they are actively researching ways to circumvent the problem of
polymorphic recursion (e.g. with "forward"-definitions that allow the
user to restrict the type of a polymorphically recursive definition). In
the not unlikely case that they succeed, I will continue to translate
the remaining chapters.

If someone wants to translate the "workaround"-solutions, I will be glad
to include them in this distribution. In the meanwhile I will wait and
see what happens in future OCAML-releases.


Notes on efficiency:

Because the data structures are purely functional, they profit a lot from
garbage collector settings. In case you find that some of them are not
efficient enough, you might want to raise the memory overhead parameter
of the garbage collector. See "http://caml.inria.fr/ocaml/speed.html"
for more details. Performance is in general excellent.


The following rules / differences to the original sources exist:

* No base module

Since there is hardly anything really necessary in the base module,
I copied the few relevant declarations into the modules. This allows
easier testing, because the modules do not depend on others.


* Syntax

Names are created by the following rules:

  * Module types are written in capitals. If they consist of more than
    a word, an underscore ('_') is placed between the words.

  * Names of exceptions follow the same rule as modules types.

  * Module implementations have to start with a capital letter, the
    rest of the name is lowercase - except if it consists of more than
    one word. In this case the first letter of the following word is
    uppercase. There is no underscore between words.


* Currying of function parameters

Currying is not used anywhere in the original source. I have tried to
curry parameters, where it makes sense. Tuples that represent a named type
(e.g. some data structure) are *not* curried in functions that are hidden
by a signature restriction -> more comprehensible.  Functions offered
via the module interface (signature) do not reveal such implementation
details (concrete type) anyway, of course.


* Superfluous bindings

If a parameter is never used in a following expression, it is not bound
to any name, but '_' will hold its place.


* Lazy evaluation

Lazy evaluation is neither an integral part of SML nor of OCAML.
The original author uses an experimental syntax for describing data
structures that have to be evaluated lazily. To give people a hint,
how lazy behaviour can be done, I have used the "Lazy"-module found in
the standard distribution of OCAML.

To make the syntax at least a bit more similar to the original, I
have introduced the prefix operator '!$', which stands for 'force" -
it forces evaluation of a lazy expression. To make an expression lazy,
the expression 'lazy' is used.

There is a test function at the end of the translation of chapter 4,
the chapter in which lazy evaluation and streams (= lazy lists) are
introduced. Uncomment it to try out, how lazy evaluation behaves.


* Interface QUEUE

Due to the impossibility to safely generalize expressions in modules
when parameterized types are used together with "Lazy.t", I had to change
the interface (most convenient workaround):

Instead of the value

  val empty    : 'a queue

the function

  val empty    : unit -> 'a queue

is used now. This guarantees that the empty value can be generated
without anomalies.

For details see:

  http://caml.inria.fr/FAQ/FAQ_EXPERT-eng.html#variables_de_types_faibles

This change allowed e.g. the translation of "PhysicstsQueue".  Of course,
you have to use "empty ()" to get the empty value of queues now.

---------------------------------------------------------------------------

Enjoy the data structures!

Vienna, April 9, 1999
Markus Mottl (markus.mottl@gmail.com)
