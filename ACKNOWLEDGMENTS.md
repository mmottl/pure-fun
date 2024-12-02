# Acknowledgements

Thanks to Arthur Charguéraud <arthur@chargueraud.org> and Roberto di Cosmo
<roberto@dicosmo.org> for their helpful comments on specifying lazily evaluated
datastructures. They have used the automated theorem prover "Coq" to prove many
of Okasaki's datastructures correct, including their computational complexity.
An earlier implementation of streams in this library differed slightly from
theirs, and it was not clear whether this could break their complexity
guarantees. The new representation is based on Arthur's and Roberto's
recommendations and should be sound.
