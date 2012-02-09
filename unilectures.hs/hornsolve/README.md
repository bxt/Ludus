(Horn)SAT
=========

The example clauses used here are:

    (1 ? A ? B) ? (A ? C ? E) ? (A ? C ? D) ? (B ? C ? E) ? (E -> 0)

The resolution tree is:

      /   \
     A     B
     /\    /\
    C  E  C  E
    |  |     |
    D  *     *

Which yields these solutions: {A,C,D}, {B,C}

    > runhaskell hornsolve.hs
    ["ACD","BC"]

It is however not the real algorithm as it uses dynamic sets. 
