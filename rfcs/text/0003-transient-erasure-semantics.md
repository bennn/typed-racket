- Feature Name: transient-erasure-semantics
- Start Date: 2020-07-21
- RFC PR: (leave this empty)
- Feature Commit(s): (leave this empty)

# Summary

The goal is to add two new languages to Typed Racket (TR). These language use
the same static types as TR to compile a program, but have different opinions
about what types mean when a program runs.

Here is a quick comparison. To ground the discussion, let's say we have
the declaration `(: str* (Listof String))` in typed code. The value of `str*`
can come from untyped code, so the question "what do types mean?" really asks
what untyped values can enter this variable, and what values lead to a run-time
error.

1. Normal `#lang typed/racket` offers _Deep_ types. The whole static type means
   something at run-time. For `str*`, the value is guaranteed to be a list
   of only strings.

2. New `#lang typed/racket/shallow` gives weaker, _Shallow_ types. Only
   the outer-most part of the type, the constructor, means something. For
   `str*`, the value is guaranteed to be a list, but there is no guarantee
   about the elements. That said, if our typed code takes `(car str*)` then
   the result has type `String` and is guaranteed to be a string --- if the
   list had a bad element, the program would raise an error at the `car`.

3. New `#lang typed/racket/optional` is like the `no-check` languages, but
   actually type-checks the code. These _Optional_ types mean nothing at
   run-time. The value of `str*` could be anything.

For the rest of this RFC, the focus is on Shallow types and how we plan to
implement them using the _Transient_ semantics. The idea with Transient is
to rewrite all typed code with simple assertions about the shape of values.

By contrast to Transient, normal TR gets Deep types through the Guarded
(aka Natural) semantics. The idea with Guarded is to keep a strict boundary
between typed and untyped code with flat contracts, chaperones, and
impersonators.

The Optional idea is simple: use TR type checker at compile-time and normal
`#lang racket` behavior at run-time.

(Optional is here, rather than a separate RFC, to test that PR #948 has a
flexible way to pick different meannings for types. I'm hoping code that works
for 3 ideas will be able to handle other ideas if needed.)


# Motivation

> Why are we doing this? What use cases does it support? What is the expected outcome?

# Guide-level explanation

> Explain the proposal as if it was already included in the language and you were
> teaching it to another Typed Racket programmer. That generally means:
> 
> - Introducing new named concepts.
> - Explaining the feature largely in terms of examples.
> - Explaining how Typed Racket programmers should *think* about the feature.
> 
> For implementation-oriented RFCs (e.g. for type checker internals), focus on how
> type system contributors should think about the change, and give examples of its
> concrete impact.

# Reference-level explanation

> Explain the design in sufficient detail that:
> 
> - Its interaction with other features is clear.
> - It is reasonably clear how the feature would be implemented.
> - Corner cases are dissected by example.
> 
> Return to the examples given in the previous section, and explain more fully how
> the detailed proposal makes those examples work.


# Drawbacks and Alternatives
[drawbacks]: #drawbacks

Drawback: more languages = more choices = more ways to get confused.
And people may be surprised that typed-to-typed communication is still
 expensive (because Deep needs to protect itself against Shallow types).

One alternative is to keep trying to make TR faster.

Another is to implement Shallow types using wrappers instead of the Transient
strategy. With wrappers, interaction with TR should be faster. But doing may
require new contracts (or chaperones), and will prevent some typed/untyped
mixes that Transient allows (unless we can make new chaperones to allow them).

Overall, Transient seems like a great compromise to try first. If it somehow
fails, we can always deprecate and try a new language.


# Prior art
[prior-art]: #prior-art

> - Does this feature exist in other programming languages and what experience
>   have their community had?

Shallow types are in Reticulated Python and Grace.

Reticulated is the original home of the Transient semantics. Michael Vitousek
invented Transient; his dissertation talks about experiences (esp. Chap 4).

Grace has shallow checks that were inspired by Transient.


> - Papers: Are there published papers, books, blog posts, etc?

Resources for transient semantics (Shallow):
- <https://scholarworks.iu.edu/dspace/handle/2022/23172>
- <https://2019.ecoop.org/details/ecoop-2019-papers/15/Transient-Typechecks-are-Almost-Free>

Resources that compare Deep and Shallow types:
- <http://prl.ccs.neu.edu/blog/2018/12/11/the-behavior-of-gradual-types-a-user-study/>
- <http://prl.ccs.neu.edu/blog/2019/10/31/complete-monitors-for-gradual-types/>

Resources that talk about many ways of mixing typed and untyped code:
- <http://prl.ccs.neu.edu/blog/2018/10/06/a-spectrum-of-type-soundness-and-performance/>
- <http://ccs.neu.edu/home/types/publications/publications.html#gfd-oopsla-2019>


# Unresolved questions
[unresolved]: #unresolved-questions

PR #948 has a list of lower-level todo items.


> - What parts of the design do you expect to resolve through the RFC process
>   before this gets merged?

Documentation. What should it include, and how should it be organized to
 introduce new TR languages.

> - What parts of the design do you expect to resolve through the implementation
>   of this feature before stabilization?

1. How to insert checks everywhere. Today, RackUnit `test-case`s and
   `with-handlers` blocks are unsafe because they're marked as "ignored" by TR.
   There may be others. (2020-07-26)

2. What's the ideal check for every type?
   Should `(Listof T)` use `list?` or `(or/c null? pair?)`?
   Should `(-> Void Void)` use `procedure?` or
    `(and/c procedure? (procedure-arity-includes/c 1))`?
   So far I've gone for "complete" checks to help the programmer and TR
    optimizer, but if the cost is too high let's go simpler.

3. How to minimize the cost of each check, probably by avoiding `racket/contract`
   combinators.

> - What related issues do you consider out of scope for this RFC that could be
>   addressed in the future independently of the solution that comes out of this
>   RFC?

Below are 4 issues related to the transient semantics.


- How to add blame? For now, an error gives a failed check (value, type,
  srcloc) and a stack trace. Use regular to TR help debug runtime type errors.

- How to avoid redundant checks (via static/dynamic analysis)? For example, a
 function that takes the `car` of a pair twice currently pays 2 checks.

  ```
    (define (f (xy : (Pairor Real Real))) : Real
      (+ (car xy) (car xy))
  
      #;(+ (check real? (car xy)) (check real? (car xy))))
  ```


- How to trust full types? For now, certain identifiers are trusted. One example
  is `map`, which always returns a `list?` and doesn't need to be checked.
  One non-example is `negate`. Currently, the implementation can trust `negate`
  returns a function, but it can't trust that new function to return booleans.

- How to re-use library wrappers? A library wrapper, like `typed/pict`, is
  written in the normal TR language. Shallow types can re-use these definitions,
  but need to pay the full TR cost at the library boundary. Ideally, these
  library wrappers should be in a special language that chooses Deep/Shallow
  types based on the `require`-ing module ... but we need to make sure that's
  always safe.


