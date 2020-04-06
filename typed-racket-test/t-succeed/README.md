- [X] Go through all the succeed tests, see if
      they make sense for transient or not.
- [X] try running all
- [ ] delete typing-only tests
- [ ] fixes / deletes

Note: seems not-practical to auto-convert the lang
 to transient. Many use submod. Others use require.
 It could work with a lang (t/r/transient).
 But expectations change too.

o = fixed
X = not worth testing, fully untyped or something
H0 = failed to parse arrow type
H1 = typed macro
     ... damn, untyped rackunit does not work
         probably need 3rd copy of macros for transient .. same types diff meaning
H2 = error in guarded? wrong contract violation
H3 = transient assert
H4 = unbound id
H5 = contract generation not supported
?? = type-map ref missing

H6 = expected contract error, got transient assert
H7 = no exception
H8 = unexpected val-able
H10 = value-contract missing
H11 = type->domain-map: cannot handle rest type yet
H12 = internal typecheck error
      maybe from rackunit

WH = "what happened"

TODO check for "that's impossible!"
(please make failures clear!)

?  | test
---------
     2d-typed.rkt
     aliasing-tests.rkt
     andmap.rkt
X    annotation-syntax-override.rkt
     annotation-test.rkt
     any-hash.rkt
     any-wrap-list.rkt
oH0   apply-append.rkt
oH0   apply-dots-list.rkt
     apply-dots.rkt
     area.rkt
     arrow-star-contracts.rkt
     assoc-with-is-equal-argument.rkt
     at-exp.rkt
     bad-map-infer.rkt
     barland.rkt
     basic-tests.rkt
     begin0-error.rkt
     both-recursive-types.rkt
     box-num.rkt
     broken-let-syntax.rkt
oH0   call-comp.rkt
     case-arrow-cast-contract.rkt
oH0   case-lambda-rest.rkt
     case-lambda1.rkt
oH1   cast-mod.rkt
     cast-no-check.rkt
oH2 todo    cast-top-level.rkt
oH3   check-expect.rkt
oH3   check-within.rkt
oH0   cl-bug.rkt
     cl-tests.rkt
     cl.rkt
     class-contract.rkt
     cmdline.rkt
     command-line-ps.rkt
     compiled
     continuation-mark.rkt
     contract-opt+kw.rkt
     contract-struct-equality.rkt
     cps.rkt
     curry.rkt
     custodian.rkt
     custom-printer.rkt
     cyclic-list.rkt
     datum-to-syntax.rkt
     def-pred.rkt
     define-forward-reference.rkt
     define-new-subtype-function.rkt
     define-new-subtype-infer.rkt
     define-new-subtype-mu-rec.rkt
     define-new-subtype.rkt
     define-poly-value.rkt
     define-type-omit-define.rkt
oH4   define-typed-untyped-identifier-syntax-properties.rkt
     define-values-invoke-unit-subtyping.rkt
     discrete-dist.rkt
oH0   do.rkt
     dot-intro.rkt
oH0   dotted-identity.rkt
oH0   dotted-identity2.rkt
     dviu-infer-deps-ok.rkt
     dviu-infer-fact.rkt
     dviu-unit-from-context.rkt
oH4   empty-case-arrow.rkt
oH3   empty-or.rkt
oH5   ephemerons.rkt
oH6   even-odd-recursive-type.rkt
     even-odd.rkt
     events-with-async-channel.rkt
     events.rkt
     exceptions.rkt
oH7   exn-any-mutation.rkt
     fix.rkt
     fixnum.rkt
     float-internal-err.rkt
     flonum.rkt
     flvector.rkt
oH0   fold-left-inst.rkt
oH0   fold-left.rkt
X    foldo.rkt ... needed for metrics.rkt
     for-ann.rkt
     for-hash.rkt
     for-in-range.rkt
     for-last.rkt
     for-list.rkt
     for-lists.rkt
     for-no-anns.rkt
     for-no-body-anns.rkt
     for-over-hash.rkt
     for-result.rkt
     for-seq.rkt
     for-set.rkt
     for-vector.rkt
     for.rkt
     force-delay.rkt
     format.rkt
     function.rkt
     fx-filter.rkt
     generalize-vectors.rkt
     gh-issue-144.rkt
     gh-issue-157.rkt
     gh-issue-163-1.rkt
     gh-issue-163-2.rkt
     gh-issue-164.rkt
     gh-issue-181.rkt
     gh-issue-205.rkt
     gh-issue-214.rkt
     gh-issue-26.rkt
     gh-issue-291.rkt
     gh-issue-304.rkt
oH0   gh-issue-336.rkt
     gh-issue-343.rkt
     gh-issue-366.rkt
     gh-issue-38.rkt
     gh-issue-426.rkt
     gh-issue-43.rkt
     gh-issue-506.rkt
     gh-issue-542.rkt
     gh-issue-829.rkt
     gui-lang.rkt
     hari-vector-bug.rkt
X    hash-contract.rkt
     hash-literal.rkt
     hash-ref.rkt
     hashtabletop-flat-contract.rkt
     het-vec.rkt
     het-vec2.rkt
     ho-box.rkt
     icfp-examples.rkt
     if-splitting-test.rkt
     in-hash-in-vector-subtype.rkt
     in-vector-range.rkt
     index-of.rkt
     inexact-complex.rkt
     inf-nested-bot.rkt
     infer-dots.rkt
     infer-funargs.rkt
     inst-dots.rkt
     inst-expected.rkt
     int-def-colon.rkt
     integer-contracts.rkt
     internal-scoped-tvar.rkt
     intersect-no-intersections.rkt
oH3   intersection1.rkt
X    issue-166.rkt
     issue-33.rkt
     issue-447.rkt
H7+  issue-598.rkt (TODO module+ needs defense)
oH7   issue-625.rkt
     issue-628.rkt
     issue-807.rkt
     issue-823.rkt
     json-hash.rkt
     keyword-function-order.rkt
     kw-def.rkt
     kw.rkt
     leftist-heap.rkt
     let-no-anns.rkt
     let-partial-annotations.rkt
     let-values-tests.rkt
todo    lifting-top-level.rkt
     linear-integer-simple.rkt
oH0   list-dots.rkt
     list-ref-vec.rkt
     list-struct-sum.rkt
     literal-char-gh-issue-434.rkt
oH8   literal-regexp-gh-issue-539.rkt
     little-schemer.rkt
     logic.rkt
oH0   lots-o-bugs.rkt
     macro-in-unit.rkt
     make-predicate-mod.rkt
todo    make-predicate-top-level.rkt
     make-top-predicate.rkt
oH5   mandelbrot.rkt
     manual-examples.rkt
     map-nonempty.rkt
     map1.rkt
     map2.rkt
     match-dots.rkt
     match-dots2.rkt
     match-expander-problem.rkt
     match-or.rkt
     match-overlap-unsafe-struct-ref.rkt
     match-tests.rkt
     match.rkt
     member-pred.rkt
     member-with-is-equal-argument.rkt
     metrics.rkt
todo    module-lang.rkt
     module-plus.rkt
     module-repl.rkt
     mpair.rkt
     mu-rec.rkt
     multi-arr-parse.rkt
     mutable-poly-struct.rkt
     mutable-struct-pred.rkt
     namespace-anchor.rkt
oH0   nested-poly.rkt
oH0   new-metrics.rkt
oH0   no-bound-fl.rkt
     non-recursive-and-recursive-type-aliases.rkt
     nonnegative-float.rkt
     null-program.rkt
     num-equal-filter.rkt
     opaque-non-opaque-contracts-together.rkt
     opaque-object-contract.rkt
     opaque-object-name.rkt
X    opaque-object-stronger.rkt
     opt-arg-test.rkt
     opt-lambda.rkt
     optimize-simple.rkt
     or-sym.rkt
     overloading.rkt
     pair-test.rkt
     pair-test2.rkt
     pair-test3.rkt
     param.rkt
     parameter-c.rkt
     parameter-proc.rkt
     parametric-require-tr-base.rkt
     parse-path.rkt
X    patch.rkt
     paths.rkt
     pathstrings.rkt
oH3   pict.rkt TODO PR923
X    places-helper.rkt
     places.rkt
oH0   poly-apply.rkt
oH0   poly-dots.rkt
     poly-ret-ann.rkt
     poly-same-annotation.rkt
     poly-simple-contract.rkt
     poly-struct-parent.rkt
     poly-struct-pred.rkt
     poly-struct-union.rkt
     poly-struct.rkt
     poly-subtype.rkt
     poly-tests.rkt
     ports.rkt
     pr10057.rkt
     pr10318.rkt
     pr10319.rkt
     pr10342.rkt
     pr10470.rkt
     pr10552.rkt
     pr10562.rkt
     pr10718+10755.rkt
     pr10729.rkt
     pr10765.rkt
     pr10937.rkt
     pr10939.rkt
     pr11099.rkt
     pr11171.rkt
     pr11172.rkt
     pr11193.rkt
     pr11194.rkt
     pr11314.rkt
     pr11390.rkt
     pr11392.rkt
     pr11425.rkt
     pr11504.rkt
     pr11509.rkt
     pr11532.rkt
oH0   pr11545+11776.rkt
     pr11560.rkt
     pr11578.rkt
     pr11617.rkt
todo    pr11669.rkt
     pr11686.rkt
     pr11709.rkt
     pr11728.rkt
     pr11747.rkt
     pr11756.rkt
     pr11859.rkt
     pr11866.rkt
     pr11887.rkt
     pr11897.rkt
     pr11901.rkt
     pr11912.rkt
     pr11971.rkt
     pr12224.rkt
     pr12644.rkt
     pr12678.rkt
     pr12806.rkt
     pr12807.rkt
     pr12905.rkt
todo    pr12913.rkt
     pr12970.rkt
     pr12974.rkt
     pr13094.rkt
     pr13124.rkt
     pr13155.rkt
     pr13160.rkt
     pr13161.rkt
     pr13185.rkt
     pr13326.rkt
     pr13339.rkt
     pr13412.rkt
     pr13464.rkt
     pr13490.rkt
todo    pr13503.rkt
oH0   pr13576.rkt
     pr13584.rkt
     pr13646.rkt
     pr13691.rkt
     pr13710.rkt
todo    pr13747.rkt
     pr13821.rkt
     pr13870.rkt
     pr13901.rkt
     pr13937.rkt
oH12  pr14138.rkt wow, guarded optimizer fails if dead code pass doesn't run! dead-code must come first, and it matches to avoid a fall-through
     pr14217.rkt
     pr14355.rkt
     pr14364.rkt
     pr14374.rkt
     pr14458.rkt
X    pr14463.rkt
     pr14521.rkt
     pr14567.rkt
     pr14568.rkt
     pr14582.rkt
any-wrap missing todo    pr14587.rkt
     pr14702.rkt
     pr14823.rkt
     pr14828.rkt
todo    pr14829.rkt
     pr14896.rkt
     pr14997.rkt
     pr15026-a.rkt
     pr15026-b.rkt
     pr15144.rkt
     pr15330.rkt
     pr226-variation-1.rkt
oUntyped code invoked a higher-order value passed as 'Any'     pr226-variation-2.rkt
oH3   pr226-variation-3.rkt
     pr241-variation-0.rkt
oH3   pr241-variation-1.rkt
     pr241-variation-2.rkt
     pr241-variation-3.rkt
todo    pr241-variation-5.rkt
X    pr267-variation-0.rkt
     pr390-variation-1.rkt
     pr390-variation-2.rkt
     pr390-variation-3.rkt
oH1   pr390-variation-4.rkt
X  pr390-variation-5.rkt
     pr390-variation-6.rkt
     pr390-variation-7.rkt
     pr403.rkt
     pr468-in-query.rkt
H1   pr476-compile-time-images.rkt
     pr575-variation-0.rkt
     pr575-variation-1.rkt
     pr575-variation-2.rkt
     pr575-variation-3.rkt
H10  pr575-variation-4.rkt
     pr575-variation-5.rkt
     pr9043.rkt
     pr9046.rkt
     pr9048.rkt
     pr9053-2.rkt
     pr9053.rkt
     pr9054.rkt
WH   prefab-field-provide.rkt
is-num-tuple? broken!  prefab.rkt
     procedure-top.rkt
     promise-provide-generate-contract.rkt
     promise.rkt
inexhaustive match defender.rkt:150     prompt-tag.rkt
     provide-alias-omit-define.rkt
     provide-case-rest.rkt
     provide-for-meta.rkt
     provide-poly-struct.rkt
     provide-sexp.rkt
     provide-struct-untyped.rkt
     provide-struct.rkt
     provide-syntax.rkt
     racket-struct.rkt
H1   rackunit-suite.rkt
     rackunit.rkt
     random-bits.rkt
     rec-het-vec-infer.rkt
     rec-type-alias-variance.rkt
     rec-types.rkt
     recursive-type-alias-terminates.rkt
todo    recursive-type-alias-top-level.rkt
     refinement-even.rkt
     refinements-and-aliases.rkt
H0   refinements-expected-type1.rkt
H0   refinements-quicksort.rkt
     regexp-match-kw.rkt
     req-type-sub.rkt
     require-poly.rkt
     require-procedure.rkt
     require-signature-all-typed.rkt
     require-struct.rkt
     require-substruct.rkt
     require-tests.rkt
     require-typed-contravariant-filter.rkt
X    require-typed-no-check.rkt
     require-typed-on-typed-module.rkt
     require-typed-parse.rkt
     require-typed-rename.rkt
     require-typed-struct-custom-type.rkt
H11  rest-star-hash-examples.rkt
     richard-bugs.rkt
     runtime-path.rkt
     safe-letrec.rkt
     safe-vector-base.rkt
X    safe-vector-untyped.rkt
H0   safe-vector.rkt
H0   scoped-type-vars.rkt
     scratch.rkt
     sealing-contract-1.rkt
     sealing-contract-2.rkt
     sealing-contract-3.rkt
     seasoned-schemer.rkt
     send.rkt
     sequence-cnt.rkt
H7   sequenceof-integer.rkt
H3   sequences.rkt
     set-contract.rkt
     set.rkt
     simple-fake-or.rkt
     simple-implies.rkt
     simple-kw-app.rkt
     simple-occurr.rkt
     simple-or.rkt
     simple-poly.rkt
     simple-unit-scope.rkt
     slow-check.rkt
     slow-parser.rkt
     somesystempath.rkt
     sort-infer.rkt
     standard-features-base.rkt
X    standard-features-no-check-base.rkt
X    standard-features-no-check-gui.rkt
X    standard-features-no-check-scheme-base.rkt
X    standard-features-no-check-scheme.rkt
X    standard-features-no-check-ts.rkt
X    standard-features-no-check.rkt
     standard-features-scheme-base.rkt
     standard-features-scheme.rkt
     standard-features-ts.rkt
     standard-features.rkt
     star-sizes.rkt
     stream.rkt
     string-const.rkt
     struct-cert.rkt
     struct-custom-type.rkt
     struct-exec.rkt
     struct-mutable-contract.rkt
     struct-mutable.rkt
     struct-no-colon.rkt
     struct-options.rkt
     struct-out.rkt
     struct-path-update.rkt
     struct-props.rkt
     struct-struct-out.rkt
     struct-top-recursive.rkt
     struct-type-contract.rkt
     struct-update.rkt
     structs-across-modules.rkt
     structs-variance.rkt
     structs-variance2.rkt
H0   stx.rkt
     submod-vector.rkt
     submodules.rkt
H0   subst-poly-dots.rkt
     test-child-field.rkt
     test.rkt
H0   test2.rkt
     threads-and-async-channels.rkt
     threads-and-channels.rkt
     time.rkt
todo    top-level-begin-for-syntax.rkt
todo    top-level-begin.rkt
todo    top-level-make-predicate.rkt
todo    toplevel-redefinition.rkt
X    transient-contract.rkt
     type-alias-omit-define-syntaxes.rkt
     type-alias-rec-struct.rkt
todo    type-printer-single-level.rkt
     type-variable-scope.rkt
     typeann-letrec.rkt
     typed-list.rkt
X    typed-scheme-no-check-arrow.rkt
     unit-3x-2.rkt
     unit-3x.rkt
     unit-non-prefix-annotation.rkt
     unit-syntax-rule-with-annotation.rkt
     unit-typed-untyped-compound-1.rkt
     unit-typed-untyped-compound-2.rkt
     unit-typed-untyped-values.rkt
     units-no-sigs.rkt
     unsafe-provide-struct.rkt
     unsafe-provide.rkt
     unsafe-reprovide.rkt
     unsafe-require-poly-struct.rkt
X    unsafe-require-top-level.rkt
     unsafe-require.rkt
     unsafe-struct-parent.rkt
     unsafe-struct.rkt
     untyped-submod.rkt
     user-defined-sp.rkt
oH0   values-dots-result.rkt
H0   values-dots.rkt
     values-dots2.rkt
     values-object.rkt
     varargs-tests.rkt
     variance-test.rkt
     vec-len-in-struct-fld.rkt
     vec-tests.rkt
     vector-chap.rkt
X    vector-contract.rkt
     vector-union.rkt
     vectortop-flat-contract.rkt
     with-asserts.rkt
     with-handlers-rest-arg.rkt
     with-handlers-star.rkt
     with-handlers.rkt
"that's impossible!"    with-linear-integer-arith.rkt
     with-linear-integer-arith2.rkt
H12  with-syntax.rkt
todo    with-type-lift.rkt
todo    with-type-typed-context-flag.rkt
todo cannot use in typed module    with-type.rkt
     zero-arg-poly.rkt
