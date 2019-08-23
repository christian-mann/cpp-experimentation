/* Goal:
 * CART_PROD(MAKE_VAR, LETTERS, LETTERS, NUMBERS) -> makes all variables of form aa1
 */

/* We begin with some infrastructure. */

/* As we all know macros don't expand recursively, but there are ways we can work around this.
 * The easiest way of doing recursion in the preprocessor is to use a deferred expression.
 * A deferred expression is an expression that requires more scans to fully expand:
 */

#define EMPTY()
#define DEFER(id) id EMPTY()
#define EXPAND(...) __VA_ARGS__

#define CAT(a, b) CAT_(a, b)
#define CAT_(a, b) a ## b
/* #define A() 123
 * A() // expands to 123
 * DEFER(A)() // Expands to A () because it requires one more scan to fully expand
 * EXPAND(DEFER(A)()) // Expands to 123, because the EXPAND macro forces another scan
 */

/* Why is this important? Well, when a macro is scanned and expanding, it creates a disabling context.
 * This disabling context will cause a token that refers to the currently expanding macro to be painted blue.
 * Thus, once it's painted blue, the macro will no longer expand. This is why macros don't expand recursively.
 * However, a disabling context only exists during one scan, so by deferring an expression
 * we can prevent our macros from becoming painted blue.
 * We will just need to apply more scans to the expression.
 * We can do that using this EVAL macro:
 */
#define EVAL(...) EVAL1(EVAL1(EVAL1(__VA_ARGS__)))
#define EVAL1(...) EVAL2(EVAL2(EVAL2(__VA_ARGS__)))
#define EVAL1(...) EVAL2(EVAL2(EVAL2(__VA_ARGS__)))
#define EVAL2(...) EVAL3(EVAL3(EVAL3(__VA_ARGS__)))
#define EVAL3(...) EVAL4(EVAL4(EVAL4(__VA_ARGS__)))
#define EVAL4(...) EVAL5(EVAL5(EVAL5(__VA_ARGS__)))
#define EVAL5(...) __VA_ARGS__

/* We need to define these sets in terms of X-macros. Two examples are given, as follows: */
#define LETTERS(X, ...) \
	DEFER(X)(a, ##__VA_ARGS__) \
	DEFER(X)(b, ##__VA_ARGS__)

#define NUMBERS(X, ...) \
	DEFER(X)(0, ##__VA_ARGS__) \
	DEFER(X)(1, ##__VA_ARGS__)

#define NUMBERS_INDIRECT() NUMBERS
/* We will need these to be variadic as shown in the near future. */

/* So how do we use these? Here's one rather simple example. */
#define MAKE_VAR(name) int name;

/* Expands to:
 * int a; int b; int c; int d;;
 */
LETTERS(MAKE_VAR); /* The semicolon at the end is not necessary, but vim gets annoyed if it is absent. */

/* So let's make a "Cartesian product" macro that takes only one set as a parameter. */
/* This doesn't do much, but we will expand it in the future. */
#define CART1(OMEGA, SET) SET(OMEGA)
/* Here's how we'd use it: */

/* Expands to:
 * LETTERS(MAKE_VAR)
 * int a; int b; int c; int d;;
 */
CART1(MAKE_VAR, LETTERS);

/* Excellent! Now we can try and extend this to two sets. */

/* Starting with the signature of our function, it should be:
 * #define CART2(OMEGA, SET1, SET2)
 */

/* #define CART2(OMEGA, SET1, SET2) SET1( //, OMEGA, SET2)
 * But what to put in that first argument? It's going to receive the parameters (elem, OMEGA, SET2)
 * for each element of the set, and it needs to proxy to set2.
 *
 * We will need an intermediate macro called BETA. I'll get to ALPHA in a minute.
 */

#define BETA(st, omega, S1, S2) DEFER(CAT(S1, _INDIRECT))()(HELPER, ALPHA, st, omega, S2)
#define CART2(omega, S1, S2) BETA(, omega, S1, S2)

/* ALPHA essentially does the same thing as BETA, just with fewer arguments. */
#define ALPHA(st, omega, S1) DEFER(CAT(S1, _INDIRECT))()(HELPER, omega, st)

/* What this does is allow us to specify something to receive the parameters as described above. 
 * st is the built-up string. For now it's empty, but later it will exist.
 * We could have skipped this, but we define it because we're going to define the rest of the Greek alphabet shortly.
 *
 * But first, let's define this HELPER function. This is the accumulator; it concatenates what we've already seen and the new letter, and passes everything else on.
 */
#define HELPER(let, alpha, st, ...) DEFER(alpha)(st ## let, ##__VA_ARGS__)

/* Things have moved pretty quickly. Here, let me demonstrate how to use this with a sample expansion.
 * I will use ε to represent the empty string; in practice this would simply be empty.
 *
 * CART2(MAKE_VAR, LETTERS, NUMBERS)
 * BETA(ε, MAKE_VAR, LETTERS, NUMBERS)
 * LETTERS(HELPER, ALPHA, ε, MAKE_VAR, NUMBERS)
 * HELPER(a, ALPHA, ε, MAKE_VAR, NUMBERS)          HELPER(b, ALPHA, ep, MAKE_VAR, NUMBERS)
 * ALPHA(a ## ε, MAKE_VAR, NUMBERS)                ALPHA(b ## ε, MAKE_VAR, NUMBERS)
 * ALPHA(a, MAKE_VAR, NUMBERS)                     ALPHA(b, MAKE_VAR, NUMBERS)
 * NUMBERS(HELPER, MAKE_VAR, a)                    NUMBERS(HELPER, MAKE_VAR, b)
 * HELPER(0, MAKE_VAR, a)  HELPER(1, MAKE_VAR, a)  HELPER(0, MAKE_VAR, b)  HELPER(1, MAKE_VAR, b)
 * MAKE_VAR(a0)             MAKE_VAR(a1)             MAKE_VAR(b0)           MAKE_VAR(b1)
 * int a0; int a1; int b0; int b1;
 */

/* But here's what that actually expands to:
 * HELPER(0, MAKE_VAR, a) HELPER(1, MAKE_VAR, a)  HELPER(0, MAKE_VAR, b) HELPER(1, MAKE_VAR, b)
 */


/* So close! Remember how we can't expand macros more than once? That's where EVAL comes in.
 * This will do the trick:
 */
EVAL(CART2(MAKE_VAR, LETTERS, NUMBERS));

/* Let's redefine CART2 so that we don't need to specify EVAL anymore. */
#undef CART2
#define CART2(omega, S1, S2) EVAL(BETA(, omega, S1, S2))


/* But this wouldn't! It gets stuck because LETTERS can't expand twice in a row.
 * We need to define LETTERS_INDIRECT so that the C preprocessor can't figure out that we're recursing.
 */
#define LETTERS_INDIRECT() LETTERS
EVAL(CART2(MAKE_VAR, LETTERS, LETTERS));

/* In the future we will put this in the CARTn() definition, so that we don't need to specify it later. */

/* Now let's create CART3, just to show the pattern. */
#define GAMMA(st, omega, S1, S2, S3) DEFER(CAT(S1, _INDIRECT))()(HELPER, BETA, st, omega, S2, S3)
#define CART3(omega, S1, S2, S3) EVAL(GAMMA(, omega, S1, S2, S3))

CART3(MAKE_VAR, LETTERS, LETTERS, NUMBERS);

/* I think we can easily define CART4 and CART5... let's do it generically! */

#define DELTA(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GAMMA, st, omega, ##__VA_ARGS__)
#define CART4(...) EVAL(DELTA(, __VA_ARGS__))

CART4(MAKE_VAR, LETTERS, NUMBERS, LETTERS, NUMBERS);

#define EPSILON(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, DELTA, st, omega, ##__VA_ARGS__)
#define CART5(...) EVAL(EPSILON(, __VA_ARGS__))

CART5(MAKE_VAR, LETTERS, NUMBERS, NUMBERS, LETTERS, NUMBERS);



/* This is almost fully generic. The only difficulty is the greek letters. I wonder if we can make them generic?
 * Each level needs to refer to the one below.
 *
 * We *could* define a successor function, that maps EPSILON -> DELTA, DELTA -> GAMMA, et cetera.
 * But wouldn't it be better if we could do it generically instead?
 */

/* Let's look at the general form here:
 * #define GREEK_5(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK_4, st, omega, ##__VA_ARGS__)
 * #define CART_5(...) EVAL( GREEK_5(, __VA_ARGS__) )
 *
 * Let's try and create a GREEK(n) function, that returns the correct greek letter!
 */
#define GREEK(n) CAT(GREEK_, n)
#define GREEK_1 ALPHA
#define GREEK_2 BETA
#define GREEK_3 GAMMA
#define GREEK_4 DELTA
#define GREEK_5 EPSILON

/* Okay, so can we use this to redefine CART5? Of course. */
#undef CART5
#define CART5(...) EVAL(GREEK(5)(, __VA_ARGS__))
CART5(MAKE_VAR, LETTERS, LETTERS, LETTERS, LETTERS, LETTERS);

/* Let's redefine all of the other CARTs as well. */
#undef CART4
#define CART4(...) EVAL(GREEK(4)(, __VA_ARGS__))
#undef CART3
#define CART3(...) EVAL(GREEK(3)(, __VA_ARGS__))
#undef CART2
#define CART2(...) EVAL(GREEK(2)(, __VA_ARGS__))
#undef CART1
#define CART1(...) EVAL(GREEK(1)(, __VA_ARGS__))

/* What if we tried redefining EPSILON itself? Sure, it works pretty well. */
#undef EPSILON
#define EPSILON(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(4), st, omega, ##__VA_ARGS__)
CART5(MAKE_VAR, LETTERS, LETTERS, LETTERS, LETTERS, LETTERS);


/* Let's remove reference to that constant 4! We will need a decrementer. */
#define DEC(i) CAT(DEC_, i)
#define DEC_0 0 /* we can't really do anything here, but it shouldn't matter */
#define DEC_1 0
#define DEC_2 1
#define DEC_3 2
#define DEC_4 3
#define DEC_5 4
#define DEC_6 5
#define DEC_7 6
#define DEC_8 7
#define DEC_9 8

#undef EPSILON
#define EPSILON(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(5)), st, omega, ##__VA_ARGS__)
/* Yes, I know we've just replaced one constant with another. I'll get to that. */

CART5(MAKE_VAR, LETTERS, LETTERS, LETTERS, LETTERS, LETTERS);

/* While we're at it, let's rewrite some other greek letters, starting with DELTA. */
#undef DELTA
/* Reminder: DELTA was defined as:
#define DELTA(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GAMMA, st, omega, ##__VA_ARGS__)
*/
#define DELTA(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(4)), st, omega, ##__VA_ARGS__)
CART5(MAKE_VAR, LETTERS, LETTERS, LETTERS, LETTERS, LETTERS);

/* Look at that! They're almost the same definition, with the exception of that constant! */
/* Let's do the others. */
#undef GAMMA
#define GAMMA(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(3)), st, omega, ##__VA_ARGS__)

#undef BETA
#define BETA(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(2)), st, omega, ##__VA_ARGS__)
CART3(MAKE_VAR, LETTERS, LETTERS, LETTERS);

/* It doesn't work for ALPHA, though... 
 * It was defined as:
#define ALPHA(st, omega, S1) DEFER(CAT(S1, _INDIRECT))()(HELPER, omega, st)
 *
 * If we tried to do the following:
#undef ALPHA
#define ALPHA(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(1)), st, omega, ##__VA_ARGS__)
 * Then the following would expand as:
CART2(MAKE_VAR, LETTERS, LETTERS); // this would need to be wrapped in EVAL

GREEK_0 (aa,MAKE_VAR) GREEK_0 (ab,MAKE_VAR) GREEK_0 (ba,MAKE_VAR) GREEK_0 (bb,MAKE_VAR);

 * As you may know, there's no 0th greek letter. We can define one, I suppose. Let's not bother to give it a name.
*/
#define GREEK_0(name, X) X(name)

/* Now we can do as we planned earlier. */
#undef ALPHA
#define ALPHA(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(1)), st, omega, ##__VA_ARGS__)
CART2(MAKE_VAR, LETTERS, LETTERS);

/* In fact, I want to do this for all of the greek letters. No sense in them having names if they don't need them. Let's redefine all of the GREEK_n macros, starting with ALPHA and ascending. */
#undef ALPHA
#undef GREEK_1
#define GREEK_1(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(1)), st, omega, ##__VA_ARGS__)
CART2(MAKE_VAR, LETTERS, NUMBERS);

#undef BETA
#undef GREEK_2
#define GREEK_2(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(2)), st, omega, ##__VA_ARGS__)

#undef GAMMA
#undef GREEK_3
#define GREEK_3(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(3)), st, omega, ##__VA_ARGS__)

#undef DELTA
#undef GREEK_4
#define GREEK_4(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(4)), st, omega, ##__VA_ARGS__)

#undef EPSILON
#undef GREEK_5
#define GREEK_5(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(5)), st, omega, ##__VA_ARGS__)
/* Just to test that everything still works: */
CART5(MAKE_VAR, LETTERS, LETTERS, LETTERS, NUMBERS, LETTERS);

/* I'm still annoyed by this hardcoding of the number, though.
 * The only GREEK_ macro that is different than the rest of them is GREEK_0.
 * Can we special-case that somehow? That way we wouldn't have to define any GREEK_ at all.
 * 
 * We need an IF statement.
 * Shoutouts to pfultz2 for this definition. I won't pretend to understand it.
 * https://github.com/pfultz2/Cloak/wiki/Is-the-C-preprocessor-Turing-complete%3F
 */

#define CHECK_N(x, n, ...) n
#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)

#define NOT(x) CHECK(CAT(NOT_, x))
#define NOT_0 ~, 1,

#define COMPL(b) CAT(COMPL_, b)
#define COMPL_0 1
#define COMPL_1 0

#define BOOL(x) COMPL(NOT(x))

#define IIF(c) CAT(IIF_, c)
#define IIF_0(t, ...) __VA_ARGS__
#define IIF_1(t, ...) t

#define IF(c) IIF(BOOL(c))

IF(0)(int a, int b); /* expands to: int b; */
IF(1)(int a, int b); /* expands to: int a; */

/* Wow, that's a lot of macros.
 * We probably should have just stayed with the GREEK_n idea, but I happen to think it's ugly.
 * Besides, this is now Reusable Code™
 */

/* Anyway, let's use this to redefine our GREEK macro.
 * It was defined as:
#define GREEK(n) CAT(GREEK_, n)
*/
#undef GREEK
/* Yes, I /know/ this is equivalent to what we started with. I'll change it in a minute. */
#define GREEK(n) IF(n) (CAT(GREEK_, n), GREEK_0)
CART2(MAKE_VAR, LETTERS, LETTERS);


/* It's been a while. Let's remind ourselves exactly what's happening when we call CARTn.
 * I'm going to skip over CAT, DEFER, and _INDIRECT when convenient.
 * I'm also going to use g0, g1, g2, etc to refer to greek letters.
 * I'm also going to use Ω to refer to omega, because I just learned about vim digraphs.
 * Also, I'm again going to use ε to refer to the empty string for clarity.
 *
 * 0. CART2( Ω, S1, S2 )
 * 1. g2( ε, Ω, S1, S2 )
 * 2. S1(H, g1, ε, Ω, S2 )
 * 3. H( s1, g1, ε, Ω, S2 )
 * 4. g1( s1 ## ε, Ω, S2 )
 * 5. g1( s1, Ω, S2 )
 * 6. S2(H, g0, s1, Ω)
 * 7. H(s2, g0, s1, Ω)
 * 8. g0(s1s2, Ω)
 * 9. Ω(s1s2)
 *
 * Look at what's happening with these greek letters. Their only purpose is to generate the next greek letter, except of course for g0.
 * What if we folded the definition for GREEK_n into the definition for our old friend HELPER?
 * We could then replace g0, g1, g2 and such with simply 0, 1, 2.
 */

/* As a reminder, this is how HELPER was defined:
#define HELPER(let, alpha, st, ...) DEFER(alpha)(st ## let, ##__VA_ARGS__)
 * The "alpha" parameter is really any greek letter.
 *
 * And this is how GREEK_3 (as an example) is defined:
#define GREEK_3(st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, GREEK(DEC(3)), st, omega, ##__VA_ARGS__)
 * Let's fold these together.
*/
#undef HELPER
#define HELPER(let, glevel, omega, S1, ...) S1(HELPER, DEC(glevel), st##let, omega, __VA_ARGS__)

/* This is insufficient, as it does not have a terminating condition. The two cases that we should consider are as follows: (lots of Greek letters incoming)
 * H(let, gn, st, Ω, S1=None, ...)
 * 		if n == 0 ---> Ω(st##let)
 * 		if n != 0 ---> S1(H, gn-1, st##let, Ω, ...)
 * But C macros cannot have optional parameters.
 * Instead, we need to define two helper macros for the, uh, helper.
 * HELPER0 is the base case, and HELPERn is the case from before.
 */
#define HELPER0(let, st, omega) omega(st ## let)
#define HELPERn(let, glevel, st, omega, S1, ...) S1(HELPER, DEC(glevel), st##let, omega, __VA_ARGS__)
/* Actually, for HELPERn we need to use the _INDIRECT trick again. */
#undef HELPERn
#define HELPER_INDIRECT() HELPER
#define HELPERn(let, glevel, st, omega, S1, ...) DEFER(CAT(S1, _INDIRECT)) () (HELPER, DEC(glevel), st##let, omega, __VA_ARGS__)

/* Now we redefine HELPER to choose between these. */
#undef HELPER
#define HELPER(let, glevel, ...) IF(glevel)(HELPERn(let, glevel, __VA_ARGS__), HELPER0(let, __VA_ARGS__))

/* And now we need to redefine CARTn to use numbers instead of GREEK_n.
 * This also means they're going to have to be a little more complex.
 * As a reminder, CART4 was defined as:
#define CART4(...) EVAL(GREEK(4)(, __VA_ARGS__))
C2(Ω, S1, S2) -> g2(ε, Ω, S1, S2)
              -> S1(H, g1, ε, Ω, S2)
 * We're combining these two into one, and using numbers instead of gn.
 */
#undef CART5
#undef CART4
#undef CART3
#undef CART2
#undef CART1
//#define CART1(omega, S1)         EVAL( S1(HELPER, 0, , omega) )
//#define CART2(omega, S1, S2)     EVAL( S1(HELPER, 1, , omega, S2) )
//#define CART3(omega, S1, S2, S3) EVAL( S1(HELPER, 2, , omega, S2, S3) )
#define CART1(omega, S1, ...) EVAL( S1(HELPER, 0, , omega, ##__VA_ARGS__) )
#define CART2(omega, S1, ...) EVAL( S1(HELPER, 1, , omega, ##__VA_ARGS__) )
#define CART3(omega, S1, ...) EVAL( S1(HELPER, 2, , omega, ##__VA_ARGS__) )
#define CART4(omega, S1, ...) EVAL( S1(HELPER, 3, , omega, ##__VA_ARGS__) )
#define CART5(omega, S1, ...) EVAL( S1(HELPER, 4, , omega, ##__VA_ARGS__) )

/* If your automation senses are tingling, please be patient. We're leaving it at this for right now. */

/* Oh god I hope this still works. */
CART1(MAKE_VAR, LETTERS);
CART2(MAKE_VAR, LETTERS, NUMBERS);
CART3(MAKE_VAR, LETTERS, LETTERS, LETTERS);
