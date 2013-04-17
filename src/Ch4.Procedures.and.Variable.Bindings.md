![](canned/ch4.png)

Chapter 4. Procedures and Variable Bindings
===========================================

Procedures and variable bindings are the fundamental building blocks of
Scheme programs. This chapter describes the small set of syntactic forms
whose primary purpose is to create procedures and manipulate variable
bindings. It begins with the two most fundamental building blocks of
Scheme programs: variable references and `lambda` expressions, and
continues with descriptions of the variable binding and assignment forms
such as `define`, `letrec`, `let-values`, and `set!`.

Various other forms that bind or assign variables for which the binding
or assignment is not the primary purpose (such as named `let`) are found
in Chapter [5](control.html#g96).

### Section 4.1. Variable References

**syntax**: `variable` \
 **returns:**the value of `variable`

Any identifier appearing as an expression in a program is a variable if
a visible variable binding for the identifier exists, e.g., the
identifier appears within the scope of a binding created by `define`,
`lambda`, `let`, or some other variable-binding construct.

`list `$\Rightarrow$` #<procedure>`<br>
`(define x 'a)`<br>
`(list x x) `$\Rightarrow$` (a a)`<br>
`(let ([x 'b])`<br>
`  (list x x)) `$\Rightarrow$` (b b)`<br>
`(let ([let 'let]) let) `$\Rightarrow$` let`

It is a syntax violation for an identifier reference to appear within a
`library` form or top-level program if it is not bound as a variable,
keyword, record name, or other entity. Since the scope of the
definitions in a `library`, top-level program, `lambda`, or other local
body is the entire body, it is not necessary for the definition of a
variable to appear before its first reference appears, as long as the
reference is not actually evaluated until the definition has been
completed. So, for example, the reference to `g` within the definition
of `f` below

`(define f`<br>
`  (lambda (x)`<br>
`    (g x)))`<br>
`(define g`<br>
`  (lambda (x)`<br>
`    (+ x x)))`

is okay, but the reference to `g` in the definition of `q` below is not.

`(define q (g 3))`<br>
`(define g`<br>
`  (lambda (x)`<br>
`    (+ x x)))`

### Section 4.2. Lambda

**syntax**: `(lambda formals body1 body2 ...)` \
 **returns:**a procedure \
 **libraries:**`(rnrs base)`, `(rnrs)`

The `lambda` syntactic form is used to create procedures. Any operation
that creates a procedure or establishes local variable bindings is
ultimately defined in terms of `lambda` or `case-lambda`.

The variables in `formals` are the formal parameters of the procedure,
and the sequence of subforms `body1 body2 ...` is its body.

The body may begin with a sequence of definitions, in which case the
bindings created by the definitions are local to the body. If
definitions are present, the keyword bindings are used and discarded
while expanding the body, and the body is expanded into a `letrec*`
expression formed from the variable definitions and the remaining
expressions, as described on page [292](syntax.html#body-expansion). The
remainder of this description of `lambda` assumes that this
transformation has taken place, if necessary, so that the body is a
sequence of expressions without definitions.

When the procedure is created, the bindings of all variables occurring
free within the body, excluding the formal parameters, are retained with
the procedure. Subsequently, whenever the procedure is applied to a
sequence of actual parameters, the formal parameters are bound to the
actual parameters, the retained bindings are restored, and the body is
evaluated.

Upon application, the formal parameters defined by `formals` are bound
to the actual parameters as follows.

-   If `formals` is a proper list of variables, e.g., `(x y z)`, each
    variable is bound to the corresponding actual parameter. An
    exception with condition type `&assertion` is raised if too few or
    too many actual parameters are supplied.
-   If `formals` is a single variable (not in a list), e.g., `z`, it is
    bound to a list of the actual parameters.
-   If `formals` is an improper list of variables terminated by a
    variable, e.g., `(x y . z)`, each variable but the last is bound to
    the corresponding actual parameter. The last variable is bound to a
    list of the remaining actual parameters. An exception with condition
    type `&assertion` is raised if too few actual parameters are
    supplied.

When the body is evaluated, the expressions in the body are evaluated in
sequence, and the procedure returns the values of the last expression.

Procedures do not have a printed representation in the usual sense.
Scheme systems print procedures in different ways; this book uses the
notation `#<procedure>`.

`(lambda (x) (+ x 3)) `$\Rightarrow$` #<procedure>`<br>
`((lambda (x) (+ x 3)) 7) `$\Rightarrow$` 10`<br>
`((lambda (x y) (* x (+ x y))) 7 13) `$\Rightarrow$` 140`<br>
`((lambda (f x) (f x x)) + 11) `$\Rightarrow$` 22`<br>
`((lambda () (+ 3 4))) `$\Rightarrow$` 7 `<br>
`((lambda (x . y) (list x y))`<br>
` 28 37) `$\Rightarrow$` (28 (37))`<br>
`((lambda (x . y) (list x y))`<br>
` 28 37 47 28) `$\Rightarrow$` (28 (37 47 28))`<br>
`((lambda (x y . z) (list x y z))`<br>
` 1 2 3 4) `$\Rightarrow$` (1 2 (3 4))`<br>
`((lambda x x) 7 13) `$\Rightarrow$` (7 13)`

### Section 4.3. Case-Lambda

A Scheme `lambda` expression always produces a procedure with a fixed
number of arguments or with an indefinite number of arguments greater
than or equal to a certain number. In particular,

`(lambda (var1 ... varn) body1 body2 ...)`

accepts exactly `n` arguments,

`(lambda r body1 body2 ...)`

accepts zero or more arguments, and

`(lambda (var1 ... varn . r) body1 body2 ...)`

accepts `n` or more arguments.

`lambda` cannot directly produce, however, a procedure that accepts,
say, either two or three arguments. In particular, procedures that
accept optional arguments are not supported directly by `lambda`. The
latter form of `lambda` shown above can be used, in conjunction with
length checks and compositions of `car` and `cdr`, to implement
procedures with optional arguments, though at the cost of clarity and
efficiency.

The `case-lambda` syntactic form directly supports procedures with
optional arguments as well as procedures with fixed or indefinite
numbers of arguments. `case-lambda` is based on the `lambda*` syntactic
form introduced in the article "A New Approach to Procedures with
Variable Arity" [[11](#references)].

**syntax**: `(case-lambda clause ...)` \
 **returns:**a procedure \
 **libraries:**`(rnrs control)`, `(rnrs)`

A `case-lambda` expression consists of a set of clauses, each resembling
a `lambda` expression. Each `clause` has the form below.

`[formals body1 body2 ...]`

The formal parameters of a clause are defined by `formals` in the same
manner as for a `lambda` expression. The number of arguments accepted by
the procedure value of a `case-lambda` expression is determined by the
numbers of arguments accepted by the individual clauses.

When a procedure created with `case-lambda` is invoked, the clauses are
considered in order. The first clause that accepts the given number of
actual parameters is selected, the formal parameters defined by its
`formals` are bound to the corresponding actual parameters, and the body
is evaluated as described for `lambda` above. If `formals` in a clause
is a proper list of identifiers, then the clause accepts exactly as many
actual parameters as there are formal parameters (identifiers) in
`formals`. As with a `lambda` `formals`, a `case-lambda` clause
`formals` may be a single identifier, in which case the clause accepts
any number of arguments, or an improper list of identifiers terminated
by an identifier, in which case the clause accepts any number of
arguments greater than or equal to the number of formal parameters
excluding the terminating identifier. If no clause accepts the number of
actual parameters supplied, an exception with condition type
`&assertion` is raised.

The following definition for `make-list` uses `case-lambda` to support
an optional fill parameter.

`(define make-list`<br>
`  (case-lambda`<br>
`    [(n) (make-list n #f)]`<br>
`    [(n x)`<br>
`     (do ([n n (- n 1)] [ls '() (cons x ls)])`<br>
`         ((zero? n) ls))]))`

The `substring` procedure may be extended with `case-lambda` to accept
either no `end` index, in which case it defaults to the end of the
string, or no `start` and `end` indices, in which case `substring` is
equivalent to `string-copy`:

`(define substring1`<br>
`  (case-lambda`<br>
`    [(s) (substring1 s 0 (string-length s))]`<br>
`    [(s start) (substring1 s start (string-length s))]`<br>
`    [(s start end) (substring s start end)]))`

It is also possible to default the `start` index rather than the `end`
index when only one index is supplied:

`(define substring2`<br>
`  (case-lambda`<br>
`    [(s) (substring2 s 0 (string-length s))]`<br>
`    [(s end) (substring2 s 0 end)]`<br>
`    [(s start end) (substring s start end)]))`

It is even possible to require that both or neither of the `start` and
`end` indices be supplied, simply by leaving out the middle clause:

`(define substring3`<br>
`  (case-lambda`<br>
`    [(s) (substring3 s 0 (string-length s))]`<br>
`    [(s start end) (substring s start end)]))`

### Section 4.4. Local Binding

**syntax**: `(let ((var expr) ...) body1 body2 ...)` \
 **returns:**the values of the final body expression \
 **libraries:**`(rnrs base)`, `(rnrs)`

`let` establishes local variable bindings. Each variable `var` is bound
to the value of the corresponding expression `expr`. The body of the
`let`, in which the variables are bound, is the sequence of subforms
`body1 body2 ...` and is processed and evaluated like a `lambda` body.

The forms `let`, `let*`, `letrec`, and `letrec*` (the others are
described after `let`) are similar but serve slightly different
purposes. With `let`, in contrast with `let*`, `letrec`, and `letrec*`,
the expressions `expr ...` are all outside the scope of the variables
`var ...`. Also, in contrast with `let*` and `letrec*`, no ordering is
implied for the evaluation of the expressions `expr ...`. They may be
evaluated from left to right, from right to left, or in any other order
at the discretion of the implementation. Use `let` whenever the values
are independent of the variables and the order of evaluation is
unimportant.

`(let ([x (* 3.0 3.0)] [y (* 4.0 4.0)])`<br>
`  (sqrt (+ x y))) `$\Rightarrow$` 5.0 `<br>
`(let ([x 'a] [y '(b c)])`<br>
`  (cons x y)) `$\Rightarrow$` (a b c) `<br>
`(let ([x 0] [y 1])`<br>
`  (let ([x y] [y x])`<br>
`    (list x y))) `$\Rightarrow$` (1 0)`

The following definition of `let` shows the typical derivation of `let`
from `lambda`.

`(define-syntax let`<br>
`  (syntax-rules ()`<br>
`    [(_ ((x e) ...) b1 b2 ...)`<br>
`     ((lambda (x ...) b1 b2 ...) e ...)]))`

Another form of `let`, *named* `let`, is described in
Section [5.4](control.html#g100), and a definition of the full `let` can
be found on page [312](syntax.html#defn:let).

**syntax**: `(let* ((var expr) ...) body1 body2 ...)` \
 **returns:**the values of the final body expression \
 **libraries:**`(rnrs base)`, `(rnrs)`

`let*` is similar to `let` except that the expressions `expr ...` are
evaluated in sequence from left to right, and each of these expressions
is within the scope of the variables to the left. Use `let*` when there
is a linear dependency among the values or when the order of evaluation
is important.

`(let* ([x (* 5.0 5.0)]`<br>
`       [y (- x (* 4.0 4.0))])`<br>
`  (sqrt y)) `$\Rightarrow$` 3.0 `<br>
`(let ([x 0] [y 1])`<br>
`  (let* ([x y] [y x])`<br>
`    (list x y))) `$\Rightarrow$` (1 1)`

Any `let*` expression may be converted to a set of nested `let`
expressions. The following definition of `let*` demonstrates the typical
transformation.

`(define-syntax let*`<br>
`  (syntax-rules ()`<br>
`    [(_ () e1 e2 ...)`<br>
`     (let () e1 e2 ...)]`<br>
`    [(_ ((x1 v1) (x2 v2) ...) e1 e2 ...)`<br>
`     (let ((x1 v1))`<br>
`       (let* ((x2 v2) ...) e1 e2 ...))]))`

**syntax**: `(letrec ((var expr) ...) body1 body2 ...)` \
 **returns:**the values of the final body expression \
 **libraries:**`(rnrs base)`, `(rnrs)`

`letrec` is similar to `let` and `let*`, except that all of the
expressions `expr ...` are within the scope of all of the variables
`var ...`. `letrec` allows the definition of mutually recursive
procedures.

`(letrec ([sum (lambda (x)`<br>
`                (if (zero? x)`<br>
`                    0`<br>
`                    (+ x (sum (- x 1)))))])`<br>
`  (sum 5)) `$\Rightarrow$` 15`

The order of evaluation of the expressions `expr ...` is unspecified, so
a program must not evaluate a reference to any of the variables bound by
the `letrec` expression before all of the values have been computed.
(Occurrence of a variable within a `lambda` expression does not count as
a reference, unless the resulting procedure is applied before all of the
values have been computed.) If this restriction is violated, an
exception with condition type `&assertion` is raised.

An `expr` should not return more than once. That is, it should not
return both normally and via the invocation of a continuation obtained
during its evaluation, and it should not return twice via two
invocations of such a continuation. Implementations are not required to
detect a violation of this restriction, but if they do, an exception
with condition type `&assertion` is raised.

Choose `letrec` over `let` or `let*` when there is a circular dependency
among the variables and their values and when the order of evaluation is
unimportant. Choose `letrec*` over `letrec` when there is a circular
dependency and the bindings need to be evaluated from left to right.

A `letrec` expression of the form

`(letrec ((var expr) ...) body1 body2 ...)`

may be expressed in terms of `let` and `set!` as

`(let ((var #f) ...)`<br>
`  (let ((temp expr) ...)`<br>
`    (set! var temp) ...`<br>
`    (let () body1 body2 ...)))`

where `temp ...` are fresh variables, i.e., ones that do not already
appear in the `letrec` expression, one for each `(var expr)` pair. The
outer `let` expression establishes the variable bindings. The initial
value given each variable is unimportant, so any value suffices in place
of `#f`. The bindings are established first so that `expr ...` may
contain occurrences of the variables, i.e., so that the expressions are
computed within the scope of the variables. The middle `let` evaluates
the values and binds them to the temporary variables, and the `set!`
expressions assign each variable to the corresponding value. The inner
`let` is present in case the body contains internal definitions.

A definition of `letrec` that uses this transformation is shown on
page [310](syntax.html#defn:letrec).

This transformation does not enforce the restriction that the `expr`
expressions must not evaluate any references of or assignments to the
variables. More elaborate transformations that enforce this restriction
and actually produce more efficient code are
possible [[31](#references)].

**syntax**: `(letrec* ((var expr) ...) body1 body2 ...)` \
 **returns:**the values of the final body expression \
 **libraries:**`(rnrs base)`, `(rnrs)`

`letrec*` is similar to `letrec`, except that `letrec*` evaluates
`expr ...` in sequence from left to right. While programs must still not
evaluate a reference to any `var` before the corresponding `expr` has
been evaluated, references to `var` may be evaluated any time
thereafter, including during the evaluation of the `expr` of any
subsequent binding.

A `letrec*` expression of the form

`(letrec* ((var expr) ...) body1 body2 ...)`

may be expressed in terms of `let` and `set!` as

`(let ((var #f) ...)`<br>
`  (set! var expr) ...`<br>
`  (let () body1 body2 ...))`

The outer `let` expression creates the bindings, each assignment
evaluates an expression and immediately sets the corresponding variable
to its value, in sequence, and the inner let evaluates the body. `let`
is used in the latter case rather than `begin` since the body may
include internal definitions as well as expressions.

`(letrec* ([sum (lambda (x)`<br>
`                 (if (zero? x)`<br>
`                     0`<br>
`                     (+ x (sum (- x 1)))))]`<br>
`          [f (lambda () (cons n n-sum))]`<br>
`          [n 15]`<br>
`          [n-sum (sum n)])`<br>
`  (f)) `$\Rightarrow$` (15 . 120) `<br>
`(letrec* ([f (lambda () (lambda () g))]`<br>
`          [g (f)])`<br>
`  (eq? (g) g)) `$\Rightarrow$` #t `<br>
`(letrec* ([g (f)]`<br>
`          [f (lambda () (lambda () g))])`<br>
`  (eq? (g) g)) `$\Rightarrow$` exception: attempt to reference undefined variable f`

### Section 4.5. Multiple Values

**syntax**: `(let-values ((formals expr) ...) body1 body2 ...)` \
 **syntax**: `(let*-values ((formals expr) ...) body1 body2 ...)` \
 **returns:**the values of the final body expression \
 **libraries:**`(rnrs base)`, `(rnrs)`

`let-values` is a convenient way to receive multiple values and bind
them to variables. It is structured like `let` but permits an arbitrary
formals list (like `lambda`) on each left-hand side. `let*-values` is
similar but performs the bindings in left-to-right order, as with
`let*`. An exception with condition type `&assertion` is raised if the
number of values returned by an `expr` is not appropriate for the
corresponding `formals`, as described in the entry for `lambda` above. A
definition of `let-values` is given on
page [310](syntax.html#fullletvalues).

`(let-values ([(a b) (values 1 2)] [c (values 1 2 3)])`<br>
`  (list a b c)) `$\Rightarrow$` (1 2 (1 2 3)) `<br>
`(let*-values ([(a b) (values 1 2)] [(a b) (values b a)])`<br>
`  (list a b)) `$\Rightarrow$` (2 1)`

### Section 4.6. Variable Definitions

**syntax**: `(define var expr)` \
 **syntax**: `(define var)` \
 **syntax**: `(define (var0 var1 ...) body1 body2 ...)` \
 **syntax**: `(define (var0 . varr) body1 body2 ...)` \
 **syntax**: `(define (var0 var1 var2 ... . varr) body1 body2 ...)` \
 **libraries:**`(rnrs base)`, `(rnrs)`

In the first form, `define` creates a new binding of `var` to the value
of `expr`. The `expr` should not return more than once. That is, it
should not return both normally and via the invocation of a continuation
obtained during its evaluation, and it should not return twice via two
invocations of such a continuation. Implementations are not required to
detect a violation of this restriction, but if they do, an exception
with condition type `&assertion` is raised.

The second form is equivalent to `(define var unspecified)`, where
`unspecified` is some unspecified value. The remaining are shorthand
forms for binding variables to procedures; they are identical to the
following definition in terms of `lambda`.

`(define var`<br>
`  (lambda formals`<br>
`    body1 body2 ...))`

where `formals` is `(var1 ...)`, `varr`, or `(var1 var2 ... . varr)` for
the third, fourth, and fifth `define` formats.

Definitions may appear at the front of a `library` body, anywhere among
the forms of a top-level-program body, and at the front of a `lambda` or
`case-lambda` body or the body of any form derived from `lambda`, e.g.,
`let`, or `letrec*`. Any body that begins with a sequence of definitions
is transformed during macro expansion into a `letrec*` expression as
described on page [292](syntax.html#body-expansion).

Syntax definitions may appear along with variable definitions wherever
variable definitions may appear; see Chapter [8](syntax.html#g133).

`(define x 3)`<br>
`x `$\Rightarrow$` 3 `<br>
`(define f`<br>
`  (lambda (x y)`<br>
`    (* (+ x y) 2)))`<br>
`(f 5 4) `$\Rightarrow$` 18 `<br>
`(define (sum-of-squares x y)`<br>
`  (+ (* x x) (* y y)))`<br>
`(sum-of-squares 3 4) `$\Rightarrow$` 25 `<br>
`(define f`<br>
`  (lambda (x)`<br>
`    (+ x 1)))`<br>
`(let ([x 2])`<br>
`  (define f`<br>
`    (lambda (y)`<br>
`      (+ y x)))`<br>
`  (f 3)) `$\Rightarrow$` 5`<br>
`(f 3) `$\Rightarrow$` 4`

A set of definitions may be grouped by enclosing them in a `begin` form.
Definitions grouped in this manner may appear wherever ordinary variable
and syntax definitions may appear. They are treated as if written
separately, i.e., without the enclosing `begin` form. This feature
allows syntactic extensions to expand into groups of definitions.

`(define-syntax multi-define-syntax`<br>
`  (syntax-rules ()`<br>
`    [(_ (var expr) ...)`<br>
`     (begin`<br>
`       (define-syntax var expr)`<br>
`       ...)]))`<br>
`(let ()`<br>
`  (define plus`<br>
`    (lambda (x y)`<br>
`        (if (zero? x)`<br>
`            y`<br>
`            (plus (sub1 x) (add1 y)))))`<br>
`  (multi-define-syntax`<br>
`    (add1 (syntax-rules () [(_ e) (+ e 1)]))`<br>
`    (sub1 (syntax-rules () [(_ e) (- e 1)])))`<br>
`  (plus 7 8)) `$\Rightarrow$` 15`

Many implementations support an interactive "top level" in which
variable and other definitions may be entered interactively or loaded
from files. The behavior of these top-level definitions is outside the
scope of the Revised^6^ Report, but as long as top-level variables are
defined before any references or assignments to them are evaluated, the
behavior is consistent across most implementations. So, for example, the
reference to `g` in the top-level definition of `f` below is okay if `g`
is not already defined, and `g` is assumed to name a variable to be
defined at some later point.

`(define f`<br>
`  (lambda (x)`<br>
`    (g x)))`

If this is then followed by a definition of `g` before `f` is evaluated,
the assumption that `g` would be defined as a variable is proven
correct, and a call to `f` works as expected.

`(define g`<br>
`  (lambda (x)`<br>
`    (+ x x)))`<br>
`(f 3) `$\Rightarrow$` 6`

If `g` were defined instead as the keyword for a syntactic extension,
the assumption that `g` would be bound as a variable is proven false,
and if `f` is not redefined before it is invoked, the implementation is
likely to raise an exception.

### Section 4.7. Assignment

**syntax**: `(set! var expr)` \
 **returns:**unspecified \
 **libraries:**`(rnrs base)`, `(rnrs)`

`set!` does not establish a new binding for `var` but rather alters the
value of an existing binding. It first evaluates `expr`, then assigns
`var` to the value of `expr`. Any subsequent reference to `var` within
the scope of the altered binding evaluates to the new value.

Assignments are not employed as frequently in Scheme as in most other
languages, but they are useful for implementing state changes.

`(define flip-flop`<br>
`  (let ([state #f])`<br>
`    (lambda ()`<br>
`      (set! state (not state))`<br>
`      state))) `<br>
`(flip-flop) `$\Rightarrow$` #t`<br>
`(flip-flop) `$\Rightarrow$` #f`<br>
`(flip-flop) `$\Rightarrow$` #t`

Assignments are also useful for caching values. The example below uses a
technique called *memoization*, in which a procedure records the values
associated with old input values so it need not recompute them, to
implement a fast version of the otherwise exponential doubly recursive
definition of the Fibonacci function (see
page [69](further.html#fibonacci)).

`(define memoize`<br>
`  (lambda (proc)`<br>
`    (let ([cache '()])`<br>
`      (lambda (x)`<br>
`        (cond`<br>
`          [(assq x cache) => cdr]`<br>
`          [else`<br>
`           (let ([ans (proc x)])`<br>
`             (set! cache (cons (cons x ans) cache))`<br>
`             ans)]))))) `<br>
`(define fibonacci`<br>
`  (memoize`<br>
`    (lambda (n)`<br>
`      (if (< n 2)`<br>
`          1`<br>
`          (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))) `<br>
`(fibonacci 100) `$\Rightarrow$` 573147844013817084101`
