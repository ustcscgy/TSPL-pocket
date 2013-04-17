Answers to Selected Exercises
=============================

**[Exercise 2.2.1]. **(page 20)\

*  a*.

`(+ (* 1.2 (- 2 1/3)) -8.7)`

*  b*.

`(/ (+ 2/3 4/9) (- 5/11 4/3))`

*  c*.

`(+ 1 (/ 1 (+ 2 (/ 1 (+ 1 1/2)))))`

*  d*.

`(* (* (* (* (* (* 1 -2) 3) -4) 5) -6) 7)` or `(* 1 -2 3 -4 5 -6 7)`

**[Exercise 2.2.2]. **(page 20)\
 See [Section 6.4].

**[Exercise 2.2.3]. **(page 20)\

*  a*.

`(car . cdr)`

*  b*.

`(this (is silly))`

*  c*.

`(is this silly?)`

*  d*.

`(+ 2 3)`

*  e*.

`(+ 2 3)`

*  f*.

`+`

*  g*.

`(2 3)`

*  h*.

`#<procedure>`

*  i*.

`cons`

*  j*.

`'cons`

*  k*.

`quote`

*  l*.

`5`

*  m*.

`5`

*  n*.

`5`

*  o*.

`5`

**[Exercise 2.2.4]. **(page 21)\

`(car (cdr (car '((a b) (c d))))) `$\Rightarrow$` b`<br>
`(car (car (cdr '((a b) (c d))))) `$\Rightarrow$` c`<br>
`(car (cdr (car (cdr '((a b) (c d)))))) `$\Rightarrow$` d`

**[Exercise 2.2.5]. **(page 21)\

` '((a . b) ((c) d) ())`

**[Exercise 2.2.6]. **(page 21)\
 ![\<graphic\>](math/tspl/50.gif)

**[Exercise 2.2.7]. **(page 21)\

`(car '((a b) (c d))) `$\Rightarrow$` (a b)`<br>
`(car (car '((a b) (c d)))) `$\Rightarrow$` a`<br>
`(cdr (car '((a b) (c d)))) `$\Rightarrow$` (b)`<br>
`(car (cdr (car '((a b) (c d))))) `$\Rightarrow$` b`<br>
`(cdr (cdr (car '((a b) (c d))))) `$\Rightarrow$` ()`<br>
`(cdr '((a b) (c d))) `$\Rightarrow$` ((c d))`<br>
`(car (cdr '((a b) (c d)))) `$\Rightarrow$` (c d)`<br>
`(car (car (cdr '((a b) (c d))))) `$\Rightarrow$` c`<br>
`(cdr (car (cdr '((a b) (c d))))) `$\Rightarrow$` (d)`<br>
`(car (cdr (car (cdr '((a b) (c d)))))) `$\Rightarrow$` d`<br>
`(cdr (cdr (car (cdr '((a b) (c d)))))) `$\Rightarrow$` ()`<br>
`(cdr (cdr '((a b) (c d)))) `$\Rightarrow$` ()`

**[Exercise 2.2.8]. **(page 21)\
 See [Section 2.3].

**[Exercise 2.3.1]. **(page 23)\


1.  Evaluate the variables `list`, `+`, `-`, `*`, and `/`, yielding the
    list, addition, subtraction, multiplication, and division
    procedures.
2.  Apply the list procedure to the addition, subtraction,
    multiplication, and division procedures, yielding a list containing
    these procedures in order.
3.  Evaluate the variable `cdr`, yielding the cdr procedure.
4.  Apply the cdr procedure to the list produced in
    step 2, yielding a list containing the
    subtraction, multiplication, and division procedures.
5.  Evaluate the variable `car`, yielding the car procedure.
6.  Apply the car procedure to the list produced in
    step 4, yielding the subtraction procedure.
7.  Evaluate the constants `17` and `5`, yielding `17` and `5`.
8.  Apply the subtraction procedure to `17` and `5`, yielding `12`.

Other orders are possible. For example, the variable `car` could have
been evaluated before its argument.

**[Exercise 2.4.1]. **(page 25)\

*  a*.

`(let ([x (* 3 a)]) (+ (- x b) (+ x b)))`

*  b*.

`(let ([x (list a b c)]) (cons (car x) (cdr x)))`

**[Exercise 2.4.2]. **(page 25)\
 The value is 54. The outer `let` binds `x` to 9, while the inner `let`
binds `x` to 3 (9/3). The inner `let` evaluates to 6 (3 + 3), and the
outer `let` evaluates to 54 (9 × 6).

**[Exercise 2.4.3]. **(page 26)\

*  a*.

`(let ([x0 'a] [y0 'b])`<br>
`  (list (let ([x1 'c]) (cons x1 y0))`<br>
`        (let ([y1 'd]) (cons x0 y1))))`

*  b*.

`(let ([x0 '((a b) c)])`<br>
`  (cons (let ([x1 (cdr x0)])`<br>
`          (car x1))`<br>
`        (let ([x2 (car x0)])`<br>
`          (cons (let ([x3 (cdr x2)])`<br>
`                  (car x3))`<br>
`                (cons (let ([x4 (car x2)])`<br>
`                        x4)`<br>
`                      (cdr x2))))))`

**[Exercise 2.5.1]. **(page 30)\

*  a*.

`a`

*  b*.

`(a)`

*  c*.

`a`

*  d*.

`()`

**[Exercise 2.5.2]. **(page 30)\
 See [page 31].

**[Exercise 2.5.3]. **(page 30)\

*  a*.

no free variables

*  b*.

`+`

*  c*.

`f`

*  d*.

`cons`, `f`, and `y`

*  e*.

`cons` and `y`

*  f*.

`cons`, `y`, and `z` (`y` also appears as a bound variable)

**[Exercise 2.6.1]. **(page 34)\
 The program would loop indefinitely.

**[Exercise 2.6.2]. **(page 34)\

`(define compose`<br>
`  (lambda (p1 p2)`<br>
`    (lambda (x)`<br>
`      (p1 (p2 x))))) `<br>
`(define cadr (compose car cdr))`<br>
`(define cddr (compose cdr cdr))`

**[Exercise 2.6.3]. **(page 34)\

`(define caar (compose car car))`<br>
`(define cadr (compose car cdr)) `<br>
`(define cdar (compose cdr car))`<br>
`(define cddr (compose cdr cdr)) `<br>
`(define caaar (compose car caar))`<br>
`(define caadr (compose car cadr))`<br>
`(define cadar (compose car cdar))`<br>
`(define caddr (compose car cddr)) `<br>
`(define cdaar (compose cdr caar))`<br>
`(define cdadr (compose cdr cadr))`<br>
`(define cddar (compose cdr cdar))`<br>
`(define cdddr (compose cdr cddr)) `<br>
`(define caaaar (compose caar caar))`<br>
`(define caaadr (compose caar cadr))`<br>
`(define caadar (compose caar cdar))`<br>
`(define caaddr (compose caar cddr))`<br>
`(define cadaar (compose cadr caar))`<br>
`(define cadadr (compose cadr cadr))`<br>
`(define caddar (compose cadr cdar))`<br>
`(define cadddr (compose cadr cddr)) `<br>
`(define cdaaar (compose cdar caar))`<br>
`(define cdaadr (compose cdar cadr))`<br>
`(define cdadar (compose cdar cdar))`<br>
`(define cdaddr (compose cdar cddr))`<br>
`(define cddaar (compose cddr caar))`<br>
`(define cddadr (compose cddr cadr))`<br>
`(define cdddar (compose cddr cdar))`<br>
`(define cddddr (compose cddr cddr))`

**[Exercise 2.7.1]. **(page 41)\

`(define atom?`<br>
`  (lambda (x)`<br>
`    (not (pair? x))))`

**[Exercise 2.7.2]. **(page 41)\

`(define shorter`<br>
`  (lambda (ls1 ls2)`<br>
`    (if (< (length ls2) (length ls1))`<br>
`        ls2`<br>
`        ls1)))`

**[Exercise 2.8.1]. **(page 46)\
 The structure of the output would be the mirror image of the structure
of the input. For example, `(a . b)` would become `(b . a)` and
`((a . b) . (c . d))` would become `((d . c) . (b . a))`.

**[Exercise 2.8.2]. **(page 46)\

`(define append`<br>
`  (lambda (ls1 ls2)`<br>
`    (if (null? ls1)`<br>
`        ls2`<br>
`        (cons (car ls1) (append (cdr ls1) ls2)))))`

**[Exercise 2.8.3]. **(page 46)\

`(define make-list`<br>
`  (lambda (n x)`<br>
`    (if (= n 0)`<br>
`        '()`<br>
`        (cons x (make-list (- n 1) x)))))`

**[Exercise 2.8.4]. **(page 47)\
 See the description of `list-ref` on
[page 160] and the description of
`list-tail` on [page 160].

**[Exercise 2.8.5]. **(page 47)\

`(define shorter?`<br>
`  (lambda (ls1 ls2)`<br>
`    (and (not (null? ls2))`<br>
`         (or (null? ls1)`<br>
`             (shorter? (cdr ls1) (cdr ls2)))))) `<br>
`(define shorter`<br>
`  (lambda (ls1 ls2)`<br>
`    (if (shorter? ls2 ls1)`<br>
`        ls2`<br>
`        ls1)))`

**[Exercise 2.8.6]. **(page 47)\

`(define even?`<br>
`  (lambda (x)`<br>
`    (or (= x 0)`<br>
`        (odd? (- x 1)))))`<br>
`(define odd?`<br>
`  (lambda (x)`<br>
`    (and (not (= x 0))`<br>
`         (even? (- x 1)))))`

**[Exercise 2.8.7]. **(page 47)\

`(define transpose`<br>
`  (lambda (ls)`<br>
`    (cons (map car ls) (map cdr ls))))`

**[Exercise 2.9.1]. **(page 54)\

`(define make-counter`<br>
`  (lambda (init incr)`<br>
`    (let ([next init])`<br>
`      (lambda ()`<br>
`        (let ([v next])`<br>
`          (set! next (+ next incr))`<br>
`          v)))))`

**[Exercise 2.9.2]. **(page 55)\

`(define make-stack`<br>
`  (lambda ()`<br>
`    (let ([ls '()])`<br>
`      (lambda (msg . args)`<br>
`        (case msg`<br>
`          [(empty? mt?) (null? ls)]`<br>
`          [(push!) (set! ls (cons (car args) ls))]`<br>
`          [(top) (car ls)]`<br>
`          [(pop!) (set! ls (cdr ls))]`<br>
`          [else "oops"])))))`

**[Exercise 2.9.3]. **(page 55)\

`(define make-stack`<br>
`  (lambda ()`<br>
`    (let ([ls '()])`<br>
`      (lambda (msg . args)`<br>
`        (case msg`<br>
`          [(empty? mt?) (null? ls)]`<br>
`          [(push!) (set! ls (cons (car args) ls))]`<br>
`          [(top) (car ls)]`<br>
`          [(pop!) (set! ls (cdr ls))]`<br>
`          [(ref) (list-ref ls (car args))]`<br>
`          [(set!) (set-car! (list-tail ls (car args)) (cadr args))]`<br>
`          [else "oops"])))))`

**[Exercise 2.9.4]. **(page 55)\

`(define make-stack`<br>
`  (lambda (n)`<br>
`    (let ([v (make-vector n)] [i -1])`<br>
`      (lambda (msg . args)`<br>
`        (case msg`<br>
`          [(empty? mt?) (= i -1)]`<br>
`          [(push!)`<br>
`           (set! i (+ i 1))`<br>
`           (vector-set! v i (car args))]`<br>
`          [(top) (vector-ref v i)]`<br>
`          [(pop!) (set! i (- i 1))]`<br>
`          [(ref) (vector-ref v (- i (car args)))]`<br>
`          [(set!) (vector-set! v (- i (car args)) (cadr args))]`<br>
`          [else "oops"])))))`

**[Exercise 2.9.5]. **(page 56)\

`(define emptyq?`<br>
`  (lambda (q)`<br>
`    (eq? (car q) (cdr q)))) `<br>
`(define getq`<br>
`  (lambda (q)`<br>
`    (if (emptyq? q)`<br>
`        (assertion-violation 'getq "the queue is empty")`<br>
`        (car (car q))))) `<br>
`(define delq!`<br>
`  (lambda (q)`<br>
`    (if (emptyq? q)`<br>
`        (assertion-violation 'delq! "the queue is empty")`<br>
`        (set-car! q (cdr (car q))))))`

**[Exercise 2.9.6]. **(page 56)\

`(define make-queue`<br>
`  (lambda ()`<br>
`    (cons '() '()))) `<br>
`(define putq!`<br>
`  (lambda (q v)`<br>
`    (let ([p (cons v '())])`<br>
`      (if (null? (car q))`<br>
`          (begin`<br>
`            (set-car! q p)`<br>
`            (set-cdr! q p))`<br>
`          (begin`<br>
`            (set-cdr! (cdr q) p)`<br>
`            (set-cdr! q p)))))) `<br>
`(define getq`<br>
`  (lambda (q)`<br>
`    (car (car q)))) `<br>
`(define delq!`<br>
`  (lambda (q)`<br>
`    (if (eq? (car q) (cdr q))`<br>
`        (begin`<br>
`          (set-car! q '())`<br>
`          (set-cdr! q '()))`<br>
`        (set-car! q (cdr (car q))))))`

**[Exercise 2.9.7]. **(page 56)\
 When asked to print a cyclic structure, some implementations print a
representation of the output that reflects its cyclic structure. Other
implementations do not detect the cycle and produce either no output or
an infinite stream of output. When `length` is passed a cyclic list, an
exception is raised, likely with a message indicating that the list is
not proper. The definition of `length` on
[page 42] will, however, simply loop
indefinitely.

**[Exercise 2.9.8]. **(page 56)\

`(define race`<br>
`  (lambda (hare tortoise)`<br>
`    (if (pair? hare)`<br>
`        (let ([hare (cdr hare)])`<br>
`          (if (pair? hare)`<br>
`              (and (not (eq? hare tortoise))`<br>
`                   (race (cdr hare) (cdr tortoise)))`<br>
`              (null? hare)))`<br>
`        (null? hare)))) `<br>
`(define list?`<br>
`  (lambda (x)`<br>
`    (race x x)))`

**[Exercise 3.1.1]. **(page 64)\

`(let ([x (memv 'a ls)]) (and x (memv 'b x))) `<br>
`  ((lambda (x) (and x (memv 'b x))) (memv 'a ls)) `<br>
`  ((lambda (x) (if x (and (memv 'b x)) #f)) (memv 'a ls)) `<br>
`  ((lambda (x) (if x (memv 'b x) #f)) (memv 'a ls))`

**[Exercise 3.1.2]. **(page 64)\

`(or (memv x '(a b c)) (list x)) `<br>
`  (let ((t (memv x '(a b c)))) (if t t (or (list x)))) `<br>
`  ((lambda (t) (if t t (or (list x)))) (memv x '(a b c))) `<br>
`  ((lambda (t) (if t t (list x))) (memv x '(a b c)))`

**[Exercise 3.1.3]. **(page 64)\
 See [page 97].

**[Exercise 3.1.4]. **(page 64)\

`(define-syntax when`<br>
`  (syntax-rules ()`<br>
`    [(_ e0 e1 e2 ...)`<br>
`     (if e0 (begin e1 e2 ...))])) `<br>
`(define-syntax unless`<br>
`  (syntax-rules ()`<br>
`    [(_ e0 e1 e2 ...)`<br>
`     (when (not e0) e1 e2 ...)]))`

**[Exercise 3.2.1]. **(page 72)\
 Tail-recursive: `even?` and `odd?`, `race`, `fact` in second definition
of `factorial`, `fib` in second version of `fibonacci`.
Nontail-recursive: `sum`, `factorial`, `fib` in first version of
`fibonacci`. Both: `factor`.

**[Exercise 3.2.2]. **(page 72)\

`(define factor`<br>
`  (lambda (n)`<br>
`    (letrec ([f (lambda (n i)`<br>
`                  (cond`<br>
`                    [(>= i n) (list n)]`<br>
`                    [(integer? (/ n i))`<br>
`                     (cons i (f (/ n i) i))]`<br>
`                    [else (f n (+ i 1))]))])`<br>
`      (f n 2))))`

**[Exercise 3.2.3]. **(page 72)\
 Yes, but we need two named `let` expressions, one for `even?` and one
for `odd?`.

`(let even? ([x 20])`<br>
`  (or (= x 0)`<br>
`      (let odd? ([x (- x 1)])`<br>
`        (and (not (= x 0))`<br>
`             (even? (- x 1))))))`

**[Exercise 3.2.4]. **(page 72)\

`(define fibcount1 0)`<br>
`(define fibonacci1`<br>
`  (lambda (n)`<br>
`    (let fib ([i n])`<br>
`      (set! fibcount1 (+ fibcount1 1))`<br>
`      (cond`<br>
`        [(= i 0) 0]`<br>
`        [(= i 1) 1]`<br>
`        [else (+ (fib (- i 1)) (fib (- i 2)))])))) `<br>
`(define fibcount2 0)`<br>
`(define fibonacci2`<br>
`  (lambda (n)`<br>
`    (if (= n 0)`<br>
`        0`<br>
`        (let fib ([i n] [a1 1] [a2 0])`<br>
`          (set! fibcount2 (+ fibcount2 1))`<br>
`          (if (= i 1)`<br>
`              a1`<br>
`              (fib (- i 1) (+ a1 a2) a1))))))`

The counts for `(fibonacci 10)` are 177 and 10, for `(fibonacci 20)` are
21891 and 20, and for `(fibonacci 30)` are 2692537 and 30. While the
number of calls made by the second is directly proportional to the
input, the number of calls made by the first grows rapidly
(exponentially, in fact) as the input value increases.

**[Exercise 3.2.5]. **(page 73)\
 See [page 312].

**[Exercise 3.2.6]. **(page 73)\
 A call in the last subexpression of an `or` expression in tail position
would not be a tail call with the modified definition of `or`. For the
`even?`/`odd?` example, the resulting definition of `even?` would no
longer be tail-recursive and for very large inputs might exhaust
available space.

The expansion performed by this definition is incorrect in another way,
which has to do with multiple return values
([Section 5.8]): if the last subexpression returns
multiple values, the `or` expression should return multiple values, but
with the incorrect definition, each subexpression appears on the
right-hand side of a `let`, which expects a single return value. The
simpler and incorrect definition of `and` has the same problem.

**[Exercise 3.2.7]. **(page 73)\
 The first of the three versions of `factor` below directly addresses
the identified problems by stopping at ![\<graphic\>](math/tspl/10.gif),
avoiding the redundant division, and skipping the even factors after 2.
Stopping at ![\<graphic\>](math/tspl/10.gif) probably yields the biggest
savings, followed by skipping even factors greater than 2. Avoiding the
redundant division is less important, since it occurs only when a factor
is found.

`(define factor`<br>
`  (lambda (n)`<br>
`    (let f ([n n] [i 2] [step 1])`<br>
`      (if (> i (sqrt n))`<br>
`          (list n)`<br>
`          (let ([n/i (/ n i)])`<br>
`            (if (integer? n/i)`<br>
`                (cons i (f n/i i step))`<br>
`                (f n (+ i step) 2)))))))`

The second version replaces `(> i (sqrt n))` with `(> (* i i) n)`, since
`*` is typically much faster than `sqrt`.

`(define factor`<br>
`  (lambda (n)`<br>
`    (let f ([n n] [i 2] [step 1])`<br>
`      (if (> (* i i) n)`<br>
`          (list n)`<br>
`          (let ([n/i (/ n i)])`<br>
`            (if (integer? n/i)`<br>
`                (cons i (f n/i i step))`<br>
`                (f n (+ i step) 2)))))))`

The third version uses `gcd` (see [page 179]) to
avoid most of the divisions, since `gcd` should be faster than `/`.

`(define factor`<br>
`  (lambda (n)`<br>
`    (let f ([n n] [i 2] [step 1])`<br>
`      (if (> (* i i) n)`<br>
`          (list n)`<br>
`          (if (= (gcd n i) 1)`<br>
`              (f n (+ i step) 2)`<br>
`              (cons i (f (/ n i) i step)))))))`

To see the difference these changes make, time each version of `factor`,
including the original, in your Scheme system to see which performs
better. Try a variety of inputs, including larger ones like
`(+ (expt 2 100) 1)`.

**[Exercise 3.3.1]. **(page 77)\

`(let ([k.n (call/cc (lambda (k) (cons k 0)))])`<br>
`  (let ([k (car k.n)] [n (cdr k.n)])`<br>
`    (write n)`<br>
`    (newline)`<br>
`    (k (cons k (+ n 1)))))`

Or with multiple values (see [Section 5.8]):

`(call-with-values`<br>
`  (lambda () (call/cc (lambda (k) (values k 0))))`<br>
`  (lambda (k n)`<br>
`    (write n)`<br>
`    (newline)`<br>
`    (k k (+ n 1))))`

**[Exercise 3.3.2]. **(page 77)\

`(define product`<br>
`  (lambda (ls)`<br>
`    (if (null? ls)`<br>
`        1`<br>
`        (if (= (car ls) 0)`<br>
`            0`<br>
`            (let ([n (product (cdr ls))])`<br>
`              (if (= n 0) 0 (* n (car ls))))))))`

**[Exercise 3.3.3]. **(page 77)\
 If one of the processes returns without calling `pause`, it returns to
the call to `pause` that first caused it to run, or to the original call
to `start` if it was the first process in the list. Here is a
reimplementation of the system that allows a process to `quit`
explicitly. If other processes are active, the `lwp` system continues to
run. Otherwise, control returns to the continuation of the original call
to `start`.

`(define lwp-list '())`<br>
`(define lwp`<br>
`  (lambda (thunk)`<br>
`    (set! lwp-list (append lwp-list (list thunk)))))`<br>
`(define start`<br>
`  (lambda ()`<br>
`    (call/cc`<br>
`      (lambda (k)`<br>
`        (set! quit-k k)`<br>
`        (next)))))`<br>
`(define next`<br>
`  (lambda ()`<br>
`    (let ([p (car lwp-list)])`<br>
`      (set! lwp-list (cdr lwp-list))`<br>
`      (p))))`<br>
`(define pause`<br>
`  (lambda ()`<br>
`    (call/cc`<br>
`      (lambda (k)`<br>
`        (lwp (lambda () (k #f)))`<br>
`        (next)))))`<br>
`(define quit`<br>
`  (lambda (v)`<br>
`    (if (null? lwp-list)`<br>
`        (quit-k v)`<br>
`        (next))))`

**[Exercise 3.3.4]. **(page 77)\

`(define lwp-queue (make-queue))`<br>
`(define lwp`<br>
`  (lambda (thunk)`<br>
`    (putq! lwp-queue thunk)))`<br>
`(define start`<br>
`  (lambda ()`<br>
`    (let ([p (getq lwp-queue)])`<br>
`      (delq! lwp-queue)`<br>
`      (p))))`<br>
`(define pause`<br>
`  (lambda ()`<br>
`    (call/cc`<br>
`      (lambda (k)`<br>
`        (lwp (lambda () (k #f)))`<br>
`        (start)))))`

**[Exercise 3.4.1]. **(page 80)\

`(define reciprocal`<br>
`  (lambda (n success failure)`<br>
`    (if (= n 0)`<br>
`        (failure)`<br>
`        (success (/ 1 n)))))`

**[Exercise 3.4.2]. **(page 80)\

`(define retry #f) `<br>
`(define factorial`<br>
`  (lambda (x)`<br>
`    (let f ([x x] [k (lambda (x) x)])`<br>
`      (if (= x 0)`<br>
`          (begin (set! retry k) (k 1))`<br>
`          (f (- x 1) (lambda (y) (k (* x y))))))))`

**[Exercise 3.4.3]. **(page 80)\

`(define map/k`<br>
`  (lambda (p ls k)`<br>
`    (if (null? ls)`<br>
`        (k '())`<br>
`        (p (car ls)`<br>
`           (lambda (x)`<br>
`             (map/k p (cdr ls)`<br>
`               (lambda (ls)`<br>
`                 (k (cons x ls))))))))) `<br>
`(define reciprocals`<br>
`  (lambda (ls)`<br>
`    (map/k (lambda (x k) (if (= x 0) "zero found" (k (/ 1 x))))`<br>
`           ls`<br>
`           (lambda (x) x))))`

**[Exercise 3.5.1]. **(page 85)\

`(define-syntax complain`<br>
`  (syntax-rules ()`<br>
`    [(_ ek msg expr) (ek (list msg expr))]))`

**[Exercise 3.5.2]. **(page 85)\

`(define calc`<br>
`  (lambda (expr)`<br>
`    (call/cc`<br>
`      (lambda (ek)`<br>
`        (define do-calc`<br>
`          (lambda (expr)`<br>
`            (cond`<br>
`              [(number? expr) expr]`<br>
`              [(and (list? expr) (= (length expr) 3))`<br>
`               (let ([op (car expr)] [args (cdr expr)])`<br>
`                 (case op`<br>
`                   [(add) (apply-op + args)]`<br>
`                   [(sub) (apply-op - args)]`<br>
`                   [(mul) (apply-op * args)]`<br>
`                   [(div) (apply-op / args)]`<br>
`                   [else (complain "invalid operator" op)]))]`<br>
`              [else (complain "invalid expression" expr)])))`<br>
`        (define apply-op`<br>
`          (lambda (op args)`<br>
`            (op (do-calc (car args)) (do-calc (cadr args)))))`<br>
`        (define complain`<br>
`          (lambda (msg expr)`<br>
`            (ek (list msg expr))))`<br>
`        (do-calc expr)))))`

**[Exercise 3.5.3]. **(page 85)\

`(define calc #f)`<br>
`(let ()`<br>
`  (define do-calc`<br>
`    (lambda (expr)`<br>
`      (cond`<br>
`        [(number? expr) expr]`<br>
`        [(and (list? expr) (= (length expr) 3))`<br>
`         (let ([op (car expr)] [args (cdr expr)])`<br>
`           (case op`<br>
`             [(add) (apply-op + args)]`<br>
`             [(sub) (apply-op - args)]`<br>
`             [(mul) (apply-op * args)]`<br>
`             [(div) (apply-op / args)]`<br>
`             [else (complain "invalid operator" op)]))]`<br>
`        [else (complain "invalid expression" expr)])))`<br>
`  (define apply-op`<br>
`    (lambda (op args)`<br>
`      (op (do-calc (car args)) (do-calc (cadr args)))))`<br>
`  (define complain`<br>
`    (lambda (msg expr)`<br>
`      (assertion-violation 'calc msg expr)))`<br>
`  (set! calc`<br>
`    (lambda (expr)`<br>
`      (do-calc expr))))`

**[Exercise 3.5.4]. **(page 85)\
 This adds `sqrt`, `times` (an alias for `mul`), and `expt` along with
`minus`.

`(let ()`<br>
`  (define do-calc`<br>
`    (lambda (ek expr)`<br>
`      (cond`<br>
`        [(number? expr) expr]`<br>
`        [(and (list? expr) (= (length expr) 2))`<br>
`         (let ([op (car expr)] [args (cdr expr)])`<br>
`           (case op`<br>
`             [(minus) (apply-op1 ek - args)]`<br>
`             [(sqrt) (apply-op1 ek sqrt args)]`<br>
`             [else (complain ek "invalid unary operator" op)]))]`<br>
`        [(and (list? expr) (= (length expr) 3))`<br>
`         (let ([op (car expr)] [args (cdr expr)])`<br>
`           (case op`<br>
`             [(add) (apply-op2 ek + args)]`<br>
`             [(sub) (apply-op2 ek - args)]`<br>
`             [(mul times) (apply-op2 ek * args)]`<br>
`             [(div) (apply-op2 ek / args)]`<br>
`             [(expt) (apply-op2 ek expt args)]`<br>
`             [else (complain ek "invalid binary operator" op)]))]`<br>
`        [else (complain ek "invalid expression" expr)])))`<br>
`  (define apply-op1`<br>
`    (lambda (ek op args)`<br>
`      (op (do-calc ek (car args)))))`<br>
`  (define apply-op2`<br>
`    (lambda (ek op args)`<br>
`      (op (do-calc ek (car args)) (do-calc ek (cadr args)))))`<br>
`  (define complain`<br>
`    (lambda (ek msg expr)`<br>
`      (ek (list msg expr))))`<br>
`  (set! calc`<br>
`    (lambda (expr)`<br>
`      (call/cc`<br>
`        (lambda (ek)`<br>
`          (do-calc ek expr))))))`

**[Exercise 3.6.1]. **(page 87)\
 This version of `gpa` returns `x` when all of the input letter grades
are `x`.

`(define-syntax gpa`<br>
`  (syntax-rules ()`<br>
`    [(_ g1 g2 ...)`<br>
`     (let ([ls (map letter->number (remq 'x '(g1 g2 ...)))])`<br>
`       (if (null? ls)`<br>
`           'x`<br>
`           (/ (apply + ls) (length ls))))]))`

**[Exercise 3.6.2]. **(page 87)\
 After defining `$distribution` and `distribution` within the library as
follows:

`(define $distribution`<br>
`  (lambda (ls)`<br>
`    (let loop ([ls ls] [a 0] [b 0] [c 0] [d 0] [f 0])`<br>
`      (if (null? ls)`<br>
`          (list (list a 'a) (list b 'b) (list c 'c)`<br>
`            (list d 'd) (list f 'f))`<br>
`          (case (car ls)`<br>
`            [(a) (loop (cdr ls) (+ a 1) b c d f)]`<br>
`            [(b) (loop (cdr ls) a (+ b 1) c d f)]`<br>
`            [(c) (loop (cdr ls) a b (+ c 1) d f)]`<br>
`            [(d) (loop (cdr ls) a b c (+ d 1) f)]`<br>
`            [(f) (loop (cdr ls) a b c d (+ f 1))]`<br>
`           ; ignore x grades, per preceding exercise`<br>
`            [(x) (loop (cdr ls) a b c d f)]`<br>
`            [else (assertion-violation 'distribution`<br>
`                    "unrecognized grade letter"`<br>
`                    (car ls))])))))`<br>
`(define-syntax distribution`<br>
`  (syntax-rules ()`<br>
`    [(_ g1 g2 ...)`<br>
`     ($distribution '(g1 g2 ...))]))`

modify the `export` line to add `distribution` (but not
`$distribution`).

**[Exercise 3.6.3]. **(page 87)\
 After defining `histogram` as follows:

`(define histogram`<br>
`  (lambda (port distr)`<br>
`    (for-each`<br>
`      (lambda (n g)`<br>
`        (put-datum port g)`<br>
`        (put-string port ": ")`<br>
`        (let loop ([n n])`<br>
`          (unless (= n 0)`<br>
`            (put-char port #\*)`<br>
`            (loop (- n 1))))`<br>
`        (put-string port "\n"))`<br>
`      (map car distr)`<br>
`      (map cadr distr))))`

modify the `export` line to add `histogram`. The solution uses
`for-each`, which is described on [page 118].
