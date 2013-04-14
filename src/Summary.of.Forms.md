Summary of Forms
================

The table that follows summarizes the Scheme syntactic forms and
procedures described in Chapters [4](binding.html#g88)
through [11](exceptions.html#g147). It shows the category of the form
and the page number where it is defined. The category states whether the
form describes a syntactic form or a procedure.

All page numbers appearing here refer to the printed version of this
book and also serve as hypertext links to the corresponding locations in
the electronic version of this book.

Form

Category

Page

* * * * *

`'obj`

syntax

[141](objects.html#./objects:s2)

`` `obj ``

syntax

[142](objects.html#./objects:s5)

`,obj`

syntax

[142](objects.html#./objects:s5)

`,@obj`

syntax

[142](objects.html#./objects:s5)

`=>`

syntax

[112](control.html#./control:s16)

`_`

syntax

[297](syntax.html#./syntax:s26)

`...`

syntax

[297](syntax.html#./syntax:s26)

`#'template`

syntax

[300](syntax.html#./syntax:s33)

`` #`template ``

syntax

[305](syntax.html#./syntax:s40)

`#,template`

syntax

[305](syntax.html#./syntax:s40)

`#,@template`

syntax

[305](syntax.html#./syntax:s40)

`&assertion`

syntax

[366](exceptions.html#./exceptions:s21)

`&condition`

syntax

[362](exceptions.html#./exceptions:s13)

`&error`

syntax

[367](exceptions.html#./exceptions:s22)

`&i/o`

syntax

[371](exceptions.html#./exceptions:s32)

`&i/o-decoding`

syntax

[375](exceptions.html#./exceptions:s42)

`&i/o-encoding`

syntax

[376](exceptions.html#./exceptions:s43)

`&i/o-file-already-exists`

syntax

[374](exceptions.html#./exceptions:s39)

`&i/o-file-does-not-exist`

syntax

[374](exceptions.html#./exceptions:s40)

`&i/o-file-is-read-only`

syntax

[374](exceptions.html#./exceptions:s38)

`&i/o-file-protection`

syntax

[373](exceptions.html#./exceptions:s37)

`&i/o-filename`

syntax

[373](exceptions.html#./exceptions:s36)

`&i/o-invalid-position`

syntax

[372](exceptions.html#./exceptions:s35)

`&i/o-port`

syntax

[375](exceptions.html#./exceptions:s41)

`&i/o-read`

syntax

[372](exceptions.html#./exceptions:s33)

`&i/o-write`

syntax

[372](exceptions.html#./exceptions:s34)

`&implementation-restriction`

syntax

[369](exceptions.html#./exceptions:s28)

`&irritants`

syntax

[368](exceptions.html#./exceptions:s25)

`&lexical`

syntax

[370](exceptions.html#./exceptions:s29)

`&message`

syntax

[368](exceptions.html#./exceptions:s24)

`&no-infinities`

syntax

[376](exceptions.html#./exceptions:s44)

`&no-nans`

syntax

[377](exceptions.html#./exceptions:s45)

`&non-continuable`

syntax

[369](exceptions.html#./exceptions:s27)

`&serious`

syntax

[366](exceptions.html#./exceptions:s19)

`&syntax`

syntax

[370](exceptions.html#./exceptions:s30)

`&undefined`

syntax

[371](exceptions.html#./exceptions:s31)

`&violation`

syntax

[366](exceptions.html#./exceptions:s20)

`&warning`

syntax

[367](exceptions.html#./exceptions:s23)

`&who`

syntax

[369](exceptions.html#./exceptions:s26)

`(* num ...)`

procedure

[172](objects.html#./objects:s91)

`(+ num ...)`

procedure

[171](objects.html#./objects:s89)

`(- num)`

procedure

[172](objects.html#./objects:s90)

`(- num1 num2 num3 ...)`

procedure

[172](objects.html#./objects:s90)

`(/ num)`

procedure

[172](objects.html#./objects:s92)

`(/ num1 num2 num3 ...)`

procedure

[172](objects.html#./objects:s92)

`(< real1 real2 real3 ...)`

procedure

[170](objects.html#./objects:s88)

`(<= real1 real2 real3 ...)`

procedure

[170](objects.html#./objects:s88)

`(= num1 num2 num3 ...)`

procedure

[170](objects.html#./objects:s88)

`(> real1 real2 real3 ...)`

procedure

[170](objects.html#./objects:s88)

`(>= real1 real2 real3 ...)`

procedure

[170](objects.html#./objects:s88)

`(abs real)`

procedure

[178](objects.html#./objects:s105)

`(acos num)`

procedure

[185](objects.html#./objects:s132)

`(and expr ...)`

syntax

[110](control.html#./control:s11)

`(angle num)`

procedure

[183](objects.html#./objects:s124)

`(append)`

procedure

[160](objects.html#./objects:s49)

`(append list ... obj)`

procedure

[160](objects.html#./objects:s49)

`(apply procedure obj ... list)`

procedure

[107](control.html#./control:s3)

`(asin num)`

procedure

[185](objects.html#./objects:s132)

`(assert expression)`

syntax

[359](exceptions.html#./exceptions:s5)

`(assertion-violation who msg irritant ...)`

procedure

[358](exceptions.html#./exceptions:s4)

`(assertion-violation? obj)`

procedure

[366](exceptions.html#./exceptions:s21)

`(assoc obj alist)`

procedure

[165](objects.html#./objects:s58)

`(assp procedure alist)`

procedure

[166](objects.html#./objects:s60)

`(assq obj alist)`

procedure

[165](objects.html#./objects:s58)

`(assv obj alist)`

procedure

[165](objects.html#./objects:s58)

`(atan num)`

procedure

[185](objects.html#./objects:s133)

`(atan real1 real2)`

procedure

[185](objects.html#./objects:s133)

`(begin expr1 expr2 ...)`

syntax

[108](control.html#./control:s4)

`(binary-port? obj)`

procedure

[270](io.html#./io:s45)

`(bitwise-and exint ...)`

procedure

[186](objects.html#./objects:s134)

`(bitwise-arithmetic-shift exint1 exint2)`

procedure

[190](objects.html#./objects:s144)

`(bitwise-arithmetic-shift-left exint1 exint2)`

procedure

[189](objects.html#./objects:s143)

`(bitwise-arithmetic-shift-right exint1 exint2)`

procedure

[189](objects.html#./objects:s143)

`(bitwise-bit-count exint)`

procedure

[187](objects.html#./objects:s136)

`(bitwise-bit-field exint1 exint2 exint3)`

procedure

[189](objects.html#./objects:s141)

`(bitwise-bit-set? exint1 exint2)`

procedure

[188](objects.html#./objects:s139)

`(bitwise-copy-bit exint1 exint2 exint3)`

procedure

[188](objects.html#./objects:s140)

`(bitwise-copy-bit-field exint1 exint2 exint3 exint4)`

procedure

[189](objects.html#./objects:s142)

`(bitwise-first-bit-set exint)`

procedure

[187](objects.html#./objects:s138)

`(bitwise-if exint1 exint2 exint3)`

procedure

[186](objects.html#./objects:s135)

`(bitwise-ior exint ...)`

procedure

[186](objects.html#./objects:s134)

`(bitwise-length exint)`

procedure

[187](objects.html#./objects:s137)

`(bitwise-not exint)`

procedure

[186](objects.html#./objects:s134)

`(bitwise-reverse-bit-field exint1 exint2 exint3)`

procedure

[191](objects.html#./objects:s146)

`(bitwise-rotate-bit-field exint1 exint2 exint3 exint4)`

procedure

[190](objects.html#./objects:s145)

`(bitwise-xor exint ...)`

procedure

[186](objects.html#./objects:s134)

`(boolean=? boolean1 boolean2)`

procedure

[243](objects.html#./objects:s271)

`(boolean? obj)`

procedure

[150](objects.html#./objects:s14)

`(bound-identifier=? identifier1 identifier2)`

procedure

[302](syntax.html#./syntax:s37)

`(buffer-mode symbol)`

syntax

[261](io.html#./io:s27)

`(buffer-mode? obj)`

syntax

[262](io.html#./io:s28)

`(bytevector->sint-list bytevector eness size)`

procedure

[238](objects.html#./objects:s260)

`(bytevector->string bytevector transcoder)`

procedure

[286](io.html#./io:s91)

`(bytevector->u8-list bytevector)`

procedure

[232](objects.html#./objects:s252)

`(bytevector->uint-list bytevector eness size)`

procedure

[238](objects.html#./objects:s260)

`(bytevector-copy bytevector)`

procedure

[229](objects.html#./objects:s246)

`(bytevector-copy! src src-start dst dst-start n)`

procedure

[230](objects.html#./objects:s247)

`(bytevector-fill! bytevector fill)`

procedure

[229](objects.html#./objects:s245)

`(bytevector-ieee-double-native-ref bytevector n)`

procedure

[239](objects.html#./objects:s262)

`(bytevector-ieee-double-native-set! bytevector n x)`

procedure

[239](objects.html#./objects:s263)

`(bytevector-ieee-double-ref bytevector n eness)`

procedure

[240](objects.html#./objects:s264)

`(bytevector-ieee-double-set! bytevector n x eness)`

procedure

[240](objects.html#./objects:s265)

`(bytevector-ieee-single-native-ref bytevector n)`

procedure

[239](objects.html#./objects:s262)

`(bytevector-ieee-single-native-set! bytevector n x)`

procedure

[239](objects.html#./objects:s263)

`(bytevector-ieee-single-ref bytevector n eness)`

procedure

[240](objects.html#./objects:s264)

`(bytevector-ieee-single-set! bytevector n x eness)`

procedure

[240](objects.html#./objects:s265)

`(bytevector-length bytevector)`

procedure

[229](objects.html#./objects:s243)

`(bytevector-s16-native-ref bytevector n)`

procedure

[232](objects.html#./objects:s254)

`(bytevector-s16-native-set! bytevector n s16)`

procedure

[233](objects.html#./objects:s255)

`(bytevector-s16-ref bytevector n eness)`

procedure

[235](objects.html#./objects:s256)

`(bytevector-s16-set! bytevector n s16 eness)`

procedure

[236](objects.html#./objects:s257)

`(bytevector-s32-native-ref bytevector n)`

procedure

[232](objects.html#./objects:s254)

`(bytevector-s32-native-set! bytevector n s32)`

procedure

[233](objects.html#./objects:s255)

`(bytevector-s32-ref bytevector n eness)`

procedure

[235](objects.html#./objects:s256)

`(bytevector-s32-set! bytevector n s32 eness)`

procedure

[236](objects.html#./objects:s257)

`(bytevector-s64-native-ref bytevector n)`

procedure

[232](objects.html#./objects:s254)

`(bytevector-s64-native-set! bytevector n s64)`

procedure

[233](objects.html#./objects:s255)

`(bytevector-s64-ref bytevector n eness)`

procedure

[235](objects.html#./objects:s256)

`(bytevector-s64-set! bytevector n s64 eness)`

procedure

[236](objects.html#./objects:s257)

`(bytevector-s8-ref bytevector n)`

procedure

[231](objects.html#./objects:s249)

`(bytevector-s8-set! bytevector n s8)`

procedure

[231](objects.html#./objects:s251)

`(bytevector-sint-ref bytevector n eness size)`

procedure

[237](objects.html#./objects:s258)

`(bytevector-sint-set! bytevector n sint eness size)`

procedure

[238](objects.html#./objects:s259)

`(bytevector-u16-native-ref bytevector n)`

procedure

[232](objects.html#./objects:s254)

`(bytevector-u16-native-set! bytevector n u16)`

procedure

[233](objects.html#./objects:s255)

`(bytevector-u16-ref bytevector n eness)`

procedure

[235](objects.html#./objects:s256)

`(bytevector-u16-set! bytevector n u16 eness)`

procedure

[236](objects.html#./objects:s257)

`(bytevector-u32-native-ref bytevector n)`

procedure

[232](objects.html#./objects:s254)

`(bytevector-u32-native-set! bytevector n u32)`

procedure

[233](objects.html#./objects:s255)

`(bytevector-u32-ref bytevector n eness)`

procedure

[235](objects.html#./objects:s256)

`(bytevector-u32-set! bytevector n u32 eness)`

procedure

[236](objects.html#./objects:s257)

`(bytevector-u64-native-ref bytevector n)`

procedure

[232](objects.html#./objects:s254)

`(bytevector-u64-native-set! bytevector n u64)`

procedure

[233](objects.html#./objects:s255)

`(bytevector-u64-ref bytevector n eness)`

procedure

[235](objects.html#./objects:s256)

`(bytevector-u64-set! bytevector n u64 eness)`

procedure

[236](objects.html#./objects:s257)

`(bytevector-u8-ref bytevector n)`

procedure

[230](objects.html#./objects:s248)

`(bytevector-u8-set! bytevector n u8)`

procedure

[231](objects.html#./objects:s250)

`(bytevector-uint-ref bytevector n eness size)`

procedure

[237](objects.html#./objects:s258)

`(bytevector-uint-set! bytevector n uint eness size)`

procedure

[238](objects.html#./objects:s259)

`(bytevector=? bytevector1 bytevector2)`

procedure

[229](objects.html#./objects:s244)

`(bytevector? obj)`

procedure

[155](objects.html#./objects:s24)

`(caaaar pair)`

procedure

[157](objects.html#./objects:s42)

`(caaadr pair)`

procedure

[157](objects.html#./objects:s42)

`(caaar pair)`

procedure

[157](objects.html#./objects:s42)

`(caadar pair)`

procedure

[157](objects.html#./objects:s42)

`(caaddr pair)`

procedure

[157](objects.html#./objects:s42)

`(caadr pair)`

procedure

[157](objects.html#./objects:s42)

`(caar pair)`

procedure

[157](objects.html#./objects:s42)

`(cadaar pair)`

procedure

[157](objects.html#./objects:s42)

`(cadadr pair)`

procedure

[157](objects.html#./objects:s42)

`(cadar pair)`

procedure

[157](objects.html#./objects:s42)

`(caddar pair)`

procedure

[157](objects.html#./objects:s42)

`(cadddr pair)`

procedure

[157](objects.html#./objects:s42)

`(caddr pair)`

procedure

[157](objects.html#./objects:s42)

`(cadr pair)`

procedure

[157](objects.html#./objects:s42)

`(call-with-bytevector-output-port procedure)`

procedure

[266](io.html#./io:s38)

`(call-with-bytevector-output-port procedure ?transcoder)`

procedure

[266](io.html#./io:s38)

`(call-with-current-continuation procedure)`

procedure

[123](control.html#./control:s54)

`(call-with-input-file path procedure)`

procedure

[281](io.html#./io:s77)

`(call-with-output-file path procedure)`

procedure

[282](io.html#./io:s78)

`(call-with-port port procedure)`

procedure

[272](io.html#./io:s51)

`(call-with-string-output-port procedure)`

procedure

[267](io.html#./io:s39)

`(call-with-values producer consumer)`

procedure

[131](control.html#./control:s71)

`(call/cc procedure)`

procedure

[123](control.html#./control:s54)

`(car pair)`

procedure

[156](objects.html#./objects:s38)

`(case expr0 clause1 clause2 ...)`

syntax

[113](control.html#./control:s18)

`(case-lambda clause ...)`

syntax

[94](binding.html#./binding:s13)

`(cdaaar pair)`

procedure

[157](objects.html#./objects:s42)

`(cdaadr pair)`

procedure

[157](objects.html#./objects:s42)

`(cdaar pair)`

procedure

[157](objects.html#./objects:s42)

`(cdadar pair)`

procedure

[157](objects.html#./objects:s42)

`(cdaddr pair)`

procedure

[157](objects.html#./objects:s42)

`(cdadr pair)`

procedure

[157](objects.html#./objects:s42)

`(cdar pair)`

procedure

[157](objects.html#./objects:s42)

`(cddaar pair)`

procedure

[157](objects.html#./objects:s42)

`(cddadr pair)`

procedure

[157](objects.html#./objects:s42)

`(cddar pair)`

procedure

[157](objects.html#./objects:s42)

`(cdddar pair)`

procedure

[157](objects.html#./objects:s42)

`(cddddr pair)`

procedure

[157](objects.html#./objects:s42)

`(cdddr pair)`

procedure

[157](objects.html#./objects:s42)

`(cddr pair)`

procedure

[157](objects.html#./objects:s42)

`(cdr pair)`

procedure

[156](objects.html#./objects:s39)

`(ceiling real)`

procedure

[177](objects.html#./objects:s103)

`(char->integer char)`

procedure

[215](objects.html#./objects:s210)

`(char-alphabetic? char)`

procedure

[213](objects.html#./objects:s203)

`(char-ci<=? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s202)

`(char-ci<? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s202)

`(char-ci=? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s202)

`(char-ci>=? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s202)

`(char-ci>? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s202)

`(char-downcase char)`

procedure

[214](objects.html#./objects:s207)

`(char-foldcase char)`

procedure

[215](objects.html#./objects:s209)

`(char-general-category char)`

procedure

[214](objects.html#./objects:s205)

`(char-lower-case? char)`

procedure

[213](objects.html#./objects:s204)

`(char-numeric? char)`

procedure

[213](objects.html#./objects:s203)

`(char-title-case? char)`

procedure

[213](objects.html#./objects:s204)

`(char-titlecase char)`

procedure

[214](objects.html#./objects:s208)

`(char-upcase char)`

procedure

[214](objects.html#./objects:s206)

`(char-upper-case? char)`

procedure

[213](objects.html#./objects:s204)

`(char-whitespace? char)`

procedure

[213](objects.html#./objects:s203)

`(char<=? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s201)

`(char<? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s201)

`(char=? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s201)

`(char>=? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s201)

`(char>? char1 char2 char3 ...)`

procedure

[212](objects.html#./objects:s201)

`(char? obj)`

procedure

[154](objects.html#./objects:s19)

`(close-input-port input-port)`

procedure

[285](io.html#./io:s88)

`(close-output-port output-port)`

procedure

[285](io.html#./io:s88)

`(close-port port)`

procedure

[270](io.html#./io:s46)

`(command-line)`

procedure

[350](libraries.html#./libraries:s17)

`(complex? obj)`

procedure

[151](objects.html#./objects:s17)

`(cond clause1 clause2 ...)`

syntax

[111](control.html#./control:s13)

`(condition condition ...)`

procedure

[362](exceptions.html#./exceptions:s15)

`(condition-accessor rtd procedure)`

procedure

[365](exceptions.html#./exceptions:s18)

`(condition-irritants condition)`

procedure

[368](exceptions.html#./exceptions:s25)

`(condition-message condition)`

procedure

[368](exceptions.html#./exceptions:s24)

`(condition-predicate rtd)`

procedure

[365](exceptions.html#./exceptions:s18)

`(condition-who condition)`

procedure

[369](exceptions.html#./exceptions:s26)

`(condition? obj)`

procedure

[362](exceptions.html#./exceptions:s14)

`(cons obj1 obj2)`

procedure

[156](objects.html#./objects:s37)

`(cons* obj ... final-obj)`

procedure

[158](objects.html#./objects:s44)

`constant`

syntax

[141](objects.html#./objects:s1)

`(cos num)`

procedure

[185](objects.html#./objects:s131)

`(current-error-port)`

procedure

[263](io.html#./io:s32)

`(current-input-port)`

procedure

[263](io.html#./io:s32)

`(current-output-port)`

procedure

[263](io.html#./io:s32)

`(datum->syntax template-identifier obj)`

procedure

[308](syntax.html#./syntax:s45)

`(define var expr)`

syntax

[100](binding.html#./binding:s24)

`(define var)`

syntax

[100](binding.html#./binding:s24)

`(define (var0 var1 ...) body1 body2 ...)`

syntax

[100](binding.html#./binding:s24)

`(define (var0 . varr) body1 body2 ...)`

syntax

[100](binding.html#./binding:s24)

`(define (var0 var1 var2 ... . varr) body1 body2 ...)`

syntax

[100](binding.html#./binding:s24)

`(define-condition-type name parent constructor pred field ...)`

syntax

[364](exceptions.html#./exceptions:s17)

`(define-enumeration name (symbol ...) constructor)`

syntax

[250](objects.html#./objects:s290)

`(define-record-type record-name clause ...)`

syntax

[328](records.html#./records:s13)

`(define-record-type (record-name constructor pred) clause ...)`

syntax

[328](records.html#./records:s13)

`(define-syntax keyword expr)`

syntax

[292](syntax.html#./syntax:s12)

`(delay expr)`

syntax

[128](control.html#./control:s65)

`(delete-file path)`

procedure

[286](io.html#./io:s90)

`(denominator rat)`

procedure

[181](objects.html#./objects:s119)

`(display obj)`

procedure

[285](io.html#./io:s85)

`(display obj textual-output-port)`

procedure

[285](io.html#./io:s85)

`(div x1 x2)`

procedure

[175](objects.html#./objects:s99)

`(div-and-mod x1 x2)`

procedure

[175](objects.html#./objects:s99)

`(div0 x1 x2)`

procedure

[176](objects.html#./objects:s100)

`(div0-and-mod0 x1 x2)`

procedure

[176](objects.html#./objects:s100)

`(do ((var init update) ...) (test result ...) expr ...)`

syntax

[115](control.html#./control:s25)

`(dynamic-wind in body out)`

procedure

[124](control.html#./control:s56)

`else`

syntax

[112](control.html#./control:s16)

`(endianness symbol)`

syntax

[228](objects.html#./objects:s240)

`(enum-set->list enum-set)`

procedure

[252](objects.html#./objects:s294)

`(enum-set-complement enum-set)`

procedure

[254](objects.html#./objects:s299)

`(enum-set-constructor enum-set)`

procedure

[251](objects.html#./objects:s292)

`(enum-set-difference enum-set1 enum-set2)`

procedure

[253](objects.html#./objects:s298)

`(enum-set-indexer enum-set)`

procedure

[254](objects.html#./objects:s301)

`(enum-set-intersection enum-set1 enum-set2)`

procedure

[253](objects.html#./objects:s298)

`(enum-set-member? symbol enum-set)`

procedure

[253](objects.html#./objects:s297)

`(enum-set-projection enum-set1 enum-set2)`

procedure

[254](objects.html#./objects:s300)

`(enum-set-subset? enum-set1 enum-set2)`

procedure

[252](objects.html#./objects:s295)

`(enum-set-union enum-set1 enum-set2)`

procedure

[253](objects.html#./objects:s298)

`(enum-set-universe enum-set)`

procedure

[252](objects.html#./objects:s293)

`(enum-set=? enum-set1 enum-set2)`

procedure

[252](objects.html#./objects:s296)

`(environment import-spec ...)`

procedure

[137](control.html#./control:s81)

`(eof-object)`

procedure

[273](io.html#./io:s54)

`(eof-object? obj)`

procedure

[273](io.html#./io:s53)

`(eol-style symbol)`

syntax

[259](io.html#./io:s23)

`(eq? obj1 obj2)`

procedure

[143](objects.html#./objects:s10)

`(equal-hash obj)`

procedure

[245](objects.html#./objects:s279)

`(equal? obj1 obj2)`

procedure

[148](objects.html#./objects:s13)

`(eqv? obj1 obj2)`

procedure

[146](objects.html#./objects:s12)

`(error who msg irritant ...)`

procedure

[358](exceptions.html#./exceptions:s4)

`(error-handling-mode symbol)`

syntax

[260](io.html#./io:s25)

`(error? obj)`

procedure

[367](exceptions.html#./exceptions:s22)

`(eval obj environment)`

procedure

[136](control.html#./control:s80)

`(even? int)`

procedure

[174](objects.html#./objects:s96)

`(exact num)`

procedure

[180](objects.html#./objects:s114)

`(exact->inexact num)`

procedure

[181](objects.html#./objects:s116)

`(exact-integer-sqrt n)`

procedure

[184](objects.html#./objects:s128)

`(exact? num)`

procedure

[170](objects.html#./objects:s86)

`(exists procedure list1 list2 ...)`

procedure

[119](control.html#./control:s36)

`(exit)`

procedure

[350](libraries.html#./libraries:s18)

`(exit obj)`

procedure

[350](libraries.html#./libraries:s18)

`(exp num)`

procedure

[184](objects.html#./objects:s129)

`(expt num1 num2)`

procedure

[179](objects.html#./objects:s111)

`fields`

syntax

[331](records.html#./records:s16)

`(file-exists? path)`

procedure

[286](io.html#./io:s89)

`(file-options symbol ...)`

syntax

[261](io.html#./io:s26)

`(filter procedure list)`

procedure

[164](objects.html#./objects:s55)

`(find procedure list)`

procedure

[165](objects.html#./objects:s57)

`(finite? real)`

procedure

[174](objects.html#./objects:s97)

`(fixnum->flonum fx)`

procedure

[211](objects.html#./objects:s198)

`(fixnum-width)`

procedure

[193](objects.html#./objects:s152)

`(fixnum? obj)`

procedure

[193](objects.html#./objects:s150)

`(fl* fl ...)`

procedure

[207](objects.html#./objects:s186)

`(fl+ fl ...)`

procedure

[206](objects.html#./objects:s184)

`(fl- fl)`

procedure

[206](objects.html#./objects:s185)

`(fl- fl1 fl2 fl3 ...)`

procedure

[206](objects.html#./objects:s185)

`(fl/ fl)`

procedure

[207](objects.html#./objects:s187)

`(fl/ fl1 fl2 fl3 ...)`

procedure

[207](objects.html#./objects:s187)

`(fl<=? fl1 fl2 fl3 ...)`

procedure

[203](objects.html#./objects:s178)

`(fl<? fl1 fl2 fl3 ...)`

procedure

[203](objects.html#./objects:s178)

`(fl=? fl1 fl2 fl3 ...)`

procedure

[203](objects.html#./objects:s178)

`(fl>=? fl1 fl2 fl3 ...)`

procedure

[203](objects.html#./objects:s178)

`(fl>? fl1 fl2 fl3 ...)`

procedure

[203](objects.html#./objects:s178)

`(flabs fl)`

procedure

[209](objects.html#./objects:s192)

`(flacos fl)`

procedure

[210](objects.html#./objects:s195)

`(flasin fl)`

procedure

[210](objects.html#./objects:s195)

`(flatan fl)`

procedure

[210](objects.html#./objects:s195)

`(flatan fl1 fl2)`

procedure

[210](objects.html#./objects:s195)

`(flceiling fl)`

procedure

[208](objects.html#./objects:s190)

`(flcos fl)`

procedure

[210](objects.html#./objects:s194)

`(fldenominator fl)`

procedure

[209](objects.html#./objects:s191)

`(fldiv fl1 fl2)`

procedure

[207](objects.html#./objects:s188)

`(fldiv-and-mod fl1 fl2)`

procedure

[207](objects.html#./objects:s188)

`(fldiv0 fl1 fl2)`

procedure

[208](objects.html#./objects:s189)

`(fldiv0-and-mod0 fl1 fl2)`

procedure

[208](objects.html#./objects:s189)

`(fleven? fl-int)`

procedure

[205](objects.html#./objects:s182)

`(flexp fl)`

procedure

[209](objects.html#./objects:s193)

`(flexpt fl1 fl2)`

procedure

[210](objects.html#./objects:s197)

`(flfinite? fl)`

procedure

[205](objects.html#./objects:s181)

`(flfloor fl)`

procedure

[208](objects.html#./objects:s190)

`(flinfinite? fl)`

procedure

[205](objects.html#./objects:s181)

`(flinteger? fl)`

procedure

[204](objects.html#./objects:s180)

`(fllog fl)`

procedure

[209](objects.html#./objects:s193)

`(fllog fl1 fl2)`

procedure

[209](objects.html#./objects:s193)

`(flmax fl1 fl2 ...)`

procedure

[205](objects.html#./objects:s183)

`(flmin fl1 fl2 ...)`

procedure

[205](objects.html#./objects:s183)

`(flmod fl1 fl2)`

procedure

[207](objects.html#./objects:s188)

`(flmod0 fl1 fl2)`

procedure

[208](objects.html#./objects:s189)

`(flnan? fl)`

procedure

[205](objects.html#./objects:s181)

`(flnegative? fl)`

procedure

[204](objects.html#./objects:s179)

`(flnumerator fl)`

procedure

[209](objects.html#./objects:s191)

`(flodd? fl-int)`

procedure

[205](objects.html#./objects:s182)

`(flonum? obj)`

procedure

[203](objects.html#./objects:s177)

`(floor real)`

procedure

[177](objects.html#./objects:s102)

`(flpositive? fl)`

procedure

[204](objects.html#./objects:s179)

`(flround fl)`

procedure

[208](objects.html#./objects:s190)

`(flsin fl)`

procedure

[210](objects.html#./objects:s194)

`(flsqrt fl)`

procedure

[210](objects.html#./objects:s196)

`(fltan fl)`

procedure

[210](objects.html#./objects:s194)

`(fltruncate fl)`

procedure

[208](objects.html#./objects:s190)

`(flush-output-port output-port)`

procedure

[280](io.html#./io:s74)

`(flzero? fl)`

procedure

[204](objects.html#./objects:s179)

`(fold-left procedure obj list1 list2 ...)`

procedure

[120](control.html#./control:s38)

`(fold-right procedure obj list1 list2 ...)`

procedure

[121](control.html#./control:s41)

`(for-all procedure list1 list2 ...)`

procedure

[119](control.html#./control:s37)

`(for-each procedure list1 list2 ...)`

procedure

[118](control.html#./control:s33)

`(force promise)`

procedure

[128](control.html#./control:s65)

`(free-identifier=? identifier1 identifier2)`

procedure

[302](syntax.html#./syntax:s37)

`(fx* fx1 fx2)`

procedure

[195](objects.html#./objects:s159)

`(fx*/carry fx1 fx2 fx3)`

procedure

[197](objects.html#./objects:s162)

`(fx+ fx1 fx2)`

procedure

[195](objects.html#./objects:s157)

`(fx+/carry fx1 fx2 fx3)`

procedure

[197](objects.html#./objects:s162)

`(fx- fx)`

procedure

[195](objects.html#./objects:s158)

`(fx- fx1 fx2)`

procedure

[195](objects.html#./objects:s158)

`(fx-/carry fx1 fx2 fx3)`

procedure

[197](objects.html#./objects:s162)

`(fx<=? fx1 fx2 fx3 ...)`

procedure

[193](objects.html#./objects:s153)

`(fx<? fx1 fx2 fx3 ...)`

procedure

[193](objects.html#./objects:s153)

`(fx=? fx1 fx2 fx3 ...)`

procedure

[193](objects.html#./objects:s153)

`(fx>=? fx1 fx2 fx3 ...)`

procedure

[193](objects.html#./objects:s153)

`(fx>? fx1 fx2 fx3 ...)`

procedure

[193](objects.html#./objects:s153)

`(fxand fx ...)`

procedure

[197](objects.html#./objects:s163)

`(fxarithmetic-shift fx1 fx2)`

procedure

[201](objects.html#./objects:s173)

`(fxarithmetic-shift-left fx1 fx2)`

procedure

[201](objects.html#./objects:s172)

`(fxarithmetic-shift-right fx1 fx2)`

procedure

[201](objects.html#./objects:s172)

`(fxbit-count fx)`

procedure

[198](objects.html#./objects:s165)

`(fxbit-field fx1 fx2 fx3)`

procedure

[200](objects.html#./objects:s170)

`(fxbit-set? fx1 fx2)`

procedure

[199](objects.html#./objects:s168)

`(fxcopy-bit fx1 fx2 fx3)`

procedure

[200](objects.html#./objects:s169)

`(fxcopy-bit-field fx1 fx2 fx3 fx4)`

procedure

[200](objects.html#./objects:s171)

`(fxdiv fx1 fx2)`

procedure

[196](objects.html#./objects:s160)

`(fxdiv-and-mod fx1 fx2)`

procedure

[196](objects.html#./objects:s160)

`(fxdiv0 fx1 fx2)`

procedure

[196](objects.html#./objects:s161)

`(fxdiv0-and-mod0 fx1 fx2)`

procedure

[196](objects.html#./objects:s161)

`(fxeven? fx)`

procedure

[194](objects.html#./objects:s155)

`(fxfirst-bit-set fx)`

procedure

[199](objects.html#./objects:s167)

`(fxif fx1 fx2 fx3)`

procedure

[198](objects.html#./objects:s164)

`(fxior fx ...)`

procedure

[197](objects.html#./objects:s163)

`(fxlength fx)`

procedure

[198](objects.html#./objects:s166)

`(fxmax fx1 fx2 ...)`

procedure

[195](objects.html#./objects:s156)

`(fxmin fx1 fx2 ...)`

procedure

[195](objects.html#./objects:s156)

`(fxmod fx1 fx2)`

procedure

[196](objects.html#./objects:s160)

`(fxmod0 fx1 fx2)`

procedure

[196](objects.html#./objects:s161)

`(fxnegative? fx)`

procedure

[194](objects.html#./objects:s154)

`(fxnot fx)`

procedure

[197](objects.html#./objects:s163)

`(fxodd? fx)`

procedure

[194](objects.html#./objects:s155)

`(fxpositive? fx)`

procedure

[194](objects.html#./objects:s154)

`(fxreverse-bit-field fx1 fx2 fx3)`

procedure

[202](objects.html#./objects:s175)

`(fxrotate-bit-field fx1 fx2 fx3 fx4)`

procedure

[201](objects.html#./objects:s174)

`(fxxor fx ...)`

procedure

[197](objects.html#./objects:s163)

`(fxzero? fx)`

procedure

[194](objects.html#./objects:s154)

`(gcd int ...)`

procedure

[179](objects.html#./objects:s109)

`(generate-temporaries list)`

procedure

[310](syntax.html#./syntax:s49)

`(get-bytevector-all binary-input-port)`

procedure

[275](io.html#./io:s60)

`(get-bytevector-n binary-input-port n)`

procedure

[274](io.html#./io:s57)

`(get-bytevector-n! binary-input-port bytevector start n)`

procedure

[274](io.html#./io:s58)

`(get-bytevector-some binary-input-port)`

procedure

[275](io.html#./io:s59)

`(get-char textual-input-port)`

procedure

[275](io.html#./io:s61)

`(get-datum textual-input-port)`

procedure

[278](io.html#./io:s67)

`(get-line textual-input-port)`

procedure

[277](io.html#./io:s66)

`(get-string-all textual-input-port)`

procedure

[277](io.html#./io:s65)

`(get-string-n textual-input-port n)`

procedure

[276](io.html#./io:s63)

`(get-string-n! textual-input-port string start n)`

procedure

[276](io.html#./io:s64)

`(get-u8 binary-input-port)`

procedure

[274](io.html#./io:s55)

`(greatest-fixnum)`

procedure

[193](objects.html#./objects:s151)

`(guard (var clause1 clause2 ...) b1 b2 ...)`

syntax

[361](exceptions.html#./exceptions:s8)

`(hashtable-clear! hashtable)`

procedure

[249](objects.html#./objects:s287)

`(hashtable-clear! hashtable size)`

procedure

[249](objects.html#./objects:s287)

`(hashtable-contains? hashtable key)`

procedure

[246](objects.html#./objects:s282)

`(hashtable-copy hashtable)`

procedure

[248](objects.html#./objects:s286)

`(hashtable-copy hashtable mutable?)`

procedure

[248](objects.html#./objects:s286)

`(hashtable-delete! hashtable key)`

procedure

[248](objects.html#./objects:s284)

`(hashtable-entries hashtable)`

procedure

[250](objects.html#./objects:s289)

`(hashtable-equivalence-function hashtable)`

procedure

[245](objects.html#./objects:s278)

`(hashtable-hash-function hashtable)`

procedure

[245](objects.html#./objects:s278)

`(hashtable-keys hashtable)`

procedure

[249](objects.html#./objects:s288)

`(hashtable-mutable? hashtable)`

procedure

[245](objects.html#./objects:s277)

`(hashtable-ref hashtable key default)`

procedure

[246](objects.html#./objects:s281)

`(hashtable-set! hashtable key obj)`

procedure

[246](objects.html#./objects:s280)

`(hashtable-size hashtable)`

procedure

[248](objects.html#./objects:s285)

`(hashtable-update! hashtable key procedure default)`

procedure

[247](objects.html#./objects:s283)

`(hashtable? obj)`

procedure

[155](objects.html#./objects:s25)

`(i/o-decoding-error? obj)`

procedure

[375](exceptions.html#./exceptions:s42)

`(i/o-encoding-error-char condition)`

procedure

[376](exceptions.html#./exceptions:s43)

`(i/o-encoding-error? obj)`

procedure

[376](exceptions.html#./exceptions:s43)

`(i/o-error-filename condition)`

procedure

[373](exceptions.html#./exceptions:s36)

`(i/o-error-port condition)`

procedure

[375](exceptions.html#./exceptions:s41)

`(i/o-error-position condition)`

procedure

[372](exceptions.html#./exceptions:s35)

`(i/o-error? obj)`

procedure

[371](exceptions.html#./exceptions:s32)

`(i/o-file-already-exists-error? obj)`

procedure

[374](exceptions.html#./exceptions:s39)

`(i/o-file-does-not-exist-error? obj)`

procedure

[374](exceptions.html#./exceptions:s40)

`(i/o-file-is-read-only-error? obj)`

procedure

[374](exceptions.html#./exceptions:s38)

`(i/o-file-protection-error? obj)`

procedure

[373](exceptions.html#./exceptions:s37)

`(i/o-filename-error? obj)`

procedure

[373](exceptions.html#./exceptions:s36)

`(i/o-invalid-position-error? obj)`

procedure

[372](exceptions.html#./exceptions:s35)

`(i/o-port-error? obj)`

procedure

[375](exceptions.html#./exceptions:s41)

`(i/o-read-error? obj)`

procedure

[372](exceptions.html#./exceptions:s33)

`(i/o-write-error? obj)`

procedure

[372](exceptions.html#./exceptions:s34)

`(identifier-syntax tmpl)`

syntax

[297](syntax.html#./syntax:s27)

`(identifier-syntax (id1 tmpl1) ((set! id2 e2) tmpl2))`

syntax

[297](syntax.html#./syntax:s27)

`(identifier? obj)`

procedure

[301](syntax.html#./syntax:s35)

`(if test consequent alternative)`

syntax

[109](control.html#./control:s8)

`(if test consequent)`

syntax

[109](control.html#./control:s8)

`(imag-part num)`

procedure

[182](objects.html#./objects:s121)

`immutable`

syntax

[331](records.html#./records:s16)

`(implementation-restriction-violation? obj)`

procedure

[369](exceptions.html#./exceptions:s28)

`(inexact num)`

procedure

[180](objects.html#./objects:s112)

`(inexact->exact num)`

procedure

[181](objects.html#./objects:s116)

`(inexact? num)`

procedure

[170](objects.html#./objects:s87)

`(infinite? real)`

procedure

[174](objects.html#./objects:s97)

`(input-port? obj)`

procedure

[270](io.html#./io:s44)

`(integer->char n)`

procedure

[215](objects.html#./objects:s211)

`(integer-valued? obj)`

procedure

[153](objects.html#./objects:s18)

`(integer? obj)`

procedure

[151](objects.html#./objects:s17)

`(irritants-condition? obj)`

procedure

[368](exceptions.html#./exceptions:s25)

`(lambda formals body1 body2 ...)`

syntax

[92](binding.html#./binding:s3)

`(latin-1-codec)`

procedure

[259](io.html#./io:s22)

`(lcm int ...)`

procedure

[179](objects.html#./objects:s110)

`(least-fixnum)`

procedure

[193](objects.html#./objects:s151)

`(length list)`

procedure

[159](objects.html#./objects:s46)

`(let ((var expr) ...) body1 body2 ...)`

syntax

[95](binding.html#./binding:s16)

`(let name ((var expr) ...) body1 body2 ...)`

syntax

[114](control.html#./control:s20)

`(let* ((var expr) ...) body1 body2 ...)`

syntax

[96](binding.html#./binding:s18)

`(let*-values ((formals expr) ...) body1 body2 ...)`

syntax

[99](binding.html#./binding:s23)

`(let-syntax ((keyword expr) ...) form1 form2 ...)`

syntax

[293](syntax.html#./syntax:s13)

`(let-values ((formals expr) ...) body1 body2 ...)`

syntax

[99](binding.html#./binding:s23)

`(letrec ((var expr) ...) body1 body2 ...)`

syntax

[97](binding.html#./binding:s20)

`(letrec* ((var expr) ...) body1 body2 ...)`

syntax

[98](binding.html#./binding:s22)

`(letrec-syntax ((keyword expr) ...) form1 form2 ...)`

syntax

[293](syntax.html#./syntax:s13)

`(lexical-violation? obj)`

procedure

[370](exceptions.html#./exceptions:s29)

`(list obj ...)`

procedure

[158](objects.html#./objects:s43)

`(list->string list)`

procedure

[223](objects.html#./objects:s229)

`(list->vector list)`

procedure

[226](objects.html#./objects:s238)

`(list-ref list n)`

procedure

[159](objects.html#./objects:s47)

`(list-sort predicate list)`

procedure

[167](objects.html#./objects:s62)

`(list-tail list n)`

procedure

[160](objects.html#./objects:s48)

`(list? obj)`

procedure

[158](objects.html#./objects:s45)

`(log num)`

procedure

[184](objects.html#./objects:s130)

`(log num1 num2)`

procedure

[184](objects.html#./objects:s130)

`(lookahead-char textual-input-port)`

procedure

[275](io.html#./io:s62)

`(lookahead-u8 binary-input-port)`

procedure

[274](io.html#./io:s56)

`(magnitude num)`

procedure

[183](objects.html#./objects:s125)

`(make-assertion-violation)`

procedure

[366](exceptions.html#./exceptions:s21)

`(make-bytevector n)`

procedure

[228](objects.html#./objects:s242)

`(make-bytevector n fill)`

procedure

[228](objects.html#./objects:s242)

`(make-custom-binary-input-port id r! gp sp! close)`

procedure

[267](io.html#./io:s41)

`(make-custom-binary-input/output-port id r! w! gp sp! close)`

procedure

[267](io.html#./io:s41)

`(make-custom-binary-output-port id w! gp sp! close)`

procedure

[267](io.html#./io:s41)

`(make-custom-textual-input-port id r! gp sp! close)`

procedure

[268](io.html#./io:s42)

`(make-custom-textual-input/output-port id r! w! gp sp! close)`

procedure

[268](io.html#./io:s42)

`(make-custom-textual-output-port id w! gp sp! close)`

procedure

[268](io.html#./io:s42)

`(make-enumeration symbol-list)`

procedure

[251](objects.html#./objects:s291)

`(make-eq-hashtable)`

procedure

[243](objects.html#./objects:s274)

`(make-eq-hashtable size)`

procedure

[243](objects.html#./objects:s274)

`(make-eqv-hashtable)`

procedure

[244](objects.html#./objects:s275)

`(make-eqv-hashtable size)`

procedure

[244](objects.html#./objects:s275)

`(make-error)`

procedure

[367](exceptions.html#./exceptions:s22)

`(make-hashtable hash equiv?)`

procedure

[244](objects.html#./objects:s276)

`(make-hashtable hash equiv? size)`

procedure

[244](objects.html#./objects:s276)

`(make-i/o-decoding-error pobj)`

procedure

[375](exceptions.html#./exceptions:s42)

`(make-i/o-encoding-error pobj cobj)`

procedure

[376](exceptions.html#./exceptions:s43)

`(make-i/o-error)`

procedure

[371](exceptions.html#./exceptions:s32)

`(make-i/o-file-already-exists-error filename)`

procedure

[374](exceptions.html#./exceptions:s39)

`(make-i/o-file-does-not-exist-error filename)`

procedure

[374](exceptions.html#./exceptions:s40)

`(make-i/o-file-is-read-only-error filename)`

procedure

[374](exceptions.html#./exceptions:s38)

`(make-i/o-file-protection-error filename)`

procedure

[373](exceptions.html#./exceptions:s37)

`(make-i/o-filename-error filename)`

procedure

[373](exceptions.html#./exceptions:s36)

`(make-i/o-invalid-position-error position)`

procedure

[372](exceptions.html#./exceptions:s35)

`(make-i/o-port-error pobj)`

procedure

[375](exceptions.html#./exceptions:s41)

`(make-i/o-read-error)`

procedure

[372](exceptions.html#./exceptions:s33)

`(make-i/o-write-error)`

procedure

[372](exceptions.html#./exceptions:s34)

`(make-implementation-restriction-violation)`

procedure

[369](exceptions.html#./exceptions:s28)

`(make-irritants-condition irritants)`

procedure

[368](exceptions.html#./exceptions:s25)

`(make-lexical-violation)`

procedure

[370](exceptions.html#./exceptions:s29)

`(make-message-condition message)`

procedure

[368](exceptions.html#./exceptions:s24)

`(make-no-infinities-violation)`

procedure

[376](exceptions.html#./exceptions:s44)

`(make-no-nans-violation)`

procedure

[377](exceptions.html#./exceptions:s45)

`(make-non-continuable-violation)`

procedure

[369](exceptions.html#./exceptions:s27)

`(make-polar real1 real2)`

procedure

[183](objects.html#./objects:s123)

`(make-record-constructor-descriptor rtd parent-rcd protocol)`

procedure

[332](records.html#./records:s24)

`(make-record-type-descriptor name parent uid s? o? fields)`

procedure

[331](records.html#./records:s20)

`(make-rectangular real1 real2)`

procedure

[182](objects.html#./objects:s122)

`(make-serious-condition)`

procedure

[366](exceptions.html#./exceptions:s19)

`(make-string n)`

procedure

[218](objects.html#./objects:s218)

`(make-string n char)`

procedure

[218](objects.html#./objects:s218)

`(make-syntax-violation form subform)`

procedure

[370](exceptions.html#./exceptions:s30)

`(make-transcoder codec)`

procedure

[259](io.html#./io:s19)

`(make-transcoder codec eol-style)`

procedure

[259](io.html#./io:s19)

`(make-transcoder codec eol-style error-handling-mode)`

procedure

[259](io.html#./io:s19)

`(make-undefined-violation)`

procedure

[371](exceptions.html#./exceptions:s31)

`(make-variable-transformer procedure)`

procedure

[306](syntax.html#./syntax:s42)

`(make-vector n)`

procedure

[224](objects.html#./objects:s232)

`(make-vector n obj)`

procedure

[224](objects.html#./objects:s232)

`(make-violation)`

procedure

[366](exceptions.html#./exceptions:s20)

`(make-warning)`

procedure

[367](exceptions.html#./exceptions:s23)

`(make-who-condition who)`

procedure

[369](exceptions.html#./exceptions:s26)

`(map procedure list1 list2 ...)`

procedure

[117](control.html#./control:s30)

`(max real1 real2 ...)`

procedure

[178](objects.html#./objects:s107)

`(member obj list)`

procedure

[161](objects.html#./objects:s51)

`(memp procedure list)`

procedure

[163](objects.html#./objects:s52)

`(memq obj list)`

procedure

[161](objects.html#./objects:s51)

`(memv obj list)`

procedure

[161](objects.html#./objects:s51)

`(message-condition? obj)`

procedure

[368](exceptions.html#./exceptions:s24)

`(min real1 real2 ...)`

procedure

[178](objects.html#./objects:s108)

`(mod x1 x2)`

procedure

[175](objects.html#./objects:s99)

`(mod0 x1 x2)`

procedure

[176](objects.html#./objects:s100)

`(modulo int1 int2)`

procedure

[175](objects.html#./objects:s98)

`mutable`

syntax

[331](records.html#./records:s16)

`(nan? real)`

procedure

[174](objects.html#./objects:s97)

`(native-endianness)`

procedure

[228](objects.html#./objects:s241)

`(native-eol-style)`

procedure

[260](io.html#./io:s24)

`(native-transcoder)`

procedure

[259](io.html#./io:s21)

`(negative? real)`

procedure

[173](objects.html#./objects:s95)

`(newline)`

procedure

[285](io.html#./io:s87)

`(newline textual-output-port)`

procedure

[285](io.html#./io:s87)

`(no-infinities-violation? obj)`

procedure

[376](exceptions.html#./exceptions:s44)

`(no-nans-violation? obj)`

procedure

[377](exceptions.html#./exceptions:s45)

`(non-continuable-violation? obj)`

procedure

[369](exceptions.html#./exceptions:s27)

`nongenerative`

syntax

[331](records.html#./records:s16)

`(not obj)`

procedure

[110](control.html#./control:s10)

`(null-environment version)`

procedure

[137](control.html#./control:s82)

`(null? obj)`

procedure

[151](objects.html#./objects:s15)

`(number->string num)`

procedure

[191](objects.html#./objects:s148)

`(number->string num radix)`

procedure

[191](objects.html#./objects:s148)

`(number->string num radix precision)`

procedure

[191](objects.html#./objects:s148)

`(number? obj)`

procedure

[151](objects.html#./objects:s17)

`(numerator rat)`

procedure

[181](objects.html#./objects:s118)

`(odd? int)`

procedure

[174](objects.html#./objects:s96)

`opaque`

syntax

[331](records.html#./records:s16)

`(open-bytevector-input-port bytevector)`

procedure

[264](io.html#./io:s34)

`(open-bytevector-input-port bytevector ?transcoder)`

procedure

[264](io.html#./io:s34)

`(open-bytevector-output-port)`

procedure

[265](io.html#./io:s36)

`(open-bytevector-output-port ?transcoder)`

procedure

[265](io.html#./io:s36)

`(open-file-input-port path)`

procedure

[262](io.html#./io:s29)

`(open-file-input-port path options)`

procedure

[262](io.html#./io:s29)

`(open-file-input-port path options b-mode)`

procedure

[262](io.html#./io:s29)

`(open-file-input-port path options b-mode ?transcoder)`

procedure

[262](io.html#./io:s29)

`(open-file-input/output-port path)`

procedure

[263](io.html#./io:s31)

`(open-file-input/output-port path options)`

procedure

[263](io.html#./io:s31)

`(open-file-input/output-port path options b-mode)`

procedure

[263](io.html#./io:s31)

`(open-file-input/output-port path options b-mode ?transcoder)`

procedure

[263](io.html#./io:s31)

`(open-file-output-port path)`

procedure

[262](io.html#./io:s30)

`(open-file-output-port path options)`

procedure

[262](io.html#./io:s30)

`(open-file-output-port path options b-mode)`

procedure

[262](io.html#./io:s30)

`(open-file-output-port path options b-mode ?transcoder)`

procedure

[262](io.html#./io:s30)

`(open-input-file path)`

procedure

[280](io.html#./io:s75)

`(open-output-file path)`

procedure

[281](io.html#./io:s76)

`(open-string-input-port string)`

procedure

[265](io.html#./io:s35)

`(open-string-output-port)`

procedure

[266](io.html#./io:s37)

`(or expr ...)`

syntax

[110](control.html#./control:s12)

`(output-port-buffer-mode port)`

procedure

[273](io.html#./io:s52)

`(output-port? obj)`

procedure

[270](io.html#./io:s44)

`(pair? obj)`

procedure

[151](objects.html#./objects:s16)

`parent`

syntax

[331](records.html#./records:s16)

`parent-rtd`

syntax

[331](records.html#./records:s16)

`(partition procedure list)`

procedure

[164](objects.html#./objects:s56)

`(peek-char)`

procedure

[284](io.html#./io:s83)

`(peek-char textual-input-port)`

procedure

[284](io.html#./io:s83)

`(port-eof? input-port)`

procedure

[278](io.html#./io:s68)

`(port-has-port-position? port)`

procedure

[271](io.html#./io:s49)

`(port-has-set-port-position!? port)`

procedure

[272](io.html#./io:s50)

`(port-position port)`

procedure

[271](io.html#./io:s49)

`(port-transcoder port)`

procedure

[271](io.html#./io:s48)

`(port? obj)`

procedure

[270](io.html#./io:s43)

`(positive? real)`

procedure

[173](objects.html#./objects:s94)

`(expr0 expr1 ...)`

syntax

[107](control.html#./control:s1)

`(procedure? obj)`

procedure

[155](objects.html#./objects:s23)

`protocol`

syntax

[331](records.html#./records:s16)

`(put-bytevector binary-output-port bytevector)`

procedure

[279](io.html#./io:s70)

`(put-bytevector binary-output-port bytevector start)`

procedure

[279](io.html#./io:s70)

`(put-bytevector binary-output-port bytevector start n)`

procedure

[279](io.html#./io:s70)

`(put-char textual-output-port char)`

procedure

[279](io.html#./io:s71)

`(put-datum textual-output-port obj)`

procedure

[279](io.html#./io:s73)

`(put-string textual-output-port string)`

procedure

[279](io.html#./io:s72)

`(put-string textual-output-port string start)`

procedure

[279](io.html#./io:s72)

`(put-string textual-output-port string start n)`

procedure

[279](io.html#./io:s72)

`(put-u8 binary-output-port octet)`

procedure

[278](io.html#./io:s69)

`(quasiquote obj ...)`

syntax

[142](objects.html#./objects:s5)

`(quasisyntax template ...)`

syntax

[305](syntax.html#./syntax:s40)

`(quote obj)`

syntax

[141](objects.html#./objects:s2)

`(quotient int1 int2)`

procedure

[175](objects.html#./objects:s98)

`(raise obj)`

procedure

[357](exceptions.html#./exceptions:s3)

`(raise-continuable obj)`

procedure

[357](exceptions.html#./exceptions:s3)

`(rational-valued? obj)`

procedure

[153](objects.html#./objects:s18)

`(rational? obj)`

procedure

[151](objects.html#./objects:s17)

`(rationalize real1 real2)`

procedure

[181](objects.html#./objects:s117)

`(read)`

procedure

[284](io.html#./io:s81)

`(read textual-input-port)`

procedure

[284](io.html#./io:s81)

`(read-char)`

procedure

[284](io.html#./io:s82)

`(read-char textual-input-port)`

procedure

[284](io.html#./io:s82)

`(real->flonum real)`

procedure

[211](objects.html#./objects:s198)

`(real-part num)`

procedure

[182](objects.html#./objects:s120)

`(real-valued? obj)`

procedure

[153](objects.html#./objects:s18)

`(real? obj)`

procedure

[151](objects.html#./objects:s17)

`(record-accessor rtd idx)`

procedure

[334](records.html#./records:s31)

`(record-constructor rcd)`

procedure

[333](records.html#./records:s29)

`(record-constructor-descriptor record-name)`

syntax

[333](records.html#./records:s28)

`(record-field-mutable? rtd idx)`

procedure

[338](records.html#./records:s39)

`(record-mutator rtd idx)`

procedure

[334](records.html#./records:s32)

`(record-predicate rtd)`

procedure

[333](records.html#./records:s30)

`(record-rtd record)`

procedure

[338](records.html#./records:s41)

`(record-type-descriptor record-name)`

syntax

[333](records.html#./records:s28)

`(record-type-descriptor? obj)`

procedure

[332](records.html#./records:s23)

`(record-type-field-names rtd)`

procedure

[337](records.html#./records:s38)

`(record-type-generative? rtd)`

procedure

[337](records.html#./records:s37)

`(record-type-name rtd)`

procedure

[336](records.html#./records:s34)

`(record-type-opaque? rtd)`

procedure

[337](records.html#./records:s37)

`(record-type-parent rtd)`

procedure

[336](records.html#./records:s35)

`(record-type-sealed? rtd)`

procedure

[337](records.html#./records:s37)

`(record-type-uid rtd)`

procedure

[336](records.html#./records:s36)

`(record? obj)`

procedure

[338](records.html#./records:s40)

`(remainder int1 int2)`

procedure

[175](objects.html#./objects:s98)

`(remove obj list)`

procedure

[163](objects.html#./objects:s53)

`(remp procedure list)`

procedure

[163](objects.html#./objects:s54)

`(remq obj list)`

procedure

[163](objects.html#./objects:s53)

`(remv obj list)`

procedure

[163](objects.html#./objects:s53)

`(reverse list)`

procedure

[161](objects.html#./objects:s50)

`(round real)`

procedure

[178](objects.html#./objects:s104)

`(scheme-report-environment version)`

procedure

[137](control.html#./control:s82)

`sealed`

syntax

[331](records.html#./records:s16)

`(serious-condition? obj)`

procedure

[366](exceptions.html#./exceptions:s19)

`(set! var expr)`

syntax

[102](binding.html#./binding:s28)

`(set-car! pair obj)`

procedure

[157](objects.html#./objects:s40)

`(set-cdr! pair obj)`

procedure

[157](objects.html#./objects:s41)

`(set-port-position! port pos)`

procedure

[272](io.html#./io:s50)

`(simple-conditions condition)`

procedure

[363](exceptions.html#./exceptions:s16)

`(sin num)`

procedure

[185](objects.html#./objects:s131)

`(sint-list->bytevector list eness size)`

procedure

[239](objects.html#./objects:s261)

`(sqrt num)`

procedure

[183](objects.html#./objects:s127)

`(standard-error-port)`

procedure

[264](io.html#./io:s33)

`(standard-input-port)`

procedure

[264](io.html#./io:s33)

`(standard-output-port)`

procedure

[264](io.html#./io:s33)

`(string char ...)`

procedure

[218](objects.html#./objects:s217)

`(string->bytevector string transcoder)`

procedure

[287](io.html#./io:s92)

`(string->list string)`

procedure

[222](objects.html#./objects:s228)

`(string->number string)`

procedure

[191](objects.html#./objects:s147)

`(string->number string radix)`

procedure

[191](objects.html#./objects:s147)

`(string->symbol string)`

procedure

[242](objects.html#./objects:s269)

`(string->utf16 string)`

procedure

[287](io.html#./io:s94)

`(string->utf16 string endianness)`

procedure

[287](io.html#./io:s94)

`(string->utf32 string)`

procedure

[287](io.html#./io:s94)

`(string->utf32 string endianness)`

procedure

[287](io.html#./io:s94)

`(string->utf8 string)`

procedure

[287](io.html#./io:s93)

`(string-append string ...)`

procedure

[219](objects.html#./objects:s223)

`(string-ci-hash string)`

procedure

[245](objects.html#./objects:s279)

`(string-ci<=? string1 string2 string3 ...)`

procedure

[217](objects.html#./objects:s216)

`(string-ci<? string1 string2 string3 ...)`

procedure

[217](objects.html#./objects:s216)

`(string-ci=? string1 string2 string3 ...)`

procedure

[217](objects.html#./objects:s216)

`(string-ci>=? string1 string2 string3 ...)`

procedure

[217](objects.html#./objects:s216)

`(string-ci>? string1 string2 string3 ...)`

procedure

[217](objects.html#./objects:s216)

`(string-copy string)`

procedure

[219](objects.html#./objects:s222)

`(string-downcase string)`

procedure

[221](objects.html#./objects:s226)

`(string-fill! string char)`

procedure

[220](objects.html#./objects:s225)

`(string-foldcase string)`

procedure

[221](objects.html#./objects:s226)

`(string-for-each procedure string1 string2 ...)`

procedure

[122](control.html#./control:s50)

`(string-hash string)`

procedure

[245](objects.html#./objects:s279)

`(string-length string)`

procedure

[218](objects.html#./objects:s219)

`(string-normalize-nfc string)`

procedure

[222](objects.html#./objects:s227)

`(string-normalize-nfd string)`

procedure

[222](objects.html#./objects:s227)

`(string-normalize-nfkc string)`

procedure

[222](objects.html#./objects:s227)

`(string-normalize-nfkd string)`

procedure

[222](objects.html#./objects:s227)

`(string-ref string n)`

procedure

[218](objects.html#./objects:s220)

`(string-set! string n char)`

procedure

[219](objects.html#./objects:s221)

`(string-titlecase string)`

procedure

[221](objects.html#./objects:s226)

`(string-upcase string)`

procedure

[221](objects.html#./objects:s226)

`(string<=? string1 string2 string3 ...)`

procedure

[216](objects.html#./objects:s215)

`(string<? string1 string2 string3 ...)`

procedure

[216](objects.html#./objects:s215)

`(string=? string1 string2 string3 ...)`

procedure

[216](objects.html#./objects:s215)

`(string>=? string1 string2 string3 ...)`

procedure

[216](objects.html#./objects:s215)

`(string>? string1 string2 string3 ...)`

procedure

[216](objects.html#./objects:s215)

`(string? obj)`

procedure

[154](objects.html#./objects:s20)

`(substring string start end)`

procedure

[220](objects.html#./objects:s224)

`(symbol->string symbol)`

procedure

[242](objects.html#./objects:s270)

`(symbol-hash symbol)`

procedure

[245](objects.html#./objects:s279)

`(symbol=? symbol1 symbol2)`

procedure

[242](objects.html#./objects:s268)

`(symbol? obj)`

procedure

[154](objects.html#./objects:s22)

`(syntax template)`

syntax

[300](syntax.html#./syntax:s33)

`(syntax->datum obj)`

procedure

[308](syntax.html#./syntax:s44)

`(syntax-case expr (literal ...) clause ...)`

syntax

[299](syntax.html#./syntax:s30)

`(syntax-rules (literal ...) clause ...)`

syntax

[294](syntax.html#./syntax:s14)

`(syntax-violation who msg form)`

procedure

[359](exceptions.html#./exceptions:s6)

`(syntax-violation who msg form subform)`

procedure

[359](exceptions.html#./exceptions:s6)

`(syntax-violation-form condition)`

procedure

[370](exceptions.html#./exceptions:s30)

`(syntax-violation-subform condition)`

procedure

[370](exceptions.html#./exceptions:s30)

`(syntax-violation? obj)`

procedure

[370](exceptions.html#./exceptions:s30)

`(tan num)`

procedure

[185](objects.html#./objects:s131)

`(textual-port? obj)`

procedure

[270](io.html#./io:s45)

`(transcoded-port binary-port transcoder)`

procedure

[271](io.html#./io:s47)

`(transcoder-codec transcoder)`

procedure

[259](io.html#./io:s20)

`(transcoder-eol-style transcoder)`

procedure

[259](io.html#./io:s20)

`(transcoder-error-handling-mode transcoder)`

procedure

[259](io.html#./io:s20)

`(truncate real)`

procedure

[177](objects.html#./objects:s101)

`(u8-list->bytevector list)`

procedure

[232](objects.html#./objects:s253)

`(uint-list->bytevector list eness size)`

procedure

[239](objects.html#./objects:s261)

`(undefined-violation? obj)`

procedure

[371](exceptions.html#./exceptions:s31)

`(unless test-expr expr1 expr2 ...)`

syntax

[112](control.html#./control:s17)

`(unquote obj ...)`

syntax

[142](objects.html#./objects:s5)

`(unquote-splicing obj ...)`

syntax

[142](objects.html#./objects:s5)

`(unsyntax template ...)`

syntax

[305](syntax.html#./syntax:s40)

`(unsyntax-splicing template ...)`

syntax

[305](syntax.html#./syntax:s40)

`(utf-16-codec)`

procedure

[259](io.html#./io:s22)

`(utf-8-codec)`

procedure

[259](io.html#./io:s22)

`(utf16->string bytevector endianness)`

procedure

[288](io.html#./io:s96)

`(utf16->string bytevector endianness endianness-mandatory?)`

procedure

[288](io.html#./io:s96)

`(utf32->string bytevector endianness)`

procedure

[288](io.html#./io:s96)

`(utf32->string bytevector endianness endianness-mandatory?)`

procedure

[288](io.html#./io:s96)

`(utf8->string bytevector)`

procedure

[287](io.html#./io:s95)

`(values obj ...)`

procedure

[131](control.html#./control:s70)

`variable`

syntax

[91](binding.html#./binding:s2)

`(vector obj ...)`

procedure

[224](objects.html#./objects:s231)

`(vector->list vector)`

procedure

[225](objects.html#./objects:s237)

`(vector-fill! vector obj)`

procedure

[225](objects.html#./objects:s236)

`(vector-for-each procedure vector1 vector2 ...)`

procedure

[122](control.html#./control:s47)

`(vector-length vector)`

procedure

[224](objects.html#./objects:s233)

`(vector-map procedure vector1 vector1 ...)`

procedure

[121](control.html#./control:s44)

`(vector-ref vector n)`

procedure

[224](objects.html#./objects:s234)

`(vector-set! vector n obj)`

procedure

[225](objects.html#./objects:s235)

`(vector-sort predicate vector)`

procedure

[226](objects.html#./objects:s239)

`(vector-sort! predicate vector)`

procedure

[226](objects.html#./objects:s239)

`(vector? obj)`

procedure

[154](objects.html#./objects:s21)

`(violation? obj)`

procedure

[366](exceptions.html#./exceptions:s20)

`(warning? obj)`

procedure

[367](exceptions.html#./exceptions:s23)

`(when test-expr expr1 expr2 ...)`

syntax

[112](control.html#./control:s17)

`(who-condition? obj)`

procedure

[369](exceptions.html#./exceptions:s26)

`(with-exception-handler procedure thunk)`

procedure

[360](exceptions.html#./exceptions:s7)

`(with-input-from-file path thunk)`

procedure

[283](io.html#./io:s79)

`(with-output-to-file path thunk)`

procedure

[283](io.html#./io:s80)

`(with-syntax ((pattern expr) ...) body1 body2 ...)`

syntax

[304](syntax.html#./syntax:s38)

`(write obj)`

procedure

[284](io.html#./io:s84)

`(write obj textual-output-port)`

procedure

[284](io.html#./io:s84)

`(write-char char)`

procedure

[285](io.html#./io:s86)

`(write-char char textual-output-port)`

procedure

[285](io.html#./io:s86)

`(zero? num)`

procedure

[173](objects.html#./objects:s93)
