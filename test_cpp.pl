% -*- mode: Prolog; coding: utf-8 -*-

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* This tests the examples in the SWI-cpp.h documentation. */

:- module(test_cpp,
          [ test_cpp/0
          ]).

:- encoding(utf8).

:- use_module(library(plunit)).

:- use_foreign_library(foreign(cpp4pl)).

test_cpp :-
    run_tests([ cpp
              ]).

:- begin_tests(cpp).

test(hello_1) :-
    % TODO: this outputs to cout ... make a version that checks the output?
    hello(world).

test(hello2_1) :-
    % TODO: this outputs to cout ... make a version that checks the output?
    hello2(world2).

test(hello3_1) :-
    % TODO: this outputs to cout ... make a version that checks the output?
    hello3('世界弐').

test(add_3, Result == 666) :-
    add(667, -1, Result).
test(add_3_err, error(type_error(integer,0.1))) :-
    add(666, 0.1, _).

test(add_num_3_a, Result == 666) :-
    add_num(555, 111, Result).
test(add_num_3_b, Result == 666.6) :-
    add_num(555.2, 111.4, Result).
test(add_num_3_c, error(type_error(float,"abc"))) :-
    add_num(123, "abc", _Result).

testing:p(1).  % For average/3 test
testing:p(10).
testing:p(20).

test(average_3, Average == Expected) :-
    average(X, testing:p(X), Average),
    Expected is (1+10+20)/3 .

test(hello_0) :-
    hello.

test(term_1, Term = hello(world)) :-
    term(Term).

test(term_2a, Result == 'hello world') :-
    term(atom, Result).
test(term_2b, Result == "hello world") :-
    term(string, Result).
test(term_2c, Result = [104,101,108,108,111,32,119,111,114,108,100]) :-
    term(code_list, Result).
test(term_2d, Result = [h,e,l,l,o,' ',w,o,r,l,d]) :-
    term(char_list, Result).
test(term_2e, Result = hello(world)) :-
    term(term, Result).
test(term_2f, error(domain_error(type,foo))) :-
    term(foo, _Result).

test(can_unify_2a, [true(X\==Y)]) :-
    can_unify(f(X), f(Y)).
test(can_unify_2b) :-
    can_unify(a(X), a(1)),
    assertion(var(X)).

test(unify_ex_2a, X == a) :-
    unify_ex(foo(X), foo(a)).
test(unify_ex_2b, fail) :-
    unify_ex(foo(_X), bar(a)).

test(make_integer_2a, X == 123) :-
    make_uint64(123, X).
test(make_integer_2b) :-
    X = 666,
    Y = 666,
    make_uint64(X, 666),
    make_uint64(666, 666),
    make_uint64(X, Y).
test(make_integer_2c, fail) :-
    make_uint64(123, 124).
test(make_integer_2d, error(representation_error(uint64_t))) :-
    Val is 0xffffffffffffffff + 999, % uses extended integers
    make_uint64(Val, _Y).
test(make_integer_2e, error(domain_error(not_less_than_zero,-1))) :-
    make_uint64(-1, _Y).

test(make_integer_2a, X == 123) :-
    make_int64(123, X).
test(make_integer_2b) :-
    X = 666,
    Y = 666,
    make_int64(X, 666),
    make_int64(666, 666),
    make_int64(X, Y).
test(make_integer_2c, fail) :-
    make_int64(123, 124).
test(make_integer_2d, error(representation_error(int64_t))) :-
    Val is 0xffffffffffffffff + 999, % uses extended integers
    make_int64(Val, _Y).
test(make_integer_2e, Y == -1) :-
    make_int64(-1, Y).

test(hostname_1, [Host == Host2]) :-
    hostname(Host),
    hostname2(Host2).

test(cappend, Result = [a,b,c,d,e]) :-
    cappend([a,b,c], [d,e], Result).

test(call_atom_1) :-
    call_atom('writeln(abc)'). % smoke test

test(square_roots_2a, Result == [0.0, 1.0, 1.4142135623730951, 1.7320508075688772, 2.0]) :-
    square_roots(5, Result).
test(square_roots_2b, error(resource_error(stack))) :-
    square_roots(1000000000, _).

test(malloc_2) :-
    malloc(1000, _Result). % smoke test

test(name_arity_1) :-
    name_arity(foo(bar,zot)).

test(name_arity_3) :-
    name_arity(foo(bar,zot), Name, Arity),
    assertion(Name == foo),
    assertion(Arity == 2).

test(list_modules_0) :-
    % TODO: this outputs to cout ... make a version that checks the output?
    list_modules.

test(my_object, Contents == "foo-bar") :-
    make_my_object(MyObject),
    my_object_contents(MyObject, Contents),
    free_my_object(MyObject).

test(make_functor_3a, F == foo(x)) :-
    make_functor(foo, x, F).
test(make_functor_3b, error(type_error(atom,123))) :-
    make_functor(123, x, _).
test(make_functor_3c) :-
    make_functor(bar, 123, bar(123)).
test(make_functor_3d, fail) :-
    make_functor(bar, 123, bar(666)).
test(make_functor_3e, fail) :-
    make_functor(bar, 123, qqsv(123)).
test(make_functor_3f, Z==6.66) :-
    make_functor(bbb, Z, F),
    F = bbb(6.66).

:- end_tests(cpp).
