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

/* This tests the examples in the SWI-cpp2.h documentation. */

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
test(add_3, Result == 123) :-
    add(100.0, 23, Result).
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

% Note: unify_error has additional tests for eq1/2
test(eq1_2a, X == a) :-
    eq1(foo(X), foo(a)).
test(eq1_2b, fail) :-
    eq1(foo(_X), bar(a)).

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

:- if(current_prolog_flag(bounded,false)).
test(make_integer_2d, error(representation_error(uint64_t))) :-
    Val is 0xffffffffffffffff + 999, % uses extended integers
    make_uint64(Val, _Y).
:- endif.

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

:- if(current_prolog_flag(bounded,false)).
test(make_integer_2d, error(representation_error(int64_t))) :-
    Val is 0xffffffffffffffff + 999, % uses extended integers
    make_int64(Val, _Y).
:- endif.

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

test(malloc_1) :-
    malloc(1000, Result), % smoke test
    free(Result).

too_big_alloc_request(Request) :-
    current_prolog_flag(address_bits, Bits),
    (   Bits == 32
    ->  Request = 0x7fffffff
    ;   Bits == 64
    ->  Request = 0x7fffffffffffffff
        %         0x10000000000 is ASAN maximum on 64-bit machines
    ;   assertion(memberchk(Bits, [32,64]))
    ).

:- if(current_prolog_flag(bounded,false)).

too_many_bits_alloc_request(Request) :-
    current_prolog_flag(address_bits, Bits),
    (   Bits == 32
    ->  Request is 0x7fffffffffffffffff
    ;   Bits == 64
    ->  Request is 0x7fffffffffffffffff
    ;   assertion(memberchk(Bits, [32,64]))
    ).

:- endif.

test(malloc_2) :-
    too_big_alloc_request(Request),
    malloc(Request, Result),
    assertion(Result == 0),
    free(Result).

:- if(current_prolog_flag(bounded,false)).

test(malloc_3) :-
    % This assumes size_t is no more than 64 bits:
    too_many_bits_alloc_request(Request),
    catch( ( malloc(Request, Result),
             free(Result)
           ),
           error(E,_), true),
    assertion(memberchk(E, [representation_error(size_t),
                            type_error(integer,_)])).

:- endif.

test(new_chars_1) :-
    new_chars(1000, Result), % smoke test
    delete_chars(Result).

:- if(\+ address_sanitizer).
% ASAN has maximum 0x10000000000
%   see ASAN_OPTIONS=allocator_may_return_null=1:soft_rss_limit_mb=...:hard_rss_limit_mb=...
% https://github.com/google/sanitizers/issues/295
% https://github.com/google/sanitizers/issues/740

test(new_chars_2, error(resource_error(memory))) :-
    too_big_alloc_request(Request),
    new_chars(Request, Result),
    delete_chars(Result).

:- endif.

:- if(current_prolog_flag(bounded,false)).

test(new_chars_3) :-
    % This assumes size_t is no more than 64 bits:
    too_many_bits_alloc_request(Request),
    catch( ( new_chars(Request, Result),
             delete_chars(Result)
           ),
           error(E,_), true),
    assertion(memberchk(E, [representation_error(size_t),
                            type_error(integer,_)])).

:- endif.

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

% The following are for verifying some documentation details, and for
% ensuring that various mechanisms for reporting failure and
% exceptions behave as expected.

test(c_PL_unify_nil_ex, X == []) :-
    c_PL_unify_nil_ex(X).
test(c_PL_unify_nil_ex) :-
    c_PL_unify_nil_ex([]).

% The following are for verifying that an exception in
% PL_occurs_term() is handled properly - exceptions such as
% out-of-stack should behave the same way, if they don't result in a
% fatal error. The same set of tests are repeated for eq1/2, eq2/2,
% eq3/2.

test(unify_error, [ setup(( current_prolog_flag(occurs_check, OCF),
                                set_prolog_flag(occurs_check, error) )),
                    cleanup(    set_prolog_flag(occurs_check, OCF) ),
                    error(occurs_check(B,f(B))) ]) :-
    eq1(X, f(X)).

test(unify_error, [ setup(( current_prolog_flag(occurs_check, OCF),
                                set_prolog_flag(occurs_check, true) )),
                    cleanup(    set_prolog_flag(occurs_check, OCF) ),
                    fail]) :-
    eq1(X, f(X)).

test(unify_error, [ setup(( prolog_flag(occurs_check, OCF),
                               set_prolog_flag(occurs_check, false) )),
                    cleanup(   set_prolog_flag(occurs_check, OCF) ),
                    true]) :-
    eq1(X, f(X)).

% Repeat the unify_error test, using eq2/2:

test(unify_error, [ setup(( current_prolog_flag(occurs_check, OCF),
                                set_prolog_flag(occurs_check, error) )),
                    cleanup(    set_prolog_flag(occurs_check, OCF) ),
                    error(occurs_check(B,f(B))) ]) :-
    eq2(X, f(X)).

test(unify_error, [ setup(( current_prolog_flag(occurs_check, OCF),
                                set_prolog_flag(occurs_check, true) )),
                    cleanup(    set_prolog_flag(occurs_check, OCF) ),
                    fail]) :-
    eq2(X, f(X)).

test(unify_error, [ setup(( prolog_flag(occurs_check, OCF),
                               set_prolog_flag(occurs_check, false) )),
                    cleanup(   set_prolog_flag(occurs_check, OCF) ),
                    true]) :-
    eq2(X, f(X)).

% Repeat the unify_error test, using eq3/2:

test(unify_error, [ setup(( current_prolog_flag(occurs_check, OCF),
                                set_prolog_flag(occurs_check, error) )),
                    cleanup(    set_prolog_flag(occurs_check, OCF) ),
                    error(occurs_check(B,f(B))) ]) :-
    eq3(X, f(X)).

test(unify_error, [ setup(( current_prolog_flag(occurs_check, OCF),
                                set_prolog_flag(occurs_check, true) )),
                    cleanup(    set_prolog_flag(occurs_check, OCF) ),
                    fail]) :-
    eq3(X, f(X)).

test(unify_error, [ setup(( prolog_flag(occurs_check, OCF),
                               set_prolog_flag(occurs_check, false) )),
                    cleanup(   set_prolog_flag(occurs_check, OCF) ),
                    true]) :-
    eq3(X, f(X)).

% TODO: Add tests for as_string(enc), such as enc=EncLatin1 and atom is non-ascii
%       ... for PlTerm::as_string() where term isn't an atom


% Tests from test_ffi.pl, for functions translated from ffi4pl.c:

test(range_cpp1, all(X == [1,2])) :-
    range_cpp(1, 3, X).
test(range_cpp2, all(X == [-2,-1,0,1,2])) :-
    range_cpp(-2, 3, X).
test(range_cpp3a, all(X == [0])) :-
    range_cpp(0, 1, X).
test(range_cpp3b, all(X == [10])) :-
    range_cpp(10, 11, X).
test(range_cpp3c, all(X == [-2])) :-
    range_cpp(-2, -1, X).
test(range_cpp4a, fail) :-
    range_cpp(1, 1, _X).
test(range_cpp4a, fail) :-
    range_cpp(0, 0, _X).
test(range_cpp4a, fail) :-
    range_cpp(-1, -1, _X).
test(range_cpp4d, fail) :-
    range_cpp(1, 2, 2).
test(range_cpp5, X == 1) :- % Will produce warning if non-deterministic
    range_cpp(1, 2, X).
test(range_cpp6b, error(type_error(integer,a))) :-
    range_cpp(a, 10, _).
test(range_cpp6b, error(type_error(integer,foo))) :-
    range_cpp(1, foo, _).

% This is test wchar_1 in test_ffi.pl:
test(wchar_1, all(Result == ["//0", "/ /1",
                             "/abC/3",
                             "/Hello World!/12",
                             "/хелло/5",
                             "/хелло 世界/8",
                             "/網目錦へび [àmímé níshíkíhéꜜbì]/26"])) :-
    (   w_atom_cpp('',             Result)
    ;   w_atom_cpp(' ',            Result)
    ;   w_atom_cpp('abC',          Result)
    ;   w_atom_cpp('Hello World!', Result)
    ;   w_atom_cpp('хелло',        Result)
    ;   w_atom_cpp('хелло 世界',   Result)
    ;   w_atom_cpp('網目錦へび [àmímé níshíkíhéꜜbì]', Result)
    ).

% TODO: decouple this test from message hooks
%       ('$messages':message_to_string/2 or print_message/'$write_on_string'/2):
test(type_error_string, S == "Type error: `foofoo' expected, found `'foo-bar'' (an atom)") :-
    type_error_string('foo-bar', S, T),
    assertion(unifiable(T, error(type_error(foofoo,'foo-bar'),A), [A=B])),
    assertion(var(A)),
    assertion(var(B)),
    assertion(A\==B).

:- end_tests(cpp).

w_atom_cpp(Atom, String) :-
    with_output_to(string(String), w_atom_cpp_(current_output, Atom)).
