% -*- mode: Prolog; coding: utf-8 -*-

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2023, SWI-Prolog Solutions b.v.
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
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- autoload(library(aggregate)).
:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).

:- encoding(utf8).

:- use_foreign_library(foreign(test_cpp)).

test_cpp :-
    run_tests([ cpp,
                cpp_atommap
	      ]).

% Some of the tests can result in crashes if there's a bug, so the
% `output(on_failure)` option results in nothing being written.
% If so, uncomment the following line
% :- set_test_options([output(always), format(log)]).

:- begin_tests(cpp).

test(hello, Out == "hello hello hello") :-
    % hello :- write('hello hello hello')
    with_output_to(string(Out), hello).

test(hello, Out == "Hello WORLD\nHello WORLD\nHello WORLD\nHello WORLD\nHello WORLD\n") :-
    hello("WORLD", Out).
test(hello, error(representation_error(encoding))) :-
    hello("世界", _Out).

% The following might give a different result, depending on locale:
test(hello2, Out == "Hello2 world2\nHello2 world2\nHello2 world2\nHello2 world2\nHello2 world2\n") :-
    hello2(world2, Out).

test(hello3, Out == "Hello3 世界弐\n") :-
    hello3(世界弐, Out).

test(call_cpp, Out == "hello(foo)\n") :-
    with_output_to(string(Out), call_cpp(writeln(hello(foo)))).
test(call_cpp, Out == "hello(世界四)\n") :-
    with_output_to(string(Out), call_cpp(writeln(hello(世界四)))).
test(call_cpp, error(existence_error(procedure,unknown_pred/1))) :-
    call_cpp(unknown_pred(hello(世界四))).
test(call_cpp, fail) :-
    call_cpp(atom(hello(foo))).

test(call_cpp, Ex == "no exception") :-
    call_cpp_ex(writeln(hello(世界四)), Ex).
test(call_cpp) :-
    call_cpp_ex(unknown_pred(hello(世界四)), Ex),
    assertion(subsumes_term(error(existence_error(procedure, unknown_pred/1), _), Ex)).

test(call_cpp, Out == "hello(世界四)\n") :-
    with_output_to(string(Out), call_cpp(writeln, hello(世界四))).
test(call_cpp, error(existence_error(procedure,unknown_pred/1))) :-
    call_cpp(unknown_pred, hello(世界四)).
test(call_cpp, fail) :-
    call_cpp(atom, hello(foo)).

test(as_string, S == "foo") :-
    atom_to_string(foo, S).
test(as_string, S == "foo(bar)") :-
    term_to_string(foo(bar), S).

% Note: atom_to_string/2 and term_to_string/2 translate the data
% to a UTF-8 string.  We currenly do not support encoding for
% PlTerm.unify_string(), so we get as result the byte encoding
% of the UTF8 data.
test(as_string, S == "ä¸\u0096ç\u0095\u008Cå\u009B\u009B") :-
    atom_to_string(世界四, S).
test(as_string, S == "hello(ä¸\u0096ç\u0095\u008Cå\u009B\u009B)") :-
    term_to_string(hello(世界四), S).

test(add_3, Result == 666) :-
    add(667, -1, Result).
test(add_3, Result == 123) :-
    add(100.0, 23, Result).
test(add_3_err, error(type_error(integer,0.1))) :-
    add(666, 0.1, _).

test(add_num_3, Result == 666) :-
    add_num(555, 111, Result).
test(add_num_3, Result == 666.6) :-
    add_num(555.2, 111.4, Result).
test(add_num_3, error(type_error(float,"abc"))) :-
    add_num(123, "abc", _Result).

testing:p(1).  % For average/3 test
testing:p(10).
testing:p(20).

test(average_3, Average =:= Expected) :-
    average(X, testing:p(X), Average),
    Expected is (1+10+20)/3 .
test(average_3, Average =:= Expected) :-
    average(X, between(1,6,X), Average),
    aggregate(sum(X)/count, between(1,6,X), A),
    Expected is A.

call_cut_test :-
    setup_call_cleanup(true,
		       between(1, 5, _X),
		       atom_codes(_,_)).

test(call_cut, error(existence_error(procedure,call_cut_test/0))) :-
    % This tests that an error in ~PlQuery() is handled properly
    % See discussion: https://github.com/SWI-Prolog/packages-cpp/pull/27
    call_cut("call_cut_test").

test(term_1, Term == hello(world)) :-
    term(Term).

test(term_2, Result == 'hello world') :-
    term(atom, Result).
test(term_2, Result == "hello world") :-
    term(string, Result).
test(term_2, Result == [104,101,108,108,111,32,119,111,114,108,100]) :-
    term(code_list, Result).
test(term_2, Result == [h,e,l,l,o,' ',w,o,r,l,d]) :-
    term(char_list, Result).
test(term_1, Result == hello(world)) :-
    term(term, Result).
test(term_1, error(domain_error(type,foo))) :-
    term(foo, _Result).

test(can_unify_2, [true(X\==Y)]) :-
    can_unify(f(X), f(Y)).
test(can_unify_2) :-
    can_unify(a(X), a(1)),
    assertion(var(X)).

% Note: unify_error has additional tests for eq1/2
test(eq1_2, X == a) :-
    eq1(foo(X), foo(a)).
test(eq1_2, fail) :-
    eq1(foo(_X), bar(a)).

test(make_integer_2, X == 123) :-
    make_uint64(123, X).
test(make_integer) :-
    X = 666,
    Y = 666,
    make_uint64(X, 666),
    make_uint64(666, 666),
    make_uint64(X, Y).
test(make_integer_2, fail) :-
    make_uint64(123, 124).

:- if(current_prolog_flag(bounded,false)).
test(make_uint64_2, error(representation_error(uint64_t))) :-
    Val is 0xffffffffffffffff + 999, % uses extended integers
    make_uint64(Val, _Y).
:- endif.

test(make_uint64_2, error(domain_error(not_less_than_zero,-1))) :-
    make_uint64(-1, _Y).

test(make_int64_2, X == 123) :-
    make_int64(123, X).
test(make_int64_2) :-
    X = 666,
    Y = 666,
    make_int64(X, 666),
    make_int64(666, 666),
    make_int64(X, Y).
test(make_int64_2, fail) :-
    make_int64(123, 124).
test(make_int64_2, error(type_error(integer,abc))) :-
    make_int64(abc, _Y).

:- if(current_prolog_flag(bounded,false)).
test(make_int64_2, error(representation_error(int64_t))) :-
    Val is 0xffffffffffffffff + 999, % uses extended integers
    make_int64(Val, _Y).
:- endif.

test(make_int64_2, Y == -1) :-
    make_int64(-1, Y).

test(hostname, [Host == Host2]) :-
    hostname(Host),
    hostname2(Host2).

test(cappend, Result == [a,b,c,d,e]) :-
    cappend([a,b,c], [d,e], Result).
test(cappend) :-
    cappend([a,b,c], [d,e], [a,b,c,d,e]).
test(cappend, fail) :-
    cappend([a,b,c], [d,e], [a,b,c,d]).
test(cappend, fail) :-
    cappend([a,b,c], [d,e], [a,b,c,d,e,f]).
test(cappend, fail) :-
    cappend([a,b,c], [d,e], [a,b,c,d,e|f]).

test(cpp_call, Out == "abc\n") :-
    with_output_to(string(Out),
		   cpp_call(writeln(abc), [normal])).

cpp_call(Goal, Flags) :-
    query_flags(Flags, CombinedFlag),
    cpp_call_(Goal, CombinedFlag, false).

test(square_roots_2, Result == [0.0, 1.0, 1.4142135623730951, 1.7320508075688772, 2.0]) :-
    square_roots(4, Result).

:- meta_predicate with_small_stacks(+, 0).
with_small_stacks(Free, Goal) :-
    force_gc,
    statistics(globalused, G),
    statistics(trailused, T),
    statistics(localused, L),
    NewLimit is G+L+T+Free,
    current_prolog_flag(stack_limit, Old),
    setup_call_cleanup(
	set_prolog_flag(stack_limit, NewLimit),
	Goal,
	set_prolog_flag(stack_limit, Old)).

test(square_roots_2, error(resource_error(stack))) :-
    with_small_stacks(5 000 000, % 400 000 seems to be about the smallest allowed value
		      square_roots(1000000000, _)).

test(malloc) :-
    malloc_new(1000, Result), % smoke test
    free_delete(Result).

test(malloc) :-
    malloc_malloc(1000, Result), % smoke test
    free_malloc(Result).

test(malloc) :-
    malloc_PL_malloc(1000, Result), % smoke test
    free_PL_malloc(Result).

:- if(\+ current_prolog_flag(asan, true)).
too_big_alloc_request(Request) :-
    current_prolog_flag(address_bits, Bits),
    (   Bits == 32
    ->  Request = 0xffffffff
    ;   Bits == 64
    ->  Request = 0xffffffffffffffff
	%         0x10000000000 is ASAN maximum on 64-bit machines
    ;   assertion(memberchk(Bits, [32,64]))
    ).

:- if(current_prolog_flag(bounded,false)).

too_many_bits_alloc_request(Request) :-
    % This assumes size_t is no more than 64 bits:
    current_prolog_flag(address_bits, Bits),
    (   Bits == 32
    ->  Request is 0xffffffff + 1
    ;   Bits == 64
    ->  Request is 0xffffffffffffffff + 1
    ;   assertion(memberchk(Bits, [32,64]))
    ).

:- endif.

test(malloc, error(resource_error(memory))) :-
    too_big_alloc_request(Request),
    malloc_new(Request, _Result).

:- if(current_prolog_flag(bounded,false)).

test(malloc) :-
    too_many_bits_alloc_request(Request),
    catch( ( malloc_new(Request, Result),
	     free_delete(Result)
	   ),
	   error(E,_), true),
    assertion(memberchk(E, [representation_error(_), % representation_error(uint64_t)
			    type_error(integer,_)])).


:- endif.

% ASAN has maximum 0x10000000000
%   see ASAN_OPTIONS=allocator_may_return_null=1:soft_rss_limit_mb=...:hard_rss_limit_mb=...
% https://github.com/google/sanitizers/issues/295
% https://github.com/google/sanitizers/issues/740

test(new_chars_2, error(resource_error(memory))) :-
    too_big_alloc_request(Request),
    new_chars(Request, Result),
    delete_chars(Result).

:- if(current_prolog_flag(bounded,false)).

test(new_chars_2) :-
    too_many_bits_alloc_request(Request),
    catch( ( new_chars(Request, Result),
	     delete_chars(Result)
	   ),
	   error(E,_), true),
    assertion(memberchk(E, [representation_error(_),
			    type_error(integer,_)])).

:- endif.
:- endif.

test(new_chars_2) :-
    new_chars(1000, Result), % smoke test
    delete_chars(Result).

test(name_arity_2, Out == "name = foo, arity = 2\n") :-
    name_arity(foo(bar,zot), Out).

test(name_arity_2) :-
    name_arity(foo(bar,zot), Name, Arity),
    assertion(Name == foo),
    assertion(Arity == 2).

test(list_modules_0) :-
    list_modules(Text),
    split_string(Text, "\n", "", Strings),
    forall(( member(S, Strings), S \== ""),
	   ( atom_string(M, S),
	     current_module(M))).

test(my_object, Contents == "foo-bar") :-
    make_my_object(MyObject),
    my_object_contents(MyObject, Contents),
    free_my_object(MyObject).

test(make_functor_3, F == foo(x)) :-
    make_functor(foo, x, F).
test(make_functor_3, error(type_error(atom,123))) :-
    make_functor(123, x, _).
test(make_functor_3) :-
    make_functor(bar, 123, bar(123)).
test(make_functor_3, fail) :-
    make_functor(bar, 123, bar(666)).
test(make_functor_3, fail) :-
    make_functor(bar, 123, qqsv(123)).
test(make_functor_3, Z==6.66) :-
    make_functor(bbb, Z, F),
    F = bbb(6.66).

test(cpp_arg, A == bar) :-
    cpp_arg(1, foo(bar,zot), A).
test(cpp_arg, A == zot) :-
    cpp_arg(2, foo(bar,zot), A).
test(cpp_arg, error(domain_error(arity,3))) :-
    cpp_arg(3, foo(bar,zot), _A).
test(cpp_arg, error(domain_error(not_less_than_zero,0))) :-
    cpp_arg(0, foo(bar,zot), _A).
test(cpp_arg, error(domain_error(not_less_than_zero,-2))) :-
    cpp_arg(-2, foo(bar,zot), _A).
test(cpp_arg, error(type_error(compound,foo))) :-
    cpp_arg(1, foo, _A).

% The following are for verifying some documentation details, and for
% ensuring that various mechanisms for reporting failure and
% exceptions behave as expected.

test(c_PL_unify_nil, X == []) :-
    c_PL_unify_nil(X).
test(c_PL_unify_nil) :-
    c_PL_unify_nil([]).
test(c_PL_unify_nil, fail) :-
    c_PL_unify_nil(abc).

test(c_PL_unify_nil_ex, X == []) :-
    c_PL_unify_nil_ex(X).
test(c_PL_unify_nil_ex) :-
    c_PL_unify_nil_ex([]).
test(c_PL_unify_nil_ex, error(type_error(list,abc))) :-
    c_PL_unify_nil_ex(abc).

test(check_c_PL_unify_nil, X == []) :-
    check_c_PL_unify_nil(X).
test(check_c_PL_unify_nil) :-
    check_c_PL_unify_nil([]).
% The following error is subject to change:
test(check_c_PL_unify_nil, error(unknown_error('False return code without exception'))) :-
    check_c_PL_unify_nil(abc).

test(check_c_PL_unify_nil_ex, X == []) :-
    check_c_PL_unify_nil_ex(X).
test(check_c_PL_unify_nil_ex) :-
    check_c_PL_unify_nil_ex([]).
test(check_c_PL_unify_nil_ex, error(type_error(list,abc))) :-
    check_c_PL_unify_nil_ex(abc).

test(cpp_unify_nil, X == []) :-
    cpp_unify_nil(X).
test(cpp_unify_nil) :-
    cpp_unify_nil([]).
test(cpp_unify_nil, fail) :-
    cpp_unify_nil(abc).

test(cpp_unify_nil_ex, X == []) :-
    cpp_unify_nil_ex(X).
test(cpp_unify_nil_ex) :-
    cpp_unify_nil_ex([]).
test(cpp_unify_nil_ex, error(type_error(list,abc))) :-
    cpp_unify_nil_ex(abc).

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

test(range_cpp, all(X == [1,2])) :-
    range_cpp(1, 3, X).
test(range_cpp, all(X == [-2,-1,0,1,2])) :-
    range_cpp(-2, 3, X).
test(range_cpp, all(X == [0])) :-
    range_cpp(0, 1, X).
test(range_cpp, all(X == [10])) :-
    range_cpp(10, 11, X).
test(range_cpp, all(X == [-2])) :-
    range_cpp(-2, -1, X).
test(range_cpp, fail) :-
    range_cpp(1, 1, _X).
test(range_cpp, fail) :-
    range_cpp(0, 0, _X).
test(range_cpp, fail) :-
    range_cpp(-1, -1, _X).
test(range_cpp, fail) :-
    range_cpp(1, 2, 2).
test(range_cpp, X == 1) :- % Will produce warning if non-deterministic
    range_cpp(1, 2, X).
test(range_cpp, error(type_error(integer,a))) :-
    range_cpp(a, 10, _).
test(range_cpp, error(type_error(integer,foo))) :-
    range_cpp(1, foo, _).

% TODO: not finished -- use nb_set and friends to preserve
%       first 2 results
range_2(From, To, Result) :-
    (   range_cpp(From, To, _),
        fail
    *-> true
    ;   range_cpp(From, To, Result)
    ).


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
test(type_error_string) :-
    type_error_string('foo-bar', _S, T),
    assertion(unifiable(T, error(type_error(foofoo,'foo-bar'),A), [A=B])),
    % TODO: when PlException::string_term() is revived (using '$messages':message_to_string/2),
    %       add the following assertion:
    % assertion(S == "Type error: `foofoo' expected, found `'foo-bar'' (an atom)"])
    assertion(var(A)),
    assertion(var(B)),
    assertion(A\==B).

:- if(\+ current_prolog_flag(asan, true)).

% test(int_info) causes a memory leak because the IntInfo map doesn't
% destruct the elements on cleanup. (At least, I think that's the
% cause of the memory leak.)

test(int_info) :-
    findall(Name:Info, int_info(Name, Info), Infos),
    assertion(memberchk(uint32_t:int_info(uint32_t,4,0,4294967295), Infos)).
test(int_info, [nondet, Name:Info == uint32_t:int_info(uint32_t,4,0,4294967295)]) :-
    Info = int_info(_,_,0,_),
    int_info(Name, Info),
    Info = int_info(uint32_t,_,_,_).
test(int_info) :-
    Info = int_info(_,_,0,_),
    findall(Name:Info, int_info(Name, Info), Infos),
    assertion(memberchk(uint16_t:int_info(uint16_t,2,0,65535), Infos)).

:- endif.

% int_info_cut test checks that PL_PRUNED works as expected:
test(int_info_cut, Name:Info == bool:int_info(bool, 1, 0, 1)) :-
    int_info(Name, Info), !.

test(cvt_i_bool, R == 1) :- cvt_i_bool(true, R).
test(cvt_i_bool, R == 1) :- cvt_i_bool(on, R).
test(cvt_i_bool, R == 1) :- cvt_i_bool(1, R).
test(cvt_i_bool, error(type_error(bool,666))) :- cvt_i_bool(666, _R).
test(cvt_i_bool, error(type_error(bool,-666))) :- cvt_i_bool(-666, _R).
:- if(current_prolog_flag(bounded,false)).
test(cvt_i_bool, error(type_error(bool,18446744073709552614))) :-
    Val is 0xffffffffffffffff + 999, % uses extended integers
    cvt_i_bool(Val, _R).
:- endif.
test(cvt_i_bool, R == 0) :- cvt_i_bool(false, R).
test(cvt_i_bool, R == 0) :- cvt_i_bool(off, R).
test(cvt_i_bool, R == 0) :- cvt_i_bool(0, R).
test(cvt_i_bool, error(type_error(bool,'FALSE')))  :- cvt_i_bool('FALSE', _R).
test(cvt_i_bool, error(type_error(bool,0.0)))      :- cvt_i_bool(0.0, _R).
test(cvt_i_bool, error(type_error(bool,"false")))  :- cvt_i_bool("false", _R).

test(scan_options, [R =@= options(1, 5, foo(bar), _, "")]) :- % Note use of (=@=)/2 because of uninstantiated variable
    cpp_options([quoted(true), length(5), callback(foo(bar))], false, R).
test(scan_options, [R == options(1, 5, foo(bar), qqsv, "DESCR")]) :-
    cpp_options([token(qqsv), descr("DESCR"), quoted(true), length(5), callback(foo(bar))], false, R).
test(scan_options, [R == options(1, 5, foo(bar), qqsv, "DESCR")]) :-
    cpp_options([token(qqsv), descr("DESCR"), quoted(true), length(5), callback(foo(bar)), unknown_option(blah)], false, R).
test(scan_options, [error(domain_error(cpp_options,unknown_option(blah)))]) :-
    cpp_options([token(qqsv), descr("DESCR"), quoted(true), length(5), callback(foo(bar)), unknown_option(blah)], true, _).
test(scan_options, [R == options(1, 5, foo(bar), qqsv, "DESCR")]) :-
    cpp_options(options{token:qqsv, descr:"DESCR", quoted:true, length:5, callback:foo(bar)}, false, R).
test(scan_options, [R == options(1, 5, foo(bar), qqsv, "DESCR")]) :-
    cpp_options([token(qqsv), descr("DESCR"), quoted, length(5), callback(foo(bar))], false, R).
test(scan_options, [R == options(0, 5, foo(bar), qqsv, "DESCR")]) :-
    cpp_options([token(qqsv), descr("DESCR"), length(5), callback(foo(bar))], false, R).
test(scan_options, [error(instantiation_error)]) :-
    cpp_options([token(qqsv), _, descr("DESCR"), length(5), callback(foo(bar))], false, _).
test(scan_options, [error(type_error(option,123))]) :- % TODO: is this intended behavior?
    cpp_options([token(qqsv), descr("DESCR"), 123, length(5), callback(foo(bar))], false, _R).
test(scan_options, [error(type_error(option,123))]) :- % TODO: is this intended behavior?
    cpp_options([token(qqsv), 123, descr("DESCR"), length(5), callback(foo(bar))], false, _R).
test(scan_options, [error(domain_error(cpp_options,unknown_option:blah))]) :-
    cpp_options(options{token:qqsv, descr:"DESCR", quoted:true, length:5, callback:foo(bar), unknown_option:blah}, true, _).

test(error_term, error(domain_error(footype,qqsv("ABC")),context(throw_domain_cpp0/1,_Msg))) :-
    throw_domain_cpp0(qqsv("ABC")).

test(error_term, [error(domain_error(footype,qqsv("ABC")),_)]) :-
    throw_domain_cpp1(qqsv("ABC")).

test(error_term, error(domain_error(footype,qqsv("ABC")),context(throw_domain_cpp2/1,_Msg))) :-
    throw_domain_cpp2(qqsv("ABC")).

test(error_term, error(domain_error(footype,qqsv("ABC")),context(throw_domain_cpp3/1,_Msg))) :-
    throw_domain_cpp3(qqsv("ABC")).

test(error_term, [error(domain_error(footype,qqsv("ABC")),_)]) :-
    throw_domain_cpp4(qqsv("ABC")).

test(throw, error(uninstantiation_error(abc),_)) :-
    throw_uninstantiation_error_cpp(abc).

test(throw, error(representation_error(some_resource))) :-
    throw_representation_error_cpp(some_resource).

test(throw, error(type_error(int, "abc"))) :-
    throw_type_error_cpp(int, "abc").

test(throw, error(type_error(float, abc))) :-
    throw_and_check_error_cpp(float, abc).

test(throw, error(domain_error(positive, -5))) :-
    throw_domain_error_cpp(positive, -5).

test(throw, error(existence_error(something_something, foo:bar/2))) :-
    throw_existence_error_cpp(something_something, foo:bar/2).

test(throw, error(permission_error(operation, type, the(culprit)))) :-
    throw_permission_error_cpp(operation, type, the(culprit)).

test(throw, error(resource_error('NO_RESOURCE'))) :-
    throw_resource_error_cpp('NO_RESOURCE').

test(compare) :-
    eq_int64(1, 1).
test(compare, fail) :-
    eq_int64(1, 2).
test(compare, error(type_error(integer,a))) :-
    eq_int64(1, a).
test(compare, error(type_error(integer,b))) :-
    eq_int64(b, 1).
test(compare) :-
    lt_int64(1, 2).
test(compare, fail) :-
    lt_int64(2, 1).
test(compare, error(type_error(integer,a))) :-
    lt_int64(1, a).
test(compare, error(type_error(integer,b))) :-
    lt_int64(b, 1).

test(get_atom, A == abc) :-
    get_atom_ex(abc, A).
test(get_atom) :-
    get_atom_ex(abc, abc).
test(get_atom, fail) :-
    get_atom_ex(abc, abcd).
test(get_atom, error(type_error(atom,"abc"))) :-
    get_atom_ex("abc", _A).
test(get_atom, error(type_error(atom,123))) :-
    get_atom_ex(123, _A).
test(get_atom, error(type_error(atom,foo(bar)))) :-
    get_atom_ex(foo(bar), _A).

test(ten,
     [[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] ==
      [one, two, three, 4, 5.0, "six", seven("SEVEN"), [], true, [hd]]]) :-
    ten(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10).

test(blob) :-
    create_my_blob(foo, Blob),
    assertion(blob(Blob, my_blob)),
    close_my_blob(Blob).
test(blob, error(my_blob_open_error(_),_)) :-
    create_my_blob('FAIL', _).
test(blob, error(my_blob_close_error(Blob))) :-
    create_my_blob('FAIL_close', Blob),
    assertion(blob(Blob, my_blob)),
    close_my_blob(Blob).

% The following attempts to test the handling of close errors in the
% "release" callback, which calls ~MyBlob. It doesn't throw an error
% (because it can't) but does output an error message.
% You can run this test by hand:
%    ?- create_fail_blob.
%    ?- force_gc.
% and that should print:
%    Close MyBlob failed: FAIL_close
test(blob, [blocked(cant_throw_error),
            error(e),
            setup(force_gc(GC_thread)),
            cleanup(restore_gc(GC_thread))]) :-
    create_fail_blob,
    garbage_collect,
    garbage_collect_atoms.
create_fail_blob :-
    create_my_blob('FAIL_close', Blob),
    assertion(blob(Blob, my_blob)).

test(blob, cleanup(close_my_blob(A))) :-
    create_my_blob('foobar', A),
    with_output_to(string(Astr), write(current_output, A)),
    assertion(my_blob_string(Astr, _, _)).

test(blob_compare1, [cleanup((close_my_blob(A),
                             close_my_blob(B)))]) :-
    force_gc,
    create_my_blob('A', A),
    create_my_blob('B', B),
    sort([A,B], Sorted),
    predsort(compare_portray_form, [A,B], Sorted2),
    assertion(Sorted == Sorted2).
test(blob_compare2, [cleanup((close_my_blob(A),
                              close_my_blob(B)))]) :-
    % Create in the opposite order from the previous test,
    % because the addresses ought to be in ascending order.
    force_gc,
    create_my_blob('B', B),
    create_my_blob('A', A),
    % The blobs are repeated here, to verify that the equality check
    % is done by Prolog and never gets to my_data::compare_fields(),
    % which has an assertion check.
    sort([B,A,B,A], Sorted),
    predsort(compare_portray_form, [B,A,B,A], Sorted2),
    assertion(Sorted == Sorted2).
test(blob_compare3, [cleanup((close_my_blob(A1),
                              close_my_blob(A2),
                              close_my_blob(B)))]) :-
    force_gc,
    create_my_blob('A', A1),
    create_my_blob('A', A2),
    create_my_blob('B', B),
    sort([A2,A1,B], Sorted),
    predsort(compare_portray_form, [A1,A2,B], Sorted2),
    assertion(Sorted == Sorted2).
test(blob_compare4, [cleanup((close_my_blob(A1),
                              close_my_blob(A2),
                              close_my_blob(B)))
                    ]) :-
    % Different ordering of creation, so that address order changes
    force_gc,
    create_my_blob('B', B),
    create_my_blob('A', A2),
    create_my_blob('A', A1),
    sort([A2,A1,B], Sorted),
    predsort(compare_portray_form, [A2,A1,B], Sorted2),
    assertion(Sorted == Sorted2).

compare_portray_form(Compare, A, B) :-
    with_output_to(string(Astr), write(A)),
    with_output_to(string(Bstr), write(B)),
    my_blob_string(Astr, APtr, AName),
    my_blob_string(Bstr, BPtr, BName),
    compare(Compare, AName-APtr, BName-BPtr).

my_blob_string(String, Ptr, Name) :-
    atom_codes(String, Codes),
    phrase(my_blob(Ptr, Name), Codes).

my_blob(Ptr, Name) -->
    "<my_blob>(",
    ("0x" -> [] ; []),
    xinteger(Ptr),
    ",name=",
    string(NameS), ")",
    !,
    { atom_codes(Name, NameS) }.

force_gc :-
    force_gc(GC_thread),
    restore_gc(GC_thread).

force_gc(GC_thread) :-
    current_prolog_flag(gc_thread, GC_thread),
    set_prolog_gc_thread(false),
    garbage_collect,
    garbage_collect_atoms.

restore_gc(GC_thread) :-
    set_prolog_gc_thread(GC_thread).


% TODO:
% test this (https://swi-prolog.discourse.group/t/cpp2-exceptions/6040/61):
%
% Now call this from C(++). The first PL_next_solution() says TRUE,
% but the cleanup is not executed. Now close the query. That runs the
% cleanup handler and should raise error. If the goal in the
% setup_call_cleanup/3 completed (fail, exception, deterministic
% true), the cleanup handler has done its work before control gets
% back to Prolog and thus PL_next_solution() already generates the
% exception.

test_setup_call_cleanup(X) :-
    setup_call_cleanup(
        true,
        between(1, 5, X),
        throw(error)).

:- end_tests(cpp).

:- begin_tests(cpp_atommap).

test(atom_atom_map) :-
    atom_atom_add(foo, foo_value),
    atom_atom_add(bar, bar_value),
    atom_atom_add(bar, bar_value), % OK to add identical enntry
    catch(atom_atom_add(foo, foo_value2),
          error(permission_error(add,atom_atom,foo),_),
          Exc = true),
    assertion(Exc == true),
    atom_atom_find(foo, F),
    assertion(F == foo_value),
    assertion(\+ atom_atom_find(foox, _)),
    atom_atom_erase(foo),
    assertion(\+ atom_atom_find(foo, _)),
    atom_atom_erase(non_existent_key),
    atom_atom_erase(bar),
    atom_atom_size(Size),
    assertion(Size == 0).

test(atom_term_map) :-
    % This test uses different keys from the other atom_term_map test.
    % If it succeeds, it will have erased all the entries that were
    % inserted.
    atom_term_insert(foo, foo_value),
    atom_term_insert(bar, bar_value),
    atom_term_insert(bar, bar_value), % OK to add identical enntry
    catch(atom_term_insert(foo, foo_value2),
          error(permission_error(insert,atom_term,foo),_),
          Exc = true),
    assertion(Exc == true),
    atom_term_find(foo, F),
    assertion(F == foo_value),
    assertion(\+ atom_term_find(foox, _)),
    atom_term_erase(foo),
    assertion(\+ atom_term_find(foo, _)),
    atom_term_erase(non_existent_key),
    atom_term_erase(bar),
    atom_atom_size(Size),
    assertion(Size == 0).

test(atom_term_map) :-
    % This test uses different keys from the other atom_term_map test.
    % If it succeeds, it will have erased all the entries that were
    % inserted.
    atom_term_insert(foo2, foo2(value)),
    atom_term_insert(bar2, 123),
    atom_term_find(foo2, F),
    assertion(F == foo2(value)),
    atom_term_find(bar2, B),
    assertion(B == 123),
    assertion(\+ atom_term_find(foo2x, _)),
    atom_term_erase(foo2),
    assertion(\+ atom_term_find(foo2, _)),
    atom_term_erase(non_existent_key),
    atom_term_erase(bar2),
    atom_atom_size(Size),
    assertion(Size == 0).

:- end_tests(cpp_atommap).

w_atom_cpp(Atom, String) :-
    with_output_to(string(String), w_atom_cpp_(current_output, Atom)).

%!  query_flag(?Name, ?Bit)
%
%   Flags for  PL_open_query().  Check  with SWI-Prolog.h.   Same code
%   appears   in  test_ffi.pl.    This  is   duplicated  to   simplify
%   installation of these tests in the binary version.
%
%   This code is mainly for debugging.

query_flag(debug,		I) => I =  0x0001.
query_flag(normal,		I) => I =  0x0002.
query_flag(nodebug,		I) => I =  0x0004.
query_flag(catch_exception,	I) => I =  0x0008.
query_flag(pass_exception,	I) => I =  0x0010.
query_flag(allow_yield,		I) => I =  0x0020.
query_flag(ext_status,		I) => I =  0x0040.
query_flag(deterministic,	I) => I =  0x0100.
% and pseudo-flags (see XX_Q_* flags in test_ffi.c):
query_flag(clear_return_true,   I) => I = 0x01000.
query_flag(close_query,         I) => I = 0x02000.
query_flag(exc_term,            I) => I = 0x04000.

% This should give the same result as PlQuery::verify()
check_query_flag(Flags) :-
    query_flag(normal, F1),
    query_flag(catch_exception, F2),
    query_flag(pass_exception, F3),
    Mask is F1 \/ F2 \/ F3,
    Bits is popcount(Flags /\ Mask),
    (   Bits =< 1
    ->  true
    ;   domain_error(query_flags, Flags)
    ).

query_flags(Flags, CombinedFlag) :-
    maplist(query_flag, Flags, Ints),
    aggregate_all(sum(I), member(I, Ints), CombinedFlag),
    check_query_flag(CombinedFlag).

end_of_file.
