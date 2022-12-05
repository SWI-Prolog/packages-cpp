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
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- autoload(library(aggregate)).
:- use_module(library(plunit)).

:- encoding(utf8).

:- use_foreign_library(foreign(test_cpp)).

test_cpp :-
    run_tests([ cpp
	      ]).

:- begin_tests(cpp).

test(hello, Out == "Hello world\nHello world\nHello world\nHello world\nHello world\n") :-
    hello(world, Out).

test(hello2, Out == "Hello2 world2\nHello2 world2\nHello2 world2\nHello2 world2\nHello2 world2\n") :-
    hello2(world2, Out).

test(hello3, Out == "Hello3 世界弐\n") :-
    hello3(世界弐, Out).

test(hello_call, Out == "hello(foo)\n") :-
    with_output_to(string(Out), hello_call(writeln(hello(foo)))).
test(hello_call, Out == "hello(世界四)\n") :-
    with_output_to(string(Out), hello_call(writeln(hello(世界四)))).
test(hello_call, error(existence_error(procedure,writeln_wrong/1))) :-
    hello_call(writeln_wrong(hello(世界四))).
test(hello_call, fail) :-
    hello_call(atom(hello(foo))).

test(hello_query, Out == "hello(世界四)\n") :-
    with_output_to(string(Out), hello_query(writeln, hello(世界四))).
test(hello_query, error(existence_error(procedure,writeln_wrong/1))) :-
    hello_query(writeln_wrong, hello(世界四)).
test(hello_query, fail) :-
    hello_query(atom, hello(foo)).

test(as_string, S == "foo") :-
    atom_to_string(foo, S).
test(as_string, S = "foo(bar)") :-
    term_to_string(foo(bar), S).

% Note: atom_to_string/2 and term_to_string/2 translate the data
% to a UTF-8 string.  We currenly do not support encoding for
% PlTerm.unify_string(), so we get as result the byte encoding
% of the UTF8 data.
test(as_string, S == "ä¸\u0096ç\u0095\u008Cå\u009B\u009B") :-
    atom_to_string(世界四, S).
test(as_string, S = "hello(ä¸\u0096ç\u0095\u008Cå\u009B\u009B)") :-
    term_to_string(hello(世界四), S).

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

test(average_3, Average =:= Expected) :-
    average(X, testing:p(X), Average),
    Expected is (1+10+20)/3 .

test(hello_0, Out == "hello world\n") :-
    with_output_to(string(Out), hello).

call_cut_test :-
    setup_call_cleanup(true,
		       between(1, 5, _X),
		       atom_codes(_,_)).

test(call_cut, error(existence_error(procedure,call_cut_test/0))) :-
    % This tests that an error in ~PlQuery() is handled properly
    % See discussion: https://github.com/SWI-Prolog/packages-cpp/pull/27
    call_cut("call_cut_test").

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

test(cpp_call, Out == "abc\n") :-
    with_output_to(string(Out),
		   cpp_call(writeln(abc), [normal])).

cpp_call(Goal, Flags) :-
    query_flags(Flags, CombinedFlag),
    cpp_call_(Goal, CombinedFlag, false).


test(square_roots_2a, Result == [0.0, 1.0, 1.4142135623730951, 1.7320508075688772, 2.0]) :-
    square_roots(5, Result).

:- meta_predicate with_small_stacks(+, 0).
with_small_stacks(Free, Goal) :-
    garbage_collect,
    statistics(globalused, G),
    statistics(trailused, T),
    statistics(localused, L),
    NewLimit is G+L+T+Free,
    current_prolog_flag(stack_limit, Old),
    setup_call_cleanup(
	set_prolog_flag(stack_limit, NewLimit),
	Goal,
	set_prolog_flag(stack_limit, Old)).

test(square_roots_2b, error(resource_error(stack))) :-
    with_small_stacks(5 000 000, % 400 000 seems to be about the smallest allowed value
		      square_roots(1000000000, _)).

test(malloc) :-
    malloc(1000, Result), % smoke test
    free(Result).

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

test(malloc) :-
    too_big_alloc_request(Request),
    malloc(Request, Result),
    assertion(Result == 0),
    free(Result).

:- if(current_prolog_flag(bounded,false)).

test(malloc) :-
    too_many_bits_alloc_request(Request),
    catch( ( malloc(Request, Result),
	     free(Result)
	   ),
	   error(E,_), true),
    assertion(memberchk(E, [representation_error(_),
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

test(new_chars_3) :-
    too_many_bits_alloc_request(Request),
    catch( ( new_chars(Request, Result),
	     delete_chars(Result)
	   ),
	   error(E,_), true),
    assertion(memberchk(E, [representation_error(_),
			    type_error(integer,_)])).

:- endif.
:- endif.

test(new_chars_1) :-
    new_chars(1000, Result), % smoke test
    delete_chars(Result).

test(name_arity_1, Out == "name = foo, arity = 2\n") :-
    name_arity(foo(bar,zot), Out).

test(name_arity_3) :-
    name_arity(foo(bar,zot), Name, Arity),
    assertion(Name == foo),
    assertion(Arity == 2).

test(list_modules_0) :-
    % TODO: this outputs to cout ... make a version that checks the output?
    list_modules(Text),
    split_string(Text, "\n", "", Strings),
    forall(( member(S, Strings), S \== ""),
	   ( atom_string(M, S),
	     current_module(M))).

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

test(int_info) :-
    findall(Name:Info, int_info(Name, Info), Infos),
    assertion(memberchk(uint32_t:int_info(uint32_t,4,0,4294967295), Infos)).
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

% TODO: the following sometimes causes a crash:
test(scan_options, [R = options(1, 5, foo(bar), _, "")]) :- % Note use of (=)/2 because of uninstantiated variable
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

test(error_term, error(domain_error(footype,qqsv("ABC")),context(throw_domain_ffi/1,_Msg))) :-
    throw_domain_ffi(qqsv("ABC")).

test(error_term, [error(domain_error(footype,qqsv("ABC")),_)]) :-
    throw_domain_cpp1(qqsv("ABC")).

test(error_term, error(domain_error(footype,qqsv("ABC")),context(throw_domain_cpp2/1,_Msg))) :-
    throw_domain_cpp2(qqsv("ABC")).

test(error_term, error(domain_error(footype,qqsv("ABC")),context(throw_domain_cpp3/1,_Msg))) :-
    throw_domain_cpp3(qqsv("ABC")).

test(error_term, [error(domain_error(footype,qqsv("ABC")),_)]) :-
    throw_domain_cpp4(qqsv("ABC")).

:- end_tests(cpp).

w_atom_cpp(Atom, String) :-
    with_output_to(string(String), w_atom_cpp_(current_output, Atom)).

%!  query_flag(?Name, ?Bit)
%
%   Flags for  PL_open_query().  Check  with SWI-Prolog.h.   Same code
%   appears   in  test_ffi.pl.    This  is   duplicated  to   simplify
%   installation of these tests in the binary version.

% query_flag(debug,		I) => I =0x0001.
% query_flag(deterministic,	I) => I =0x0100.
query_flag(normal,		I) => I =0x0002.
query_flag(nodebug,		I) => I =0x0004.
query_flag(catch_exception,	I) => I =0x0008.
query_flag(pass_exception,	I) => I =0x0010.
query_flag(allow_yield,		I) => I =0x0020.
query_flag(ext_status,		I) => I =0x0040.

query_flags(Flags, CombinedFlag) :-
    maplist(query_flag, Flags, Ints),
    aggregate_all(sum(I), member(I, Ints), CombinedFlag).
