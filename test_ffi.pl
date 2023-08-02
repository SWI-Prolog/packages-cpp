% -*- mode: Prolog; coding: utf-8 -*-

/*  Part of SWI-Prolog

    Author:        Peter Ludemann
    E-mail:        peter.ludemann@gmail.com
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

% This tests the C interface and not the C++ interface.
% But it was most convenient to put the test here.

:- module(test_ffi,
          [ test_ffi/0
          ]).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- autoload(library(aggregate)).
:- use_module(library(memfile)).
:- use_module(library(readutil)).
:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).

:- use_foreign_library(foreign(test_ffi)).

:- encoding(utf8).

test_ffi :-
    run_tests([ ffi,
                wchar,
                scan,
                call
	      ]).

% Some of the tests can result in crashes if there's a bug, so the
% `output(on_failure)` option results in nothing being written.
:- set_test_options([output(always)]).

:- begin_tests(ffi).

test(range1, all(X == [1,2])) :-
    range_ffi(1, 3, X).
test(range2, all(X == [-2,-1,0,1,2])) :-
    range_ffi(-2, 3, X).
test(range3a, all(X == [-2])) :-
    range_ffi(-2, -1, X).
test(range3b, all(X == [0])) :-
    range_ffi(0, 1, X).
test(range3c, all(X == [10])) :-
    range_ffi(10, 11, X).
test(range4a, fail) :-
    range_ffi(1, 1, _X).
test(range4b, fail) :-
    range_ffi(0, 0, _X).
test(range4c, fail) :-
    range_ffi(-1, -1, _X).
test(range4d, fail) :-
    range_ffi(1, 2, 2).
test(range_ffi5, X == 1) :- % Will produce warning if non-deterministic
    range_ffi(1, 2, X).

test(range_ffialloc1, all(X == [1,2])) :-
    range_ffialloc(1, 3, X).
test(range_ffialloc2, all(X == [-2,-1,0,1,2])) :-
    range_ffialloc(-2, 3, X).
test(range_ffialloc3a, all(X == [0])) :-
    range_ffialloc(0, 1, X).
test(range_ffialloc3b, all(X == [10])) :-
    range_ffialloc(10, 11, X).
test(range_ffialloc3c, all(X == [-2])) :-
    range_ffi(-2, -1, X).
test(range_ffialloc4a, fail) :-
    range_ffialloc(1, 1, _X).
test(range_ffialloc4a, fail) :-
    range_ffialloc(0, 0, _X).
test(range_ffialloc4a, fail) :-
    range_ffialloc(-1, -1, _X).
test(range_ffialloc4d, fail) :-
    range_ffialloc(1, 2, 2).
test(range_ffialloc5, X == 1) :- % Will produce warning if non-deterministic
    range_ffialloc(1, 2, X).
test(range_ffialloc6a, error(type_error(integer,a))) :-
    range_ffialloc(a, 10, _).
test(range_ffialloc6b, error(type_error(integer,foo))) :-
    range_ffialloc(1, foo, _).

test(make_list, Env == ['SHELL=/bin/bash', 'TERMCAP=', 'PWD=/home/programmer/src/swipl-devel', 'LANG=en_US.UTF-8']) :-
    ffi_get_environ1(Env).
test(make_list, Env == ['SHELL=/bin/bash', 'TERMCAP=', 'PWD=/home/programmer/src/swipl-devel', 'LANG=en_US.UTF-8']) :-
    ffi_get_environ2(Env).

test(get_list, Output == "fred\ncharles\nmindy\n") :-
    with_output_to(string(Output),
                   ffi_write_atoms(current_output, [fred,charles,mindy])).
test(get_list, error(existence_error(stream,unknown_stream))) :-
    ffi_write_atoms(unknown_stream, [fred,charles,mindy]).
test(get_list, error(type_error(list,x))) :-
    ffi_write_atoms(current_output, x).
test(get_list, error(type_error(list,mindy))) :-
    % This will put "fred\ncharles\n" into Output, but that will be
    % undone by the error. (The behavior can be observed by outputting
    % to a stream, which doesn't backtrack)
    with_output_to(string(_Output),
                   ffi_write_atoms(current_output, [fred,charles|mindy])).
test(get_list, error(instantiation_error)) :-
    ffi_write_atoms(current_output, [_X]).
test(get_list, error(type_error(atom,1.0))) :-
    ffi_write_atoms(current_output, [1.0]).
test(get_list, error(type_error(atom,"foo"))) :-
    ffi_write_atoms(current_output, ["foo"]).

test(save_load_int64, L == L2) :-
    Mx is (1<<63)-1, Mn is -(1<<63),
    L = [150, 0, -150, Mx, Mn],
    tmp_file_stream(TmpFile, OutStream, [encoding(binary)]),
    maplist(ffi_write_int64(OutStream), L),
    close(OutStream),
    same_length(L, L2),
    open(TmpFile, read, InStream, [type(binary)]),
    maplist(ffi_read_int64(InStream), L2),
    close(InStream),
    read_file_to_codes(TmpFile, Codes, [type(binary)]),
    % The following should be the same on both little- and big-endian machines.
    assertion(Codes == [0x2c,0x82,0x80,0x2b,0x82,0x7e,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x81,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x81]).
test(save_load_int32, L == L2) :-
    L = [-1, 0, 0x010203fe, 0x7fffffff, -0x8000000],
    tmp_file_stream(TmpFile, OutStream, [encoding(binary)]),
    maplist(ffi_write_int32(OutStream), L),
    close(OutStream),
    same_length(L, L2),
    open(TmpFile, read, InStream, [type(binary)]),
    maplist(ffi_read_int32(InStream), L2),
    close(InStream),
    read_file_to_codes(TmpFile, Codes, [type(binary)]),
    % The following should be the same on both little- and big-endian machines.
    % assertion(Codes == [0xff,0xff,0xff,0xff,0,0,0,0,1,2,3,0xfe,0x7f,0xff,0xff,0xff,0xf8,0,0,0]).
    % If int32_t is encoded using zigzag, this is the result:
    assertion(Codes == [129,128,124,15,16,144,126,127,127,127,143,127,127,127,255]).

test(save_load_int64, L == L2) :-
    Mx is (1<<63)-1, Mn is -(1<<63),
    L = [150, 0, -150, Mx, Mn],
    new_memory_file(MemFile),
    open_memory_file(MemFile, write, OutStream, [type(octet)]),
    maplist(ffi_write_int64(OutStream), L),
    close(OutStream),
    same_length(L, L2),
    open_memory_file(MemFile, read, InStream, [type(octet)]),
    maplist(ffi_read_int64(InStream), L2),
    close(InStream),
    memory_file_to_codes(MemFile, Codes, octet),
    % TODO: the following should be the same on both little- and
    %       big-endian machines.
    assertion(Codes == [0x2c,0x82,0x80,0x2b,0x82,0x7e,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x81,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x7f,0x81]).

test(throw, error(instantiation_error,context(test_ffi:throw_instantiation_error_ffi/1,_))) :-
    throw_instantiation_error_ffi(_X).
test(throw, error(uninstantiation_error(abc),context(test_ffi:throw_uninstantiation_error_ffi/1,_))) :-
    throw_uninstantiation_error_ffi(abc).

test(throw, error(representation_error(some_resource))) :-
    throw_representation_error_ffi(some_resource).

test(throw, error(type_error(int,"abc"))) :-
    throw_type_error_ffi(int, "abc").

test(throw, error(domain_error(positive, -5))) :-
    throw_domain_error_ffi(positive, -5).

test(throw, error(existence_error(something_something, foo:bar/2))) :-
    throw_existence_error_ffi(something_something, foo:bar/2).

test(throw, error(permission_error(operation, type, the(culprit)))) :-
    throw_permission_error_ffi(operation, type, the(culprit)).

test(throw, error(resource_error('NO_RESOURCE'))) :-
    throw_resource_error_ffi('NO_RESOURCE').

:- end_tests(ffi).


:- begin_tests(wchar).

% The following "wchar" tests are regression tests related
% to https://github.com/SWI-Prolog/packages-pcre/issues/20

test(wchar,   all(Result == ["//0",
                             "/ /1",
                             "/abC/3",
                             "/Hello World!/12",
                             "/хелло/5",
                             "/хелло 世界/8",
                             "/網目錦へび [àmímé níshíkíhéꜜbì]/26"])) :-
    (   w_atom_ffi('',             Result)
    ;   w_atom_ffi(' ',            Result)
    ;   w_atom_ffi('abC',          Result)
    ;   w_atom_ffi('Hello World!', Result)
    ;   w_atom_ffi('хелло',        Result)
    ;   w_atom_ffi('хелло 世界',   Result)
    ;   w_atom_ffi('網目錦へび [àmímé níshíkíhéꜜbì]', Result)
    ).

test(wchar,
     [condition(\+ current_prolog_flag(windows, true)), % Windows doesn't like Unicode > 0xffff
      all(Result == ["/⛰⛱⛲⛾⛿/5","/\U0001FB00/1","/ᢰᢱ\x18FF\/3","/⻰⻱⻲⻳/4"])]) :-
    (   w_atom_ffi('⛰⛱⛲⛾⛿', Result)
    ;   w_atom_ffi('\U0001FB00', Result)
    ;   w_atom_ffi('ᢰᢱ\u18FF', Result)
    ;   w_atom_ffi('⻰⻱⻲⻳', Result)
    ).

test(wchar, % Same as wchar_2, but uses atom_codes
     [condition(\+ current_prolog_flag(windows, true)), % Windows doesn't like Unicode > 0xffff
      all(Result == [[47, 0x26f0, 0x26f1, 0x26f2, 0x26fe, 0x26ff, 47, 53],
                     [47, 0x1FB00, 47, 49],
                     [47, 0x18b0, 0x18b1, 0x18ff, 47, 51],
                     [47, 0x2ef0, 0x2ef1, 0x2ef2, 0x2ef3, 47, 52]])]) :-
    (   atom_codes(A, [0x26f0, 0x26f1, 0x26f2, 0x26fe, 0x26ff]),
        w_atom_ffi(A, Result0), string_codes(Result0, Result)
    ;   atom_codes(A, [0x1FB00]),
        w_atom_ffi(A, Result0), string_codes(Result0, Result)
    ;   atom_codes(A, [0x18b0, 0x18b1, 0x18ff]),
        w_atom_ffi(A, Result0), string_codes(Result0, Result)
    ;   atom_codes(A, [0x2ef0, 0x2ef1, 0x2ef2, 0x2ef3]),
        w_atom_ffi(A, Result0), string_codes(Result0, Result)
    ).

test(char_1, all(Result == ["//", "/ /", "/abC/", "/Hello World!/"])) :-
    (   atom_ffi('',               Result)
    ;   atom_ffi(' ',              Result)
    ;   atom_ffi('abC',            Result)
    ;   atom_ffi('Hello World!',   Result)
    ).

w_atom_ffi(Atom, String) :-
    with_output_to(string(String), w_atom_ffi_(current_output, Atom)).

atom_ffi(Atom, String) :-
    with_output_to(string(String), atom_ffi_(current_output, Atom)).

:- end_tests(wchar).


:- begin_tests(scan).

test(scan_options, [Options == options(true, 5, foo(bar), [], "")]) :-
    ffi_options(Options, [quoted(true), length(5), callback(foo(bar))]).
test(scan_options, [Options == options(true, 5, foo(bar), qqsv, "DESCR")]) :-
    ffi_options(Options, [token(qqsv), descr("DESCR"), quoted(true), length(5), callback(foo(bar))]).
test(scan_options, [Options == options(true, 5, foo(bar), qqsv, "DESCR")]) :-
    ffi_options(Options, [token(qqsv), descr("DESCR"), quoted(true), length(5), callback(foo(bar)), unknown_option(blah)]).
test(scan_options, [Options == options(true, 5, foo(bar), qqsv, "DESCR")]) :-
    ffi_options(Options, options{token:qqsv, descr:"DESCR", quoted:true, length:5, callback:foo(bar)}).
test(scan_options, [Options == options(true, 5, foo(bar), qqsv, "DESCR")]) :-
    ffi_options(Options, [token(qqsv), descr("DESCR"), quoted, length(5), callback(foo(bar))]).
test(scan_options, [Options == options(false, 5, foo(bar), qqsv, "DESCR")]) :-
    ffi_options(Options, [token(qqsv), descr("DESCR"), length(5), callback(foo(bar))]).
test(scan_options, [error(instantiation_error)]) :-
    ffi_options(_Options, [token(qqsv), _, descr("DESCR"), length(5), callback(foo(bar))]).
test(scan_options, [error(type_error(option,123))]) :- % TODO: is this intended behavior?
    ffi_options(_Options, [token(qqsv), descr("DESCR"), 123, length(5), callback(foo(bar))]).
test(scan_options, [error(type_error(option,123))]) :- % TODO: is this intended behavior?
    ffi_options(_Options, [token(qqsv), 123, descr("DESCR"), length(5), callback(foo(bar))]).

:- end_tests(scan).


:- begin_tests(call).

% New code should use PL_Q_PASS_EXCEPTION or PL_Q_CATCH_EXCEPTION, so
% there are only minimal tests for PL_Q_NORMAL or flags=0.

% In the following tests, the `FlagsStr` is specified, as a
% double-check that query_flags/2 has generated the correct bits in
% the flag.

test(ffi_call, X == a) :-
    ffi_call(X = a, [normal], "normal").
test(ffi_call, X == a) :-
    ffi_call(X = a, [pass_exception], "pass_exception").
test(ffi_call, X == a) :-
    ffi_call(X = a, [catch_exception], "catch_exception").

test(ffi_call) :-
    ffi_call(unknown_pred(foo), [nodebug,pass_exception,clear_return_true],
             "nodebug,pass_exception,clear_return_true",
             Exc_0, Exc_qid, Exc_0_2, NextRc),
    assertion(NextRc == 0),
    assertion(Exc_0 == Exc_0_2),
    (   match_existence_error_string(Exc_0, MatchExc_0),
        match_existence_error_string(Exc_qid, MatchExc_qid)
    ->  % The terms from Exc_0 and Exc_qid are different
        assertion(MatchExc_0 \== MatchExc_qid)
    ;   assertion(fail)
    ).
test(ffi_call) :-
    ffi_call(unknown_pred(foo), [nodebug,catch_exception,clear_return_true],
             "nodebug,catch_exception,clear_return_true",
             Exc_0, Exc_qid, Exc_0_2, NextRc),
    assertion(NextRc == 0),
    assertion(Exc_0 == "<null-term>"),
    assertion(Exc_0_2 == "<null-term>"),
    assertion(match_existence_error_string(Exc_qid, _MatchExc_qid)).

test(ffi_call) :-
    ffi_call(unknown_pred(foo), [nodebug,pass_exception,clear_return_true,exc_term],
             "nodebug,pass_exception,clear_return_true,exc_term",
             Exc_0, Exc_qid, Exc_0_2, NextRc),
    assertion(NextRc == 0),
    assertion(Exc_0 == Exc_0_2),
    match_existence_error_term(Exc_0),
    match_existence_error_term(Exc_qid).
test(ffi_call) :-
    ffi_call(unknown_pred(foo), [nodebug,catch_exception,clear_return_true,exc_term],
             "nodebug,catch_exception,clear_return_true,exc_term",
             Exc_0, Exc_qid, Exc_0_2, NextRc),
    assertion(NextRc == 0),
    assertion(Exc_0 == "<null-term>"),
    assertion(Exc_0_2 == "<null-term>"),
    match_existence_error_term(Exc_qid).

%! match_existence_error_string(+Str, -Qid).
% Utility predicate for checking that a term, when turned into a string,
% matches a particular existence error.
% Str: the error term, as a string
% Matches: gets a dict with:
%            1: the term_t value as a string
%            2: the contents of context(...) as a string
match_existence_error_string(Str, Qid) :-
    string_codes(Str, Codes),
    phrase(("<", integer(Qid), ">:error(existence_error(procedure,test_ffi:unknown_pred/1"), Codes, _Rest),
    !.

match_existence_error_term(Term) :-
    assertion(subsumes_term(error(existence_error(procedure, test_ffi:unknown_pred/1),
                                  context(_,_)), Term)).

test(ffi_call_no_options, blocked('Activates trace/debug mode')) :-
    ffi_call(non_existant_pred(foo), [], "").
test(ffi_call_normal, blocked('Invokes debugger')) :-
    ffi_call(non_existant_pred(foo), [normal], "normal").
test(ffi_call_normal, blocked('Invokes debugger')) :-
    catch(ffi_call(non_existant_pred(foo), [normal], "normal"), _E, true).

ffi_call(Goal, Flags) :-
    query_flags(Flags, CombinedFlag),
    ffi_call_exc(Goal, CombinedFlag, _, _, _, _).

ffi_call(Goal, Flags, FlagsStr) :-
    query_flags(Flags, CombinedFlag),
    query_flags_str(CombinedFlag, FlagsStr),
    ffi_call_exc(Goal, CombinedFlag, _, _, _, _).

ffi_call(Goal, Flags, FlagsStr, Exc_0, Exc_qid, Exc_0_2, Rc) :-
    query_flags(Flags, CombinedFlag),
    query_flags_str(CombinedFlag, FlagsStr),
    ffi_call_exc(Goal, CombinedFlag, Exc_0, Exc_qid, Exc_0_2, Rc).

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

ffi_p(a).
ffi_p(foo(bar)).
ffi_p(1).
ffi_p("xyz").

ffi_findall_p(L) :-
    ffi_findall_(ffi_p(_), L).

ffi_findall_bug_p(L) :-
    ffi_findall_bug_(ffi_p(_), L).

:- end_tests(call).
