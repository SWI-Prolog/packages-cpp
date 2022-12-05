% -*- mode: Prolog; coding: utf-8 -*-

/*  Part of SWI-Prolog

    Author:        Peter Ludemann
    E-mail:        peter.ludemann@gmail.com
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

% This tests the C interface and not the C++ interface.
% But it was most convenient to put the test here.

:- module(test_ffi,
          [ test_ffi/0
          ]).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- autoload(library(aggregate)).
:- use_module(library(plunit)).

:- use_foreign_library(foreign(test_ffi)).

:- encoding(utf8).

test_ffi :-
    run_tests([ ffi,
                wchar
	      ]).

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

:- end_tests(ffi).


:- begin_tests(wchar).

% The following "wchar" tests are regression tests related
% to https://github.com/SWI-Prolog/packages-pcre/issues/20

test(wchar_1, all(Result == ["//0",
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

test(wchar_2,
     [condition(\+ current_prolog_flag(windows, true)), % Windows doesn't like Unicode > 0xffff
      all(Result == ["/⛰⛱⛲⛾⛿/5","/\U0001FB00/1","/ᢰᢱ\x18FF\/3","/⻰⻱⻲⻳/4"])]) :-
    (   w_atom_ffi('⛰⛱⛲⛾⛿', Result)
    ;   w_atom_ffi('\U0001FB00', Result)
    ;   w_atom_ffi('ᢰᢱ\u18FF', Result)
    ;   w_atom_ffi('⻰⻱⻲⻳', Result)
    ).

test(wchar_2b, % Same as wchar_2, but uses atom_codes
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

ffi_call(Goal, Flags) :-
    query_flags(Flags, CombinedFlag),
    ffi_call_(Goal, CombinedFlag).

:- end_tests(wchar).


w_atom_ffi(Atom, String) :-
    with_output_to(string(String), w_atom_ffi_(current_output, Atom)).

atom_ffi(Atom, String) :-
    with_output_to(string(String), atom_ffi_(current_output, Atom)).

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
