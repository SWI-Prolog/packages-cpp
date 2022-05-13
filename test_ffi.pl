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

:- encoding(utf8).

:- use_module(library(plunit)).

:- use_foreign_library(foreign(ffi4pl)).

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

:- end_tests(ffi).


:- begin_tests(wchar).

% The following "wchar" tests are regression tests related
% to https://github.com/SWI-Prolog/packages-pcre/issues/20

test(wchar_1, all(Result == ["//0", "/ /1", "/abC/3", "/Hello World!/12", "/хелло/5", "/хелло 世界/8", "/網目錦へび [àmímé níshíkíhéꜜbì]/26"])) :-
    (   w_atom_ffi('',             Result)
    ;   w_atom_ffi(' ',            Result)
    ;   w_atom_ffi('abC',          Result)
    ;   w_atom_ffi('Hello World!', Result)
    ;   w_atom_ffi('хелло',        Result)
    ;   w_atom_ffi('хелло 世界',   Result)
    ;   w_atom_ffi('網目錦へび [àmímé níshíkíhéꜜbì]', Result)
    ).

test(char_1, all(Result == ["//", "/ /", "/abC/", "/Hello World!/"])) :-
    (   atom_ffi('',               Result)
    ;   atom_ffi(' ',              Result)
    ;   atom_ffi('abC',            Result)
    ;   atom_ffi('Hello World!',   Result)
    ).

:- end_tests(wchar).


w_atom_ffi(Atom, String) :-
    with_output_to(string(String), w_atom_ffi_(current_output, Atom)).

atom_ffi(Atom, String) :-
    with_output_to(string(String), atom_ffi_(current_output, Atom)).
