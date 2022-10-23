# SWI-Prolog C++ interface

## Embedding SWI-Prolog in a C++ program

The files likes.pl and likes.cpp  provide   a  simple  example embedding
SWI-Prolog. To compile, run

    swipl-ld -o likes likes.cpp likes.pl

Next, run as e.g.

    ./likes john
    ./likes -happy

## Extending SWI-Prolog using C++ code

The file `test_cpp.cpp` adds foreign predicates to SWI-Prolog.
To compile, run

    swipl-ld -shared -o test_cpp test_cpp.cpp

Next, run as e.g.

    swipl test.pl
    ?- use_foreign_library(test_cpp).
    ?- hello(world).
    Hello world
    true.

## Testing

This package also provides tests  for   the  native C SWI-Prolog foreign
language interface because this is the most   convenient  place to do so
and the C++ interface depends on the   C  interface. Most of the testing
thereof is indirect through its usage in the core system as well as many
of the packages.
