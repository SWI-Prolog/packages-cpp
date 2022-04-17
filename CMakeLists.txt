cmake_minimum_required(VERSION 3.5)
project(swipl-cpp)

include("../cmake/PrologPackage.cmake")

install_src(pkg_cpp_headers
	    FILES SWI-cpp.h DESTINATION
	    ${SWIPL_INSTALL_INCLUDE})

swipl_examples(test.cpp likes.cpp likes.pl test.pl README.md)

# add_subdirectory(Tests) # DO NOT SUBMIT - remove this line

pkg_doc(
    pl2cpp
    DEPENDS pkg_cpp_headers)

test_libs(ffi)

swipl_plugin(
    test_ffi
    MODULE range_ffi4pl
    C_SOURCES range_ffi4pl.c
    C_LIBS
    PL_LIBS test_ffi.pl)
