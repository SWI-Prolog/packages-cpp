cmake_minimum_required(VERSION 3.5)
project(swipl-cpp)

include("../cmake/PrologPackage.cmake")

install_src(pkg_cpp_headers
	    FILES SWI-cpp.h DESTINATION
	    ${SWIPL_INSTALL_INCLUDE})

swipl_examples(test.cpp likes.cpp likes.pl test.pl README.md)

pkg_doc(
    pl2cpp
    DEPENDS pkg_cpp_headers)

# FFI tests. The `TEST_ONLY` for the plugin declaration prevents
# installing the generated module.

test_libs(ffi)

swipl_plugin(
    test_ffi
    TEST_ONLY
    MODULE range_ffi4pl
    C_SOURCES range_ffi4pl.c)
