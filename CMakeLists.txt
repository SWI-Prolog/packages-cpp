cmake_minimum_required(VERSION 3.5)
project(swipl-cpp)

include("../cmake/PrologPackage.cmake")

set(CMAKE_CXX_STANDARD 17) # TODO: is this needed?

install_src(pkg_cpp_headers
	    FILES SWI-cpp.h SWI-cpp2.h DESTINATION
	    ${SWIPL_INSTALL_INCLUDE})

swipl_examples(cpp4pl.cpp likes.cpp likes.pl README.md)

pkg_doc(
    pl2cpp
    DEPENDS pkg_cpp_headers)

# FFI tests. The `TEST_ONLY` for the plugin declaration prevents
# installing the generated module.

test_libs(ffi cpp)

swipl_plugin(
    test_ffi
    TEST_ONLY
    MODULE ffi4pl
    C_SOURCES ffi4pl.c)

swipl_plugin(
    test_cpp
    TEST_ONLY
    MODULE cpp4pl
    C_SOURCES cpp4pl.cpp)
