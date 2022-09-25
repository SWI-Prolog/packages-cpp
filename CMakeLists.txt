cmake_minimum_required(VERSION 3.5)
project(swipl-cpp)

add_compile_options(-Wall -Wextra -Wconversion -Warith-conversion -Wsign-conversion -Wfloat-conversion -Wno-unused-parameter -Werror)

include("../cmake/PrologPackage.cmake")
include("../cmake/Sockets.cmake")

set(CMAKE_CXX_STANDARD 17) # TODO: is this needed?

check_function_exists(gethostname HAVE_GETHOSTNAME)
configure_file(config.h.cmake config.h)

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
    C_LIBS ${SOCKET_LIBRARIES}
    C_SOURCES cpp4pl.cpp)
