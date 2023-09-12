cmake_minimum_required(VERSION 3.5)
project(swipl-cpp)

include("../cmake/PrologPackage.cmake")

if(MSVC)
set(CMAKE_CXX_STANDARD 20) # Some users of this require C++-17
else()
set(CMAKE_CXX_STANDARD 17) # Some users of this require C++-17
endif()
set(CMAKE_C_STANDARD 11) # swipl requires C-11

configure_file(config.h.cmake config.h)

install_src(pkg_cpp_headers
	    FILES SWI-cpp.h SWI-cpp2.h SWI-cpp2.cpp SWI-cpp2-plx.h SWI-cpp2-atommap.h DESTINATION
	    ${SWIPL_INSTALL_INCLUDE})

swipl_examples(test_cpp.cpp test_ffi.c likes.cpp likes.pl README.md)

pkg_doc(
    pl2cpp
    SOURCES pl2cpp.doc pl2cpp2.doc
    DEPENDS pkg_cpp_headers)

# FFI tests. The `TEST_ONLY` for the plugin declaration prevents
# installing the generated module.

if(NOT STATIC_EXTENSIONS)
test_libs(ffi cpp)

swipl_plugin(
    test_ffi
    TEST_ONLY
    MODULE test_ffi
    C_SOURCES test_ffi.c)

swipl_plugin(
    test_cpp
    TEST_ONLY
    MODULE test_cpp
    C_LIBS ${SOCKET_LIBRARIES}
    C_SOURCES test_cpp.cpp SWI-cpp2.cpp)
endif()
