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
