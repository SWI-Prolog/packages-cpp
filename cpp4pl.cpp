/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
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

/* This is used by test_cpp.pl */
/* Most of these predicates are from test.cpp or the documentation.*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code may be compiled using

    swipl-ld -shared -o cpp4pl cpp4pl.cpp

and subsequently loading using

    swipl
    ?- use_foreign_library(cpp4pl).

Next, run example predicates such as below.  Scan through this file
to find the predicates provided by this C++ code.

    ?- hello(world).
    Hello world

This code is also used by test_cpp.pl, which has many examples of
how the various predicates can be called from Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define PROLOG_MODULE "user"
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "SWI-cpp2.h"
#include <iostream>
#include <unistd.h>
#include <math.h>
#include <cassert>
#include <string>
using namespace std;


PREDICATE(hello, 1)
{ cout << "Hello " << A1.c_str() << endl;
  cout << "Hello " << A1.string() << endl; /* Same output as previous line */

  return true;
}

PREDICATE(hello2, 1)
{ PlAtom atom_a1(A1);
  cout << "Hello2 " << atom_a1.string() << endl; /* Same output as hello/1 */
  return true;
}

PREDICATE(hello3, 1)
{ PlAtom atom_a1(A1);

  // Iostream doesn't work because `<<` doesn't support std::wstring:
  //   cout << "Hello3 " << atom_a1.wstring() << endl; /* Same output as hello/1 */

  // If %s is used, an error will occur if A1 has a non-ascii
  // character in it. In addition, a NUL ('\0') in the atom will cause
  // the rest of the atom to not be printed.
  if ( Sfprintf(Suser_output, "Hello3 %Ws\n", atom_a1.wc_str()) > 0 )
    return true;
  return false;
}

PREDICATE(add, 3)
{ // as_long() converts integral floats to integers
  return A3.unify_integer(A1.as_long() + A2.as_long());
}

PREDICATE(add_num, 3)
{ auto x = A1, y = A2, result = A3;
  // Note that as_float() handles floats
  double sum = x.as_float() + y.as_float();
  if ( double(long(sum)) == sum ) /* Can float be represented as int? */
    return result.unify_integer(long(sum));
  return result.unify_float(sum);
}

PREDICATE(name_arity, 1)
{ cout << "name = " << A1.name() << ", arity = " << A1.arity() << endl;

  return true;
}

PREDICATE(name_arity, 3)		/* name_arity(+Term, -Name, -Arity) */
{ PlTerm term(A1);
  PlTerm name(A2);
  PlTerm arity(A3);

  name.unify_atom_check(term.name());
  arity.unify_integer_check(term.arity());

  return true;
}

PREDICATE(list_modules, 0)
{ PlTermv av(1);

  PlQuery q("current_module", av);
  while( q.next_solution() )
    cout << av[0].string() << endl;

  return true;
}

PREDICATE(average, 3)			/* average(+Templ, :Goal, -Average) */
{ long sum = 0;
  long n = 0;

  /* Some compilers (e.g., MSVC) require the following code:
     PlTermv av(A2);
     PlQuery q("call", av);
  */
  PlQuery q("call", PlTermv(A2));
  while( q.next_solution() )
  { sum += A1.as_long();
    n++;
  }
  return A3.unify_float(double(sum) / double(n));
}

PREDICATE(hello, 0)
{ PlQuery q("write", PlTermv(PlTerm_atom("hello world\n")));
  return q.next_solution();
}

PREDICATE(term, 1)
{ return A1.unify_term(PlCompound("hello", PlTermv(PlTerm_atom("world"))));
}

PlAtom ATOM_atom("atom");

PREDICATE(term, 2)
{ PlAtom a(A1);

  if ( a.C_ == ATOM_atom.C_ )
    return A2.unify_atom("hello world"); // or A2.unify_term(PlAtom("hello world"));
  if ( A1.string() == "string" )
    return A2.unify_string("hello world");
  if ( A1.string() == "code_list" )
    return A2.unify_list_codes("hello world");
  if ( A1.string() == "char_list" )
    return A2.unify_list_chars("hello world");
  if ( A1.string() == "term" )
    return A2.unify_term(PlCompound("hello(world)"));

  throw PlDomainError("type", A1);
}


PREDICATE(can_unify, 2)
{ PlFrame fr;

  bool rval = A1.unify_term(A2);
  fr.rewind();
  return rval;
}

PREDICATE(eq1, 2)
{ A1.unify_term_check(A2);
  return true;
}

PREDICATE(eq2, 2)
{ PlCheck(A1.unify_term(A2));
  return true;
}

PREDICATE(eq3, 2)
{ return A1.unify_term(A2);
}

PREDICATE(eq4, 2)
{ PlCheck(PL_unify(A1.C_, A2.C_));
  return true;
}

PREDICATE(write_list, 1)
{ PlTerm_tail tail(A1);
  PlTerm_var e;

  while(tail.next(e))
    cout << e.string() << endl;

  return true;
}

PREDICATE(cappend, 3)
{ PlTerm_tail l1(A1);
  PlTerm_tail l3(A3);
  PlTerm_var e;

  while(l1.next(e))
  { if ( !l3.append(e) )
      return false;
  }

  return A2.unify_term(l3);
}

PREDICATE(call_atom, 1)
{ try
  { return PlCall(A1.wc_str()); // TODO: PlCall(A1.wstring())
  } catch ( PlTypeError &ex )
  { cerr << "Type Error caugth in C++" << endl;
    cerr << "Message: \"" << ex.string() << "\"" << endl;
    return false;
  }
}


/* The purpose of this predicate is mostly to show that
   resource errors are dealt with appropriately: with large
   enough argument, this will overflow the stacks.  The Prolog
   error is mapped to a C++ exception and back again when
   control is passed back to Prolog.  So this is just fine:

   ?- square_roots(1000000000, L)
   ERROR: Out of global stack
*/

PREDICATE(square_roots, 2)
{ int end = A1.as_int();
  PlTerm_tail list(A2);

  for(int i=0; i<end; i++)
    list.append(PlTerm_float(sqrt(double(i))));

  return list.close();
}

/* Create a dependency on malloc().  If the main system uses
 * tcmalloc (default when available), the shared object should
 * __not__ be linked against tcmalloc.  This code crashes when
 * compiled using
 *
 *     swipl-ld -o test -ltcmalloc -shared test.cpp
 */

PREDICATE(malloc, 2)
{ void *ptr = malloc(A1.as_size_t());
  return A2.unify_pointer(ptr);
}

PREDICATE(free, 1)
{ void *ptr = A1.pointer();
  free(ptr);
  return true;
}

PREDICATE(new_chars, 2)
{ char *ptr = new char[A1.as_size_t()];
  return A2.unify_pointer(ptr);
}

PREDICATE(delete_chars, 1)
{ char *ptr = static_cast<char *>(A1.pointer());
  delete[] ptr;
  return true;
}

// TODO: the following code for detecting address-sanitizer
//       is taken from pl-incl.h. See discussion about this
//       in https://github.com/SWI-Prolog/swipl-devel/pull/1044

/* Clang way to detect address_sanitizer */
#ifndef __has_feature
  #define __has_feature(x) 0
#endif
#ifndef __SANITIZE_ADDRESS__
#if __has_feature(address_sanitizer)
#define __SANITIZE_ADDRESS__
#endif
#endif


PREDICATE(address_sanitizer, 0)
{
  #if defined(__SANITIZE_ADDRESS__)
  return true;
  #else
  return false;
  #endif
}

PREDICATE(address_sanitizer, 1)
{
  #if defined(__SANITIZE_ADDRESS__)
  return A1.unify_bool(1);
  #else
  return A1.unify_bool(0);
  #endif
}

class MyClass
{
public:
  const char* contents;
  MyClass() : contents("foo-bar") { }
};

PREDICATE(make_my_object, 1)
{ auto myobj = new MyClass();

  return A1.unify_pointer(myobj);
}

PREDICATE(my_object_contents, 2)
{ auto myobj = static_cast<MyClass*>(A1.pointer());
  return A2.unify_string(myobj->contents);
}

PREDICATE(free_my_object, 1)
{ auto myobj = static_cast<MyClass*>(A1.pointer());

  delete myobj;
  return true;
}

PREDICATE(make_functor, 3)  // make_functor(foo, x, foo(x))
{ auto f = PlFunctor(A1.atom().c_str(), 1);
  return A3.unify_functor(f) &&
    A3[1].unify_term(A2);
}

PREDICATE(make_uint64, 2)
{ A2.unify_integer_check(A1.as_uint64_t());
  return true;
}

PREDICATE(make_int64, 2)
{ int64_t i;
  // This function is for testing PlCheck()
  PlCheck(PL_get_int64_ex(A1.C_, &i));
  A2.unify_integer_check(i);
  return true;
}

PREDICATE(hostname, 1)
{ char buf[255+1]; // SUSv2; POSIX.1 has a smaller HOST_NAME_MAX+1

  if ( gethostname(buf, sizeof buf) == 0 )
    return A1.unify_atom(buf);

  return false;
}

PREDICATE(hostname2, 1)
{ char buf[255+1]; // SUSv2; POSIX.1 has a smaller HOST_NAME_MAX+1
  if ( gethostname(buf, sizeof buf) != 0 )
    throw PlFail();
  A1.unify_atom_check(buf);
  return true;
}


PREDICATE(ensure_PlTerm_forward_declarations_are_implemented, 0)
{ PlTerm_var p01;
  PlTerm_atom p02("abc");
  PlTerm_atom p03(L"ABC");
  PlTerm_integer p04(123);
  PlTerm_integer p05(666L);
  PlTerm_integer p06(0UL);
  PlTerm_integer p07(static_cast<size_t>(-1));
  PlTerm_float p08(1.23);
  PlTerm_atom p09(PlAtom("an atom"));
  PlTerm_pointer p10(&p01);
  PlTerm_atom p11(std::string("abc"));
  PlTerm_atom p12(std::wstring(L"世界"));
  PlTerm_string s01("abc");
  PlTerm_list_codes s02("xyz");
  PlTerm_list_chars s03("mno");
  const char *   x01 = p01.c_str();
  const wchar_t *x02 = p01.wc_str();
  const std::string x01a = p01.string();
  const std::wstring x01b = p01.wstring();
  long           x04 = p03.as_long();
  int            x05 = p04.as_int();
  uint32_t       x06 = p01.as_uint32_t();
  uint64_t       x07 = p01.as_uint64_t();
  int64_t        x08 = p01.as_int64_t();
  size_t         x09 = p01.as_size_t();
  bool           x10 = p01.as_bool();
  double         x11 = p01.as_float();
  double         x12 = p01.as_double();
  PlAtom         x13 = p01.atom();
  void *         x14 = p01.pointer();
  PlTerm         x20 = p01[1];
  size_t         x21 = p01.arity();
  const char *   x22 = p01.name();

  (void)x01;
  (void)x02;
  // TODO: std::string string() const;
  (void)x04;
  (void)x05;
  (void)x06;
  (void)x07;
  (void)x08;
  (void)x09;
  (void)x10;
  (void)x11;
  (void)x12;
  (void)x13;
  (void)x14;
  (void)x20;
  (void)x21;
  (void)x22;

  (void)p01.unify_term(p02);
  (void)p01.unify_atom(PlAtom("an atom"));
  (void)p02.unify_atom("abc");
  (void)p03.unify_atom(L"ABC");
  (void)p04.unify_integer(123);
  (void)p05.unify_integer(666);
  (void)p06.unify_integer(0);
  (void)p07.unify_integer(sizeof p01);
  (void)p08.unify_float(1.23);
  (void)p09.unify_functor(PlFunctor("f", 3));
  (void)p10.unify_pointer(&p01);

  p01.unify_term_check(p02);
  p01.unify_atom_check(PlAtom("an atom"));
  p01.unify_atom_check("chars");
  p01.unify_atom_check(L"CHARS");
  p01.unify_integer_check(1);
  p01.unify_integer_check(2);
  p01.unify_integer_check(3);
  p01.unify_integer_check(sizeof p01);
  p01.unify_float_check(3.14159);
  p01.unify_functor_check(PlFunctor("f", 3));
  p01.unify_pointer_check(&p01);
  s01.unify_atom_check(x01);
  s01.unify_term_check(s01);
  s02.unify_term_check(s01);
  s03.unify_term_check(s03);

  return true;
}

// The following are for verifying some documentation details.

PREDICATE(c_PL_unify_nil, 1)          { return PL_unify_nil(A1.C_); }

PREDICATE(cpp_unify_nil, 1)           { return A1.unify_nil(); }

PREDICATE(check_c_PL_unify_nil, 1)    { PlCheck(PL_unify_nil(A1.C_));    return true; }

PREDICATE(cpp_unify_nil_check, 1)     { A1.unify_nil_check();            return true; }

// Repeat the above 4, for *_ex():

PREDICATE(c_PL_unify_nil_ex, 1)       { return PL_unify_nil_ex(A1.C_); }

PREDICATE(cpp_unify_nil_ex, 1)        { return A1.unify_nil_ex(); }

PREDICATE(check_c_PL_unify_nil_ex, 1) { PlCheck(PL_unify_nil_ex(A1.C_)); return true; }

PREDICATE(cpp_unify_nil_ex_check, 1)  { A1.unify_nil_ex_check();         return true; }



PREDICATE(c_PL_get_nil, 1)            { return PL_get_nil(A1.C_); }

PREDICATE(cpp_as_nil, 1)              { A1.as_nil();                     return true; }

PREDICATE(check_c_PL_get_nil, 1)      { PlCheck(PL_get_nil(A1.C_));      return true; }

PREDICATE(check_c_PL_get_nil_ex, 1)   { PlCheck(PL_get_nil_ex(A1.C_));   return true; }

// Functions re-implemented from ffi4pl.c

// range_cpp/3 is equivalent to range_ffialloc/3

/* range_cpp/3 is used in regression tests
   - PL_foreign_context_address() and malloc()-ed context.
*/
struct RangeContext
{ long i;
  long high;
  explicit RangeContext(long i, long high)
    : i(i), high(high) { }
};

PREDICATE_NONDET(range_cpp, 3)
{ auto t_low = A1, t_high = A2, t_result = A3;
  PlForeignContextPtr<RangeContext> ctxt(handle);

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
      ctxt.set(new RangeContext(t_low.as_long(),
                                t_high.as_long()));
      break;
    case PL_REDO:
      break;
    case PL_PRUNED:
      return true;
    default:
      assert(0);
      return false;
  }

  if ( ctxt->i >= ctxt->high ||
       !t_result.unify_integer(ctxt->i) )
    return false;

  ctxt->i += 1;
  if ( ctxt->i == ctxt->high )
    return true; // Last result: succeed without a choice point

  ctxt.keep();
  PL_retry_address(ctxt.get()); // Succeed with a choice point
}

