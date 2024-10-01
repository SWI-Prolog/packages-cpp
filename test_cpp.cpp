/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2024, SWI-Prolog Solutions b.v.
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

    swipl-ld -shared -o test_cpp test_cpp.cpp

and subsequently loading using

    swipl
    ?- use_foreign_library(test_cpp).

Next, run example predicates such as below.  Scan through this file
to find the predicates provided by this C++ code.

    ?- hello(world).
    Hello world

This code is also used by test_cpp.pl, which has many examples of
how the various predicates can be called from Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define _CRT_SECURE_NO_WARNINGS 1
#define PROLOG_MODULE "user"
#include <iostream>
#include <sstream>
#include <memory>
#include "SWI-cpp2.h"
#include "SWI-cpp2-atommap.h"
#include "SWI-cpp2-flags.h"
#include <errno.h>
#include <math.h>
#include <cassert>
#include <cstdio> // for MyFileBlob
#include <limits>
#include <string>
#include <map>
#include <vector>
using namespace std;

#ifdef _MSC_VER
#undef min
#undef max
#endif

#ifdef O_DEBUG
#define DEBUG(g) g
#else
#define DEBUG(g) (void)0
#endif


PREDICATE(unwrap, 1)
{ // test that the definition of C_ as a macro hasn't broken anything
  PlTerm A1_copy(A1);
  PlTerm *A1_ptr = &A1_copy;

  return A1.C_ == A1.unwrap() &&
    A1.C_ == *PlUnwrapAsPtr(A1_ptr) &&
    A1.unwrap() == *PlUnwrapAsPtr(A1_ptr);
}


PREDICATE(hello, 0)
{ PlQuery q("write", PlTermv(PlTerm_atom("hello hello hello")));
  PlCheckFail(q.next_solution());
  return true;
}

PREDICATE(hello, 2)
{ std::stringstream buffer;
  // This will result in an encoding error if A1 isn't Latin-1
  buffer << "Hello " << A1.as_string() << endl;
  buffer << "Hello " << A1.as_string().c_str() << endl; // Same output as previous line
  buffer << "Hello " << A1.as_string(PlEncoding::Latin1) << endl; // Also same, if it's ASCII
  buffer << "Hello " << A1.as_string(PlEncoding::UTF8) << endl;
  buffer << "Hello " << A1.as_string(PlEncoding::Locale) << endl; // Can vary by locale settings

  return A2.unify_string(buffer.str());
}

PREDICATE(hello2, 2)
{ PlAtom atom_a1(A1.as_atom());
  std::stringstream buffer;
  // The following have the same output as hello/1, if A1 is an atom
  buffer << "Hello2 " << atom_a1.as_string() << endl;
  buffer << "Hello2 " << A1.as_string().c_str() << endl;
  buffer << "Hello2 " << A1.as_string(PlEncoding::Latin1) << endl;
  buffer << "Hello2 " << A1.as_string(PlEncoding::UTF8) << endl;
  buffer << "Hello2 " << A1.as_string(PlEncoding::Locale) << endl;

  return A2.unify_string(buffer.str());
}

PREDICATE(hello3, 2)
{ PlAtom atom_a1(A1.as_atom());
  char buf[1024];

  // Iostream doesn't work because `<<` doesn't support std::wstring:
  //   cout << "Hello3 " << atom_a1.wstring() << endl; /* Same output as hello/1 */

  // If %s is used, an error will occur if A1 has a non-ascii
  // character in it. In addition, a NUL ('\0') in the atom will cause
  // the rest of the atom to not be printed.

  int len = Ssnprintf(buf, sizeof buf,
		      "Hello3 %Ws\n", atom_a1.as_wstring().c_str());
  if ( len >= 0 )
    // TODO: use len when fixed: https://github.com/SWI-Prolog/swipl-devel/issues/1074
    return A2.unify_chars(PL_STRING|REP_UTF8, strlen(buf), buf);
  return false;
}

PREDICATE(hello4, 1)
{ // The following code is the same as
  //   A1.unify_term(PlCompound("hello", PlTermv(PlAtom("world"))));
  // but is in separate statements to make tracig constructors easier.
  auto hello_world = A1;
  auto world_atom = PlAtom("world");
  auto world_termv = PlTermv(world_atom);
  auto hello_world_compound = PlCompound("hello", world_termv);
  return hello_world.unify_term(hello_world_compound);
}

// TODO: add tests
PREDICATE(as_string, 2)
{ return A2.unify_string(A1.as_string());
}

// TODO: add tests
PREDICATE(as_wstring, 2)
{ return A2.unify_wstring(A1.as_wstring());
}

PREDICATE(add, 3)
{ // as_long() converts integral floats to integers
  return A3.unify_integer(A1.as_long() + A2.as_long());
}

PREDICATE(add_num, 3)
{ auto x = A1, y = A2, result = A3;
  // Note that as_float() handles integers
  double sum = x.as_float() + y.as_float();
  return ( double(long(sum)) == sum ) // Can float be represented as int?
    ? result.unify_integer(long(sum))
    : result.unify_float(sum);
}

PREDICATE(name_arity, 1)
{ PlStream strm(Scurrent_output);
  strm.printf("name = %s, arity = %zd\n", A1.name().as_string().c_str(), A1.arity());
  return true;
}

PREDICATE(name_arity, 2)
{ std::stringstream buffer;

  buffer << "name = " << A1.name().as_string() << ", arity = " << A1.arity() << endl;
  return A2.unify_string(buffer.str());
}

PREDICATE(name_arity, 3)		/* name_arity(+Term, -Name, -Arity) */
{ PlTerm term(A1);
  PlTerm name(A2);
  PlTerm arity(A3);

  PlCheckFail(name.unify_atom(term.name()));
  PlCheckFail(arity.unify_integer(term.arity()));

  return true;
}

PREDICATE(name_arity_bool, 3)  // like name_arity/3 but doesn't throw
{ PlTerm term(A1), name(A2), arity(A3);
  PlAtom name_a(PlAtom::null);
  size_t arity_a;
  if ( !term.name_arity(&name_a, &arity_a) )
    return false;
  assert(name_a.not_null());

  return name.unify_atom(name_a) && arity.unify_integer(arity_a);
}

PREDICATE(list_modules, 1)
{ std::stringstream buffer;
  PlTermv av(1);

  PlQuery q("current_module", av);
  while( q.next_solution() )
    buffer << av[0].as_string() << endl;

  q.cut();
  return A1.unify_string(buffer.str());
}

// %! average(+Templ, :Goal, -Average) is det.
// % Same as: aggregate(sum(X)/count, Goal, A), Average is A.
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
  q.cut();
  return A3.unify_float(double(sum) / double(n));
}

PREDICATE(call_cpp, 2)
{ PlQuery q(A1.as_string(), PlTermv(A2));
  PlCheckFail(q.next_solution());
  // There's no need for calling q.cut() - it's done implicitly by the
  // query's destructor.
  return true;
}

PREDICATE(call_cut, 1)
{ PlQuery q(A1.as_string(), PlTermv());
  PlCheckFail(q.next_solution());
  q.cut(); // This tests that ~PlQuery() behaves correctly if cut() had been called
  return true;
}

// TODO: add tests for PlQuery() with PL_Q_EXT_STATUS

PREDICATE(call_cpp, 1)
{ PlCheckFail(PlCall(A1));
  return true;
}

PREDICATE(call_cpp_obj, 1)
{ return A1.call();
}

PREDICATE(call_cpp_ex, 2)
{ try
  { PlCheckFail(PlCall(A1, PL_Q_CATCH_EXCEPTION));
  } catch ( PlException& ex )
  { bool rc = A2.unify_term(ex.term());
    Plx_clear_exception();
    return rc;
  }
  return A2.unify_string("no exception");
}

PREDICATE(atom_to_string, 2)
{ PlAtom a(A1.as_atom());
  PlCheckFail(A2.unify_string(a.as_string(PlEncoding::UTF8)));
  return true;
}

PREDICATE(term_to_string, 2)
{ PlCheckFail(A2.unify_string(A1.as_string(PlEncoding::UTF8)));
  return true;
}

PREDICATE(term, 1)
{ return A1.unify_term(PlCompound("hello", PlTermv(PlAtom("world"))));
}

PREDICATE(term, 2)
{ static PlAtom ATOM_atom("atom");
  PlAtom a(A1.as_atom());

  if ( a.unwrap() == ATOM_atom.unwrap() )
    return A2.unify_atom("hello world"); // or A2.unify_term(PlAtom("hello world"));
  if ( A1.as_string() == "string" )
    return A2.unify_string("hello world");
  if ( A1.as_string() == "code_list" )
    return A2.unify_list_codes("hello world"); // TODO: deprecated
  if ( A1.as_string() == "char_list" )
    return A2.unify_list_chars("hello world"); // TODO: deprecated
  if ( A1.as_string() == "term" )
    return A2.unify_term(PlCompound("hello(world)"));

  throw PlDomainError("type", A1);
}

PREDICATE(call_chars_discard, 1)
{ PlFrame fr;
  PlCompound goal(A1.as_string());
  bool rval = goal.call();
  fr.discard();
  return rval;
}

PREDICATE(call_chars, 1)
{ A1.must_be_atom_or_string();
  PlCompound goal(A1.as_string());
  return goal.call();
}

/* can_unify(A1, A2) :- unifiable(A1, A2, _) */
PREDICATE(can_unify, 2)
{ PlFrame fr;

  bool rval = A1.unify_term(A2);
  fr.discard(); // or: PL.rewind()
  return rval;
}

PREDICATE(can_unify_ffi, 2)
{ fid_t fid = PL_open_foreign_frame();

  int rval = PL_unify(A1.unwrap(), A2.unwrap());
  PL_discard_foreign_frame(fid);
  return rval;
}

/* if_then(A1,A2,A3,A4) :- A1 = A2 -> once(A3) ; once(A4) */
PREDICATE(if_then_a, 4)
{ PlFrame fr;
  bool t1_t2_unified = A1.unify_term(A2);
  if ( ! t1_t2_unified )
    fr.rewind();
  return PlCall(t1_t2_unified ? A3 : A4);
}

/* if_then(A1,A2,A3,A4) :- A1 = A2 -> once(A3) ; once(A4) */
PREDICATE(if_then_b, 4)
{ return PlCall(PlRewindOnFail([t1=A1,t2=A2]()->bool
                { return t1.unify_term(t2); }) ? A3 : A4);
}


PREDICATE(if_then_ffi, 4)
{ fid_t fid = PL_open_foreign_frame();
  term_t t1 = A1.unwrap(), t2 = A2.unwrap();
  int t1_t2_unified = PL_unify(t1, t2);
  if ( !t1_t2_unified )
  { if ( PL_exception(0) )
      { PL_close_foreign_frame(fid);
        return FALSE;
      }
    PL_rewind_foreign_frame(fid);
  }
  int rc;
  rc = PL_call((t1_t2_unified ? A3 : A4).unwrap(), (module_t)0);

  PL_close_foreign_frame(fid);
  return rc;
}

PREDICATE(eq1, 2)
{ PlCheckFail(A1.unify_term(A2));
  return true;
}

PREDICATE(eq2, 2)
{ return A1.unify_term(A2);
}

PREDICATE(eq3, 2)
{ return Plx_unify(A1.unwrap(), A2.unwrap());
}

PREDICATE(eq4, 2)
{ // This is what Plx_unify() expands to
  return PlWrap<bool>(PL_unify(A1.unwrap(), A2.unwrap()));
}

PREDICATE(write_list, 1)
{ PlStream strm(Scurrent_output);

  // This is an example of using the try...PREDICATE_CATCH for avoiding
  // throwing an exception in the PlStream destructor.
  PlTerm_tail tail(A1);
  PlTerm_var e;
  try
  { while( tail.next(e) )
      strm.printf("%s\n", e.as_string().c_str());
  } PREDICATE_CATCH({strm.release(); return false;})
  return tail.close();  // or: PlCheckFail(tail.unify_nil());
}

PREDICATE(cappend, 3)
{ PlTerm_tail l1(A1);
  PlTerm_tail l3(A3);
  PlTerm_var e;

  while( l1.next(e) )
    PlCheckFail(l3.append(e));

  return A2.unify_term(l3);
}

static const PlOptionsFlag<int>
open_query_options("open-query flag",
                   { // {"debug",         PL_Q_DEBUG},
                     // {"deterministic", PL_Q_DETERMINISTIC },
                     {"normal",          PL_Q_NORMAL},
                     {"nodebug",         PL_Q_NODEBUG},
                     {"catch_exception", PL_Q_CATCH_EXCEPTION},
                     {"pass_exception",  PL_Q_PASS_EXCEPTION},
                     {"allow_exception", PL_Q_ALLOW_YIELD},
                     {"ext_status",      PL_Q_EXT_STATUS} });


// TODO: This doesn't do quite what's expected if there's an
//       exception.  Instead of returning the exception to Prolog, it
//       ends up in the debugger.
//       Possibly this is because PlCall needs the flags
//       PL_Q_CATCH_EXCEPTION and not PL_Q_PASS_EXCEPTION?
PREDICATE(cpp_call_, 3)
{ int flags = A2.as_int();
  int verbose = A3.as_bool();
  std::string flag_str = open_query_options.as_string(flags);
  PlStream strm(Scurrent_output);
  if ( flag_str.empty() )
    flag_str = "cpp_call";
  else
    flag_str = "cpp_call(" + flag_str + ")";
  if ( verbose )
    strm.printf("%s: %s\n",  flag_str.c_str(), A1.as_string().c_str());

  try
  { int rc = PlCall(A1, flags);
    if ( flags & PL_Q_EXT_STATUS )
    { const char *status_str;
      switch ( rc )
      { case PL_S_EXCEPTION: status_str = "exception"; break;
	case PL_S_FALSE:     status_str = "false";     break;
	case PL_S_TRUE:      status_str = "true";      break;
	case PL_S_LAST:      status_str = "last";      break;
	case PL_S_YIELD:     status_str = "yield";     break;
	default:             status_str = "???";       break;
      }
      if ( verbose )
	strm.printf("... after call, rc=%d: %s\n", rc, status_str);
    } else
    { if ( verbose )
	strm.printf("... after call, rc=%d\n", rc);
    }

    if ( rc )
    { if ( verbose )
	strm.printf("cpp_call result: rc=%d: %s\n", rc, A1.as_string().c_str());
    } else
    { PlTerm ex(Plx_exception(0));
      if ( ex.is_null() )
      { if ( verbose )
	  strm.printf("cpp_call failed\n");
      } else
      { if ( verbose )
	  strm.printf("cpp_call failed: ex: %s\n", ex.as_string().c_str());
      }
    }
    return rc; // TODO: this is wrong with some query flags
  } catch ( PlException& ex )
  { if ( ex.is_null() )
    { if ( verbose )
	strm.printf("cpp_call except is_null\n");
    } else
    { if ( verbose )
	strm.printf("cpp_call exception: %s\n", ex.as_string().c_str());
    }
    throw;
  }
}

PREDICATE(cpp_atom_codes, 2)
{ int rc = PlCall("atom_codes", PlTermv(A1, A2));
  if ( !rc )
  { PlException ex(PlTerm(Plx_exception(0)));
    PlStream strm(Scurrent_output);
    if ( ex.is_null() )
      strm.printf("atom_codes failed\n");
    else
      strm.printf("atom_codes failed: ex: %s\n", ex.as_string().c_str()); // Shouldn't happen
  }
  return rc;
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

  for(int i=0; i<=end; i++)
    PlCheckFail(list.append(PlTerm_float(sqrt(double(i)))));

  return list.close();
}

PREDICATE(malloc_malloc, 2)
{ char *ptr = static_cast<char*>(malloc(A1.as_size_t()));
  return A2.unify_pointer(ptr);
}

PREDICATE(free_malloc, 1)
{ char *ptr = static_cast<char*>(A1.as_pointer());
  free(ptr);
  return true;
}

PREDICATE(malloc_PL_malloc, 2)
{ char *ptr = static_cast<char*>(Plx_malloc(A1.as_size_t()));
  return A2.unify_pointer(ptr);
}

PREDICATE(free_PL_malloc, 1)
{ char *ptr = static_cast<char*>(A1.as_pointer());
  Plx_free(ptr);
  return true;
}

PREDICATE(malloc_new, 2)
{ char *ptr = new char[A1.as_size_t()];
  return A2.unify_pointer(ptr);
}

PREDICATE(free_delete, 1)
{ char *ptr = static_cast<char*>(A1.as_pointer());
  delete[] ptr;
  return true;
}

PREDICATE(new_chars, 2)
{ char *ptr = new char[A1.as_size_t()];
  return A2.unify_pointer(ptr);
}

PREDICATE(delete_chars, 1)
{ char *ptr = static_cast<char *>(A1.as_pointer());
  delete[] ptr;
  return true;
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
{ auto myobj = static_cast<MyClass*>(A1.as_pointer());
  return A2.unify_string(myobj->contents);
}

PREDICATE(free_my_object, 1)
{ auto myobj = static_cast<MyClass*>(A1.as_pointer());
  delete myobj;
  return true;
}


PREDICATE(make_functor, 3)  // make_functor(foo, x, foo(x))
{ auto f = PlFunctor(A1.as_atom().as_string(), 1);
  return A3.unify_functor(f) &&
    A3[1].unify_term(A2);
}

PREDICATE(cpp_arg, 3) // like arg/3 but Arg must be instantiated
{ auto i = A1.as_uint64_t();
  return A2[i].unify_term(A3);
}

PREDICATE(make_uint64, 2)
{ PlCheckFail(A2.unify_integer(A1.as_uint64_t()));
  return true;
}

PREDICATE(make_int64, 2)
{ int64_t i;
  // This tests PlEx<bool>
  A1.get_int64_ex(&i);
  PlCheckFail(A2.unify_integer(i));
  return true;
}

/* The manual example uses gethostname(), but portability thereof is not
   trivial and we should not introduce portability issues on tests that
   are not about portability.
*/

static int
no_gethostname(char *buf, size_t len)
{ static const char hostname[] = "my_awesome_hostname";
  if ( len <= 0 )
  { errno = ENAMETOOLONG;
    return -1;
  }

  strncpy(buf, hostname, len);
  if ( buf[len-1] )
  { errno = ENAMETOOLONG;
    return -1;
  }

  return 0;
}

PREDICATE(hostname, 1)
{ char buf[255+1]; // SYSv2; POSIX.1 has a smaller HOST_NAME_MAX+1

  if ( no_gethostname(buf, sizeof buf) == 0 )
    return A1.unify_atom(buf);

  return false;
}

PREDICATE(hostname2, 1)
{ char buf[255+1]; // SYSv2; POSIX.1 has a smaller HOST_NAME_MAX+1
  if ( no_gethostname(buf, sizeof buf) != 0 )
    throw PlFail();
  PlCheckFail(A1.unify_atom(buf));
  return true;
}

// The eq_int64/2 and lt_int64/2 predicates test the deprecated PlTerm::operator==()

#ifdef _MSC_VER
#pragma warning( push )
#pragma warning (disable:4996)
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

PREDICATE(eq_int64, 2)
{ return A1 == A2.as_int64_t();
}

PREDICATE(lt_int64, 2)
{ return A1 < A2.as_int64_t();
}

#ifdef _MSC_VER
#pragma warning( pop )
#else
#pragma GCC diagnostic pop
#endif

PREDICATE(get_atom_ex, 2)
{ PlAtom a(PlAtom::null);
  A1.get_atom_ex(&a);
  return A2.unify_atom(a);
}


PREDICATE(ensure_PlTerm_forward_declarations_are_implemented, 0)
{ /*********************************************************************
   * This code is not intended to be executed; it is only compiled, to *
   * check that implementations exist where expected.                  *
   *********************************************************************/
  PlTerm_var t_var;
  PlTerm_atom t_atom1("abc");
  PlTerm_atom t_atom2(L"ABC");
  PlTerm_atom t_atom3(PlAtom("an atom"));
  PlTerm_atom p_atom4(std::string("abc"));
  PlTerm_atom p_atom5(std::wstring(L"世界"));
  PlTerm_term_t t_t(Plx_new_term_ref());
  PlTerm_term_t t_null(PlTerm::null);
  PlTerm t_t2(Plx_new_term_ref());
  PlTerm t_null2(PlTerm::null);
  // The various integer types are also used in IntInfo.
  PlTerm_integer t_int1(std::numeric_limits<int>::max());
  PlTerm_integer t_int1b(std::numeric_limits<int>::min());
  PlTerm_integer t_int2(std::numeric_limits<long>::max());
  PlTerm_integer t_int2b(std::numeric_limits<long>::min());
  PlTerm_integer t_int64(std::numeric_limits<int64_t>::max());
  PlTerm_integer t_int64b(std::numeric_limits<int64_t>::min());
  PlTerm_integer t_uint64(std::numeric_limits<uint64_t>::max());
  PlTerm_integer t_uint64b(std::numeric_limits<uint64_t>::min());
  PlTerm_integer p_size(static_cast<size_t>(-1));
  PlTerm_integer p_size2(std::numeric_limits<size_t>::max());
  PlTerm_float t_float(1.23);
  PlTerm_pointer t_ptr(&t_var);

  // There's a better test for PlRecord in int_info/2
  PlRecord r_atom1(t_atom1.record());
  PlCheckFail(t_atom1.unify_term(r_atom1.term()));

  std::shared_ptr<PlRecord> r_atom2_p(new PlRecord(t_atom2.record()), PlRecordDeleter());
  PlCheckFail(t_atom2.unify_term(r_atom2_p->term()));

  PlTerm t_rec(r_atom2_p->term());
  PlCheckFail(t_rec.unify_term(t_atom2));

  PlTerm_string t_string1("abc");
  PlTerm_string t_string2(L"世界");
  const char codes[] = {81,82,83,0};
  PlTerm_list_codes s02(codes);
  PlTerm_list_chars s03("mno");

  PlTerm_var tt;
  tt.put_variable();
  tt.put_atom(PlAtom("xyz"));
  tt.put_bool(false);
  tt.put_atom_chars("abcdefg");
  tt.put_string_chars("gfedcba");
  tt.put_chars(0, 3, "abc");
  tt.put_list_chars("mnopq");
  tt.put_list_codes("1234");
  tt.put_atom_nchars(3, "111");
  tt.put_string_nchars(3, "222");
  tt.put_list_nchars(3, "333");
  tt.put_list_ncodes(3, "444");
  tt.put_integer(-1234);
  tt.put_pointer(&tt);
  tt.put_float(0.123);
  tt.put_functor(PlFunctor("foo", 1));
  tt.put_list();
  tt.put_nil();
  tt.put_term(t_string1);

  PlAtom atom1("atom1");
  PlAtom atom2(L"原子2");
  PlAtom atom3(std::string("atom3"));
  PlAtom atom4(std::wstring(L"原子4"));
  PlAtom a5a(t_atom1.as_atom());
  PlAtom atom_null(PlAtom::null);
  // The following are unsafe (the as_string() is deleted in the statement):
  //   const char *   x01  = t_var.as_string().c_str();
  //   const wchar_t *x01a = t_var.as_wstring().c_str();
  const std::string  s01 = atom3.as_string();
  const std::wstring s01b = atom4.as_wstring();
  const std::string  s02a = t_var.as_string();
  const std::wstring s02b = t_var.as_wstring();
  atom1.register_ref();
  atom1.unregister_ref();
  { int v1;
    unsigned v2;
    long v3;
    unsigned long v4;
    size_t v5;
    t_int1.integer(&v1);
    t_int1.integer(&v2);
    t_int1.integer(&v3);
    t_int1.integer(&v4);
    t_int1.integer(&v5);
  }
  long           x04 = t_atom2.as_long();
  int            x05 = t_int1.as_int();
  uint32_t       x06 = t_var.as_uint32_t();
  uint64_t       x07 = t_var.as_uint64_t();
  int64_t        x08 = t_var.as_int64_t();
  size_t         x09 = t_var.as_size_t();
  bool           x10 = t_var.as_bool();
  double         x11 = t_var.as_float();
  double         x12 = t_var.as_double();
  PlAtom         x13 = t_var.as_atom();
  void *         x14 = t_var.as_pointer();
  PlTerm         x20 = t_var[1];
  size_t         x21 = t_var.arity();
  PlAtom         x22 = t_var.name();

  //(void)x01;
  //(void)x01a;
  (void)a5a;
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

  (void)t_var.unify_term(t_atom1);
  (void)t_var.unify_atom(PlAtom("an atom"));
  (void)t_atom1.unify_atom("abc");
  (void)t_atom2.unify_atom(L"ABC");
  (void)t_atom3.unify_functor(PlFunctor("f", 3));
  (void)t_int1.unify_integer(123);
  (void)t_int2.unify_integer(666);
  (void)t_int2b.unify_integer(0);
  (void)p_size.unify_integer(sizeof t_var);
  (void)t_float.unify_float(1.23);
  (void)t_ptr.unify_pointer(&t_var);

  bool               xx01;
  char               xx02;
  signed char        xx03;
  unsigned char      xx04;
  // TODO:
  // wchar_t            xx05;
  // char16_t           xx06;
  // char32_t           xx07;
  short              xx08;
  unsigned short     xx09;
  int                xx10;
  unsigned int       xx11;
  long               xx12;
  unsigned long      xx13;
  long long          xx14;
  unsigned long long xx15;
  size_t             xx16;
  int32_t            xx17;
  uint32_t           xx18;
  uint64_t           xx19;
  int64_t            xx20;
  intptr_t           xx21;
  uintptr_t          xx22;

  t_int1.integer(&xx01);
  t_int1.integer(&xx02);
  t_int1.integer(&xx03);
  t_int1.integer(&xx04);
  // TODO:
  // t_int1.integer(&xx05);
  // t_int1.integer(&xx06);
  // t_int1.integer(&xx07);
  t_int1.integer(&xx08);
  t_int1.integer(&xx09);
  t_int1.integer(&xx10);
  t_int1.integer(&xx11);
  t_int1.integer(&xx12);
  t_int1.integer(&xx13);
  t_int1.integer(&xx14);
  t_int1.integer(&xx15);
  t_int1.integer(&xx16);
  t_int1.integer(&xx17);
  t_int1.integer(&xx18);
  t_int1.integer(&xx19);
  t_int1.integer(&xx20);
  t_int1.integer(&xx21);
  t_int1.integer(&xx22);

  PlStream strm(x20, 0);
  strm.set_timeout(1);
  strm.unit_size();
  strm.canrepresent('a');
  strm.putcode('x');
  strm.getcode();
  strm.putw(13);
  strm.getw();
  char data[10];
  size_t bytes = strm.fwrite("abc", sizeof (char), 3);
  bytes = strm.fread(data, sizeof data[0], bytes);
  if ( strm.feof() ) return false;
  if ( strm.fpasteof() ) return false;
  strm.clearerr();
  // TODO: the rest of the methods
  strm.release();

  return true;
}

PREDICATE(unify_int_set, 1)
{ int           i_int           = 0;
  unsigned      i_unsigned      = 0;
  long          i_long          = 0;
  unsigned long i_unsigned_long = 0;
  size_t        i_size          = 0;
  int32_t       i_int32         = 0;
  uint32_t      i_uint32        = 0;
  int64_t       i_int64         = 0;
  uint64_t      i_uint64        = 0;

  PlCheckFail(A1.unify_integer(i_int));
  PlCheckFail(A1.unify_integer(i_unsigned));
  PlCheckFail(A1.unify_integer(i_long));
  PlCheckFail(A1.unify_integer(i_unsigned_long));
  PlCheckFail(A1.unify_integer(i_size));
  PlCheckFail(A1.unify_integer(i_int32));
  PlCheckFail(A1.unify_integer(i_uint32));
  PlCheckFail(A1.unify_integer(i_int64));
  PlCheckFail(A1.unify_integer(i_uint64));

  return true;
}

// The following are for verifying some documentation details.

PREDICATE(c_PL_unify_nil, 1)          { return static_cast<foreign_t>(PL_unify_nil(A1.unwrap())); }

PREDICATE(cpp_unify_nil, 1)           { return A1.unify_nil(); }

PREDICATE(check_c_PL_unify_nil, 1)    { PlEx<bool>(PL_unify_nil(A1.unwrap())); return true; }

// Repeat the above, for *_ex():

PREDICATE(c_PL_unify_nil_ex, 1)       { return static_cast<foreign_t>(PL_unify_nil_ex(A1.unwrap())); }

PREDICATE(cpp_unify_nil_ex, 1)        { A1.unify_nil_ex(); return true; }

PREDICATE(check_c_PL_unify_nil_ex, 1) { PlEx<bool>(PL_unify_nil_ex(A1.unwrap())); return true; }



PREDICATE(c_PL_get_nil, 1)            { return static_cast<foreign_t>(PL_get_nil(A1.unwrap())); }

PREDICATE(cpp_as_nil, 1)              { A1.as_nil();                     return true; }

PREDICATE(check_c_PL_get_nil, 1)      { PlEx<bool>(PL_get_nil(A1.unwrap()));      return true; }

PREDICATE(check_c_PL_get_nil_ex, 1)   { PlEx<bool>(PL_get_nil_ex(A1.unwrap()));   return true; }

// Functions re-implemented from ffi4pl.c

// range_cpp/3 is similar to range_ffialloc/3

/* range_cpp/3 is used in regression tests:
   - PL_foreign_context_address() and malloc()-ed context.
*/
struct RangeCtxt
{ long i;
  long high;
  explicit RangeCtxt(long i, long high)
    : i(i), high(high) { }
};

PREDICATE_NONDET(range_cpp, 3)
{ auto t_low = A1, t_high = A2, t_result = A3;
  auto ctxt = handle.context_unique_ptr<RangeCtxt>();

  switch( handle.foreign_control() )
  { case PL_FIRST_CALL:
      ctxt.reset(new RangeCtxt(t_low.as_long(), t_high.as_long()));
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
  if ( ctxt->i >= ctxt->high )
  { return true; // Last result: succeed without a choice point
  }

  PL_retry_address(ctxt.release()); // Succeed with a choice point
}


// For benchmarking `throw PlThrow()` vs `return false`
// Times are given for 1 million failures

// 0.085 sec for time((between(1,1000000,X), fail)).
// 0.16 sec for time((between(1,1000000,X), X=0)).
// 0.20 sec for time((between(1,1000000,X), unify_zero_0(X))).

// See also timings for individual unify_zero_* predicates, unify_foo_*


static foreign_t
unify_zero_0(term_t a1)
{ return static_cast<foreign_t>(Plx_unify_integer(a1, 0));
}

// unify_zero_0() is C code, not C++, but it's registered using
// PlRegister class (this currently only works for foreign predicates
// with a single argument).  If you wish to use the C-style
// install_test_cpp() style instead, you need to use extern "C" to
// ensure that names don't get mangled.

static PlRegister _x_unify_zero_4_1(nullptr, "unify_zero_0", unify_zero_0);

// 0.23 sec for time((between(1,1000000,X), unify_zero_1(X))).
PREDICATE(unify_zero_1, 1)
{ if ( !Plx_unify_integer(A1.unwrap(), 0) )
    return false;
  return true;
}

// 3.3 sec for time((between(1,1000000,X), unify_zero_2(X))).
PREDICATE(unify_zero_2, 1)
{ if ( !Plx_unify_integer(A1.unwrap(), 0) )
    throw PlFail();
  return true;
}

// 4.0 sec for time((between(1,1000000,X), unify_zero_3(X))).
PREDICATE(unify_zero_3, 1)
{ PlCheckFail( Plx_unify_integer(A1.unwrap(), 0) );
  return true;
}

// 4.0 sec for time((between(1,1000000,X), unify_zero_4(X))).
PREDICATE(unify_zero_4, 1)
{ PlCheckFail(A1.unify_integer(0));
  return true;
}

// 0.23 sec for time((between(1,1000000,X), unify_zero_5(X))).
PREDICATE(unify_zero_5, 1)
{ return A1.unify_integer(0);
}

// Benchmarking the various kinds of string comparisons
// For PL_unify_chars:
//   Types:          PL_ATOM, PL_STRING, PL_CODE_LIST, PL_CHAR_LIST
//   Representation: REP_ISO_LATIN_1, REP_UTF8, REP_MB
//   Extra:          PL_DIFF_LIST
//   If len == static_cast<size_t>(-1), then zero-terminated
// example run: time((between(1,10000000,X), unify_foo_string_1("foobar"))).

// 1.2 sec
PREDICATE(unify_foo_atom_1, 1)
{ return A1.unify_chars(PL_ATOM|REP_ISO_LATIN_1, 3, "foo");
}

// 1.0 sec
PREDICATE(unify_foo_string_1, 1)
{ return A1.unify_chars(PL_STRING|REP_ISO_LATIN_1, 3, "foo");
}

// 0.92 sec
PREDICATE(unify_foo_atom_2a1, 1)
{ PlAtom foo("foo");
  return A1.unify_atom(foo);
}

// 0.92 sec
PREDICATE(unify_foo_atom_2a2, 1)
{ return A1.unify_atom(PlAtom("foo"));
}

// 0.98 sec
PREDICATE(unify_foo_atom_2b, 1)
{ PlAtom foo(std::string("foo"));
  return A1.unify_atom(foo);
}

// 1.0 sec
PREDICATE(unify_foo_string_2a, 1)
{ PlTerm_string foo("foo");
  return A1.unify_term(foo);
}

// 1.0 sec
PREDICATE(unify_foo_string_2b, 1)
{ PlTerm_string foo(std::string("foo"));
  return A1.unify_term(foo);
}

// end of benchmarking predicates

// Example pl_write_atoms from foreign.doc

PREDICATE(pl_write_atoms_cpp, 1)
{ auto l = A1;
  PlStream strm(Scurrent_output);
  PlTerm tail(l.copy_term_ref());
  PlTerm_var head;

  while( tail.get_list_ex(head, tail) )
  { strm.printf("%s\n", head.as_string().c_str());
  }

  tail.get_nil_ex();
  return true;
}

PREDICATE(pl_write_atoms_c, 1)
{ term_t l = A1.unwrap();
  term_t head = PL_new_term_ref();   /* the elements */
  term_t tail = PL_copy_term_ref(l); /* copy (we modify tail) */
  int rc = TRUE;
  auto strm = Plx_acquire_stream(Scurrent_output);

  while( rc && PL_get_list_ex(tail, head, tail) )
  { PL_STRINGS_MARK();
      char *s;
      if ( (rc=PL_get_chars(head, &s, CVT_ATOM|REP_MB|CVT_EXCEPTION)) )
        Sfprintf(strm, "%s\n", s);
    PL_STRINGS_RELEASE();
  }
  Plx_release_stream(strm);

  return rc && PL_get_nil_ex(tail); /* test end for [] */
}

static const std::map<const std::string, std::pair<PlRecord, PlRecord>> name_to_term =
  { {"one", {PlTerm_integer(1).record(), PlTerm_string("eins").record()}},
    {"two", {PlTerm_integer(2).record(), PlTerm_string("zwei").record()}}
  };

PREDICATE(name_to_terms, 3)
{ A1.must_be_atom_or_string();
  const auto it = name_to_term.find(A1.as_string());
  return it != name_to_term.cend() &&
    PlRewindOnFail([t1=A2,t2=A3,&it]()
                   { return t1.unify_term(it->second.first.term()) &&
                            t2.unify_term(it->second.second.term()); });
}

PREDICATE(name_to_terms2, 3)
{ PlTerm key(A1), t1(A2), t2(A3);
  key.must_be_atom_or_string();
  const auto it = name_to_term.find(key.as_string());
  if ( it == name_to_term.cend() )
    return false;
  if ( !t1.unify_term(it->second.first.term()) )
    return false;
  PlFrame fr;
  if ( !t2.unify_term(it->second.second.term()) )
  { fr.discard();
    return false;
  }
  return true;
}

// Predicates for checking native integer handling
// See https://en.cppreference.com/w/cpp/types/numeric_limits

#define DECLS_ROW(ty) X(#ty, ty, std::numeric_limits<ty>::min(), std::numeric_limits<ty>::max())

// TODO: char8_t (since C++20)
//       float, double, long double
//       - char16_t, char32_t, long long, unsigned long long (since C++11)

#define DECLS \
  DECLS_ROW(bool)               \
  DECLS_ROW(char)               \
  DECLS_ROW(signed char)        \
  DECLS_ROW(unsigned char)      \
  DECLS_ROW(wchar_t)            \
  DECLS_ROW(char16_t)           \
  DECLS_ROW(char32_t)           \
  DECLS_ROW(short)              \
  DECLS_ROW(unsigned short)     \
  DECLS_ROW(int)                \
  DECLS_ROW(unsigned int)       \
  DECLS_ROW(long)               \
  DECLS_ROW(unsigned long)      \
  DECLS_ROW(long long)          \
  DECLS_ROW(unsigned long long) \
  DECLS_ROW(size_t)             \
  DECLS_ROW(int16_t)            \
  DECLS_ROW(uint16_t)           \
  DECLS_ROW(int32_t)            \
  DECLS_ROW(uint32_t)           \
  DECLS_ROW(uint64_t)           \
  DECLS_ROW(int64_t)            \
  DECLS_ROW(intptr_t)           \
  DECLS_ROW(uintptr_t)

#define X(name, x_type, x_min, x_max)                    \
    {name,                                               \
     PlCompound("int_info",                              \
		PlTermv(PlTerm_atom(name),               \
			PlTerm_integer(sizeof (x_type)), \
			PlTerm_integer(x_min),           \
			PlTerm_integer(x_max))).record() },

typedef std::map<const std::string, PlRecord> IntInfo;

// IntInfoCtxt has a pointer to the static IntInfo to get around a
// memory leak. If int_info_static is at the top level of this file,
// its constructor happens before Prolog has set up the memory
// management for GMP (PlTerm_integer() with a suitably large value
// uses GMP), and therefore the GMP value isn't freed when Prolog
// terminates. However, if `int_info_static` is inside the
// constructor, there's no leak.

struct IntInfoCtxt
{ IntInfo *int_info;
  IntInfo::const_iterator it;
  explicit IntInfoCtxt()
  { static IntInfo int_info_static = { DECLS };
    int_info = &int_info_static;
    it = int_info->cbegin();
  }
};

#undef X

// int_info_(name, result, ctx) is called from int_info/2 to do a
// lookup of the name in ctx->int_info (see the IntInfoCtxt
// constructor for how this gets initialized). This finds a recorded
// term, from which a fresh term is concstructed using
// PlRecord::term(), and the unification is done in the context of
// PlRewindOnFail(). This ensures that if the unification fails, any
// partial bindings will be removed.

static bool
int_info_RewindOnFail(const std::string name, PlTerm result, IntInfoCtxt *ctxt)
{ const auto it = ctxt->int_info->find(name);
  if ( it == ctxt->int_info->cend() )
    return false;

  return PlRewindOnFail([&result,&it]()
                        { return result.unify_term(it->second.term()); });
}

static bool
int_info_noRewind(const std::string name, PlTerm result, IntInfoCtxt *ctxt)
{ const auto it = ctxt->int_info->find(name);
  return it != ctxt->int_info->cend() &&
    result.unify_term(it->second.term());
}

PREDICATE_NONDET(int_info, 2)
{ auto ctxt = handle.context_unique_ptr<IntInfoCtxt>();

  // When called with PL_PRUNED, A1 is not bound; therefore, we need
  // to do the switch on PL_foreign_control(handle) before checking
  // A1.is_variable(). That is, we can't use A1.is_variable() as a way
  // of checking whether we should do backtracking or not.

  switch( handle.foreign_control() )
  { case PL_FIRST_CALL:
      if ( !A1.is_variable() ) // int_info is a map, so unique on lookup
	return int_info_RewindOnFail(A1.as_string(), A2, ctxt.get());
      ctxt.reset(new IntInfoCtxt());
      break;
    case PL_REDO:
      break;
    case PL_PRUNED:
      return true;
    default:
      assert(0);
      return false;
  }
  assert(A1.is_variable());
  // Note that int_info_RewindOnFail() has its own frame to undo unification
  while ( ctxt->it != ctxt->int_info->cend() )
  { if ( int_info_RewindOnFail(ctxt->it->first, A2, ctxt.get()) )
    { if ( !A1.unify_atom(ctxt->it->first) )
        return false; // Shouldn't happen because A1 is a varaible
      ctxt->it++;
      if ( ctxt->it == ctxt->int_info->cend() )
      { return true; // Last result: no choice point
      }
      PL_retry_address(ctxt.release()); // Succeed with choice point
    }
    ctxt->it++;
  }
  return false;
}

// Same as int_info, but uses PlFrame instead of PlRewindOnFail()
PREDICATE_NONDET(int_info2, 2)
{ auto ctxt = handle.context_unique_ptr<IntInfoCtxt>();
  PlFrame fr;

  switch( handle.foreign_control() )
  { case PL_FIRST_CALL:
      if ( !A1.is_variable() ) // int_info is a map, so unique on lookup
	return int_info_noRewind(A1.as_string(), A2, ctxt.get());
      ctxt.reset(new IntInfoCtxt());
      [[fallthrough]];
  case PL_REDO:
    assert(A1.is_variable());
    while ( ctxt->it != ctxt->int_info->cend() )
    { if ( int_info_noRewind(ctxt->it->first, A2, ctxt.get()) )
      { if ( !A1.unify_atom(ctxt->it->first) )
          return false; // Shouldn't happen because A1 is a varaible
        ctxt->it++;
        if ( ctxt->it == ctxt->int_info->cend() )
        { return true; // Last result: no choice point
        }
        PL_retry_address(ctxt.release()); // Succeed with choice point
      }
      ctxt->it++;
      fr.rewind();
    }
    return false;
    break;
  case PL_PRUNED:
    return true;
  default:
    assert(0);
    return false;
  }
}

PREDICATE(type_error_string, 3)
{ PlException e(PlTypeError("foofoo", A1));
  // std::wstring msg(e.as_wstring()); // TODO: restore this
  std::string msg(e.as_string());
  PlCheckFail(A2.unify_string(msg));
  PlCheckFail(A3.unify_term(e.term()));
  return true;
}


// Re-implementing w_atom_ffi_/2:

PREDICATE(w_atom_cpp_, 2)
{ auto stream(A1), term(A2);
  PlStream strm(stream, SIO_OUTPUT);
  PlStringBuffers _string_buffers;
  size_t len; // Documentation sample code omits this
  const pl_wchar_t *sa = Plx_atom_wchars(term.as_atom().unwrap(), &len);
  strm.printfX("/%Ws/%zd", sa, len);
  return true;
}


/* TODO: Move the "cpp_options" predicate and the associated tests
	 to somewhere in main SWI-Prolog system. */

// cpp_options(+Options:list, +Opt_all:bool, -Result)
//   Result is: cpp_options(Quoted,Length,Callback,Token,Descr)
// Reimplementation of ffi_options_(), with an additional opt_all
// parameter
PREDICATE(cpp_options, 3)
{ auto options = A1, opt_all = A2, result = A3;
  int         quoted     = false;
  size_t      length     = 10;
  PlTerm_var  callback;
  PlAtom      token(PlAtom::null);
  const char *descr      = "";
  bool        opt_all_v  = opt_all.as_bool();
  int         flags      = opt_all_v ? OPT_ALL : 0;

  static PL_option_t scan_options[] =
  { PL_OPTION("quoted",   OPT_BOOL),
    PL_OPTION("length",   OPT_SIZE),
    PL_OPTION("callback", OPT_TERM),
    PL_OPTION("token",    OPT_ATOM),
    PL_OPTION("descr",    OPT_STRING),
    PL_OPTIONS_END
  };

  PlStringBuffers _string_buffers; // for descr's contents
  PlEx<bool>(PL_scan_options(options.unwrap(), flags, "cpp_options", scan_options,
                             &quoted, &length, callback.unwrap_as_ptr(), token.unwrap_as_ptr(), &descr));

  PlCheckFail(result.unify_term(
                  PlCompound("options",
                             PlTermv(PlTerm_integer(quoted),
                                     PlTerm_integer(length),
                                     callback,
                                     token.not_null() ? PlTerm(token) : PlTerm_var(),
                                     PlTerm_string(descr)))));
  // TODO: The following are needed if callback and token aren't used
  //       by a Prolog term (e.g., if they're stored in a "blob"):
  // callback.record();
  // token.register_ref();
  return true;
}

PREDICATE(cvt_i_bool, 2)
{ return A2.unify_integer(A1.as_bool());
}

// TODO: add tests for PL_cvt_i_*() (using PlTerm::integer())

// TODO: add PlEngine tests

PREDICATE(throw_domain_cpp0, 1)
{ return Plx_domain_error("footype", A1.unwrap());
}

PREDICATE(throw_domain_cpp1, 1)
{ throw PlDomainError("footype", A1);
}

PREDICATE(throw_domain_cpp2, 1)
{ PlEx<bool>(Plx_domain_error("footype", A1.unwrap()));
  return false; // Should never reach here
}

PREDICATE(throw_domain_cpp3, 1)
{ if ( !Plx_domain_error("footype", A1.unwrap()) )
    throw PlFail();
  // Shouldn't fall through to here
  Plx_clear_exception();
  return true; // Shouldn't happen
}

PREDICATE(throw_domain_cpp4, 1)
{ return PlDomainError("footype", A1).plThrow();
}

PREDICATE(throw_instantiation_error_cpp, 1)
{ throw PlInstantiationError(A1);
}

PREDICATE(throw_uninstantiation_error_cpp, 1)
{ throw PlUninstantiationError(A1);
}

PREDICATE(throw_representation_error_cpp, 1)
{ throw PlRepresentationError(A1.as_string());
}

PREDICATE(throw_type_error_cpp, 2)
{ throw PlTypeError(A1.as_string(), A2);
}

PREDICATE(throw_and_check_error_cpp, 2)
{ try
  { throw PlTypeError(A1.as_string(), A2);
  } catch (const PlException& e)
  { PlAtom ATOM_error("error");
    PlAtom ATOM_type_error("type_error");
    PlTerm e_t = e.term();
    // if A1 is 'atom', then e_t is error(type_error(atom,A2),_):
    PlCheckFail(e_t.name() == ATOM_error);
    PlCheckFail(e_t.arity() == 2);
    PlCheckFail(e_t[1].name() == ATOM_type_error);
    PlCheckFail(e_t[1][1].as_string() == A1.as_string());
    PlCheckFail(e_t[1][2] == A2);
    throw;
  }
  return true;
}

PREDICATE(throw_domain_error_cpp, 2)
{ throw PlDomainError(A1.as_string(), A2);
}

PREDICATE(throw_existence_error_cpp, 2)
{ throw PlExistenceError(A1.as_string(), A2);
}

PREDICATE(throw_permission_error_cpp, 3)
{ throw PlPermissionError(A1.as_string(), A2.as_string(), A3);
}

PREDICATE(throw_resource_error_cpp, 1)
{ throw PlResourceError(A1.as_string());
}

PREDICATE(ten, 10)
{ PlCheckFail(A1.unify_term(PlTerm_atom(PlAtom("one"))));
  PlCheckFail(A2.unify_atom(PlAtom("two")));
  PlCheckFail(A3.unify_atom("three"));
  PlCheckFail(A4.unify_integer(4));
  PlCheckFail(A5.unify_float(5.0));
  PlCheckFail(A6.unify_string("six"));
  PlCheckFail(A7.unify_functor(PlFunctor("seven", 1)));
  PlCheckFail(A7[1].unify_string("SEVEN"));
  PlCheckFail(A8.unify_nil());
  PlCheckFail(A9.unify_bool(true));
  PlTerm_var hd;
  PlTerm_var tl;
  PlCheckFail(A10.unify_list(hd, tl));
  PlCheckFail(hd.unify_atom("hd"));
  PlCheckFail(tl.unify_nil());
  return true;
}


struct MyConnection
{ std::string name;

  explicit MyConnection() { }
  explicit MyConnection(const std::string& _name)
    : name(_name)
  { if ( name_contains("FAIL_connection") ) // Test error handling
      throw std::runtime_error("MyConnection-fail(" + _name + ")");
  }

  bool open()
  { if ( name_contains("FAIL_open") ) // Test error handling
      return false;
    return true;
  }

  bool close() noexcept
  { if ( name_contains("FAIL_close") ) // Test error handling
      return false;
    return true;
  }

  void portray(PlStream& strm) const
  { strm.printf("Connection(name=%s)", name.c_str());
  }

  bool name_contains(const char *substr) const
  { return name.find(substr) != name.npos;
  }
};


// The following code is taken from
// pl2cpp2.doc \subsubsection{Sample PlBlob code}
// with some minor changes for testing

struct MyBlob;

static PL_blob_t my_blob = PL_BLOB_DEFINITION(MyBlob, "my_blob");

struct MyBlob : public PlBlob
{ std::unique_ptr<MyConnection> connection;
  std::string blob_name; // for debugging ... during shutdown, connection can
                         // be deleted before the blob is deleted

  explicit MyBlob()
    : PlBlob(&my_blob) { }

  explicit MyBlob(const std::string& connection_name)
    : PlBlob(&my_blob),
      connection(std::make_unique<MyConnection>(connection_name)),
      blob_name(connection_name)
  { if ( !connection ) // make_unique should have thrown exception if it can't allocate
      PL_api_error("MyBlob(%s) connection=%p", blob_name.c_str(), connection.get());
    if ( !connection->open() )
      throw MyBlobError("my_blob_open_error");
    if ( name_contains("FAIL_new") ) // Test error handling
      throw MyBlobError("my_blob_fail_new");
  }

  PL_BLOB_SIZE

  virtual ~MyBlob() noexcept
  { if ( !close() )
      // Can't use PL_warning()
      Sdprintf("***ERROR: Close MyBlob failed: (%s) (%s)\n",
               name().c_str(), blob_name.c_str());
  }

  std::string name() const
  { return connection ? connection->name : "";
  }

  bool close() noexcept
  { if ( !connection )
      return true;
    bool rc = connection->close();
    connection.reset(); // Can be omitted, leaving deletion to ~MyBlob()
    return rc;
  }

  PlException MyBlobError(const char* error) const
  { return PlGeneralError(PlCompound(error, PlTermv(symbol_term())));
  }

  int compare_fields(const PlBlob* _b_data) const override
  { // dynamic_cast is safer than static_cast, but slower (see documentation)
    // It's used here for testing (the documentation has static_cast)
    if ( name_contains("FAIL_compare") )
      throw MyBlobError("my_blob_compare_error"); // Test error handling
    auto b_data = dynamic_cast<const MyBlob*>(_b_data);
    return name().compare(b_data->name());
  }

  bool write_fields(IOSTREAM *s, int flags) const override
  { if ( name_contains("FAIL_write") )
      throw MyBlobError("my_blob_write_error"); // Test error handling
    PlStream strm(s);
    strm.printf(",");
    return write_fields_only(strm);
  }

  bool write_fields_only(PlStream& strm) const
  { if ( connection )
      connection->portray(strm);
    else
      strm.printf("closed");
    return true;
  }

  bool portray(PlStream& strm) const
  { strm.printf("MyBlob(");
    write_fields_only(strm);
    strm.printf(")");
    return true;
  }

  bool name_contains(const char *substr) const
  { return blob_name.find(substr) != blob_name.npos;
  }
};

// %! create_my_blob(+Name: atom, -MyBlob) is det.
PREDICATE(create_my_blob, 2)
{ auto ref = std::unique_ptr<PlBlob>(new MyBlob(A1.as_atom().as_string()));
  return A2.unify_blob(&ref);
}

// %! close_my_blob(+MyBlob) is det.
// % Close the connection, silently succeeding if is already
// % closed; throw an exception if something goes wrong.
PREDICATE(close_my_blob, 1)
{ auto ref = PlBlobV<MyBlob>::cast_ex(A1, my_blob);
  assert(A1 == ref->symbol_term());
  if ( !ref->close() )
    throw ref->MyBlobError("my_blob_close_error");
  return true;
}

// %! portray_my_blob(+Stream, +MyBlob) is det.
// % Hook predicate for
// %   user:portray(MyBlob) :-
// %     blob(MyBlob, my_blob), !,
// %     portray_my_blob(current_output, MyBlob).
PREDICATE(portray_my_blob, 2)
{ auto ref = PlBlobV<MyBlob>::cast_ex(A2, my_blob);
  PlStream strm(A1, 0);
  return ref->portray(strm);
}


// TODO: Add the following code to
// pl2cpp2.doc \subsubsection{Sample PlBlob code (pointer)}

struct MyFileBlob;

static PL_blob_t my_file_blob = PL_BLOB_DEFINITION(MyFileBlob, "my_file_blob");

static const PlOptionsFlag<int>
MyFileBlob_options("MyFileBlob-options",
                   { {"absolute", PL_FILE_ABSOLUTE},
                     {"ospath",   PL_FILE_OSPATH},
                     {"search",   PL_FILE_SEARCH},
                     {"exist",    PL_FILE_EXIST},
                     {"read",     PL_FILE_READ},
                     {"write",    PL_FILE_WRITE},
                     {"execute",  PL_FILE_EXECUTE},
                     {"noerrors", PL_FILE_NOERRORS} });

struct MyFileBlob : public PlBlob
{ std::FILE* file_;

  std::string mode_;
  int flags_;
  std::string filename_;
  std::vector<char> buffer_; // used by read(), to avoid re-allocation

  explicit MyFileBlob()
    : PlBlob(&my_file_blob) { }

  explicit MyFileBlob(PlTerm filename, PlTerm mode, PlTerm flags)
    : PlBlob(&my_file_blob),
      mode_(mode.as_string())
  { flags_ = MyFileBlob_options.lookup_list(flags);
    filename_ = filename.get_file_name(flags_);
    file_ = fopen(filename_.c_str(), mode_.c_str());
    if ( !file_ ) // TODO: get error code (might not be existence error)
      throw PlExistenceError("my_file_blob_open", PlTerm_string(filename_));
    // for debugging:
    //   PlTerm_string(filename.as_string() + "\" => \"" +
    //                 filename_ + "\", \"" + mode_ +
    //                 ", flags=" + MyFileBlob_options.as_string(flags_) + "\")")
  }

  PL_BLOB_SIZE

  std::string read(size_t count)
  { assert(sizeof buffer_[0] == sizeof (char));
    assert(sizeof (char) == 1);

    buffer_.reserve(count);
    return std::string(buffer_.data(),
                       std::fread(buffer_.data(), sizeof buffer_[0], count, file_));
  }

  bool eof() const
  { return std::feof(file_);
  }

  bool error() const
  { return std::ferror(file_);
  }

  virtual ~MyFileBlob() noexcept
  { if ( !close() )
      // Can't use PL_warning()
      Sdprintf("***ERROR: Close MyFileBlob failed: (%s)\n", filename_.c_str());
  }

  bool close() noexcept
  { if ( !file_ )
      return true;
    int rc = std::fclose(file_);
    file_ = nullptr;
    return rc == 0;
  }

  PlException MyFileBlobError(const std::string error) const
  { return PlGeneralError(PlCompound(error, PlTermv(symbol_term())));
  }

  int compare_fields(const PlBlob* _b_data) const override
  { // dynamic_cast is safer than static_cast, but slower (see documentation)
    // It's used here for testing (the documentation has static_cast)
    auto b_data = dynamic_cast<const MyFileBlob*>(_b_data);
    return filename_.compare(b_data->filename_);
  }

  bool write_fields(IOSTREAM *s, int flags) const override
  { PlStream strm(s);
    strm.printf(",");
    return write_fields_only(strm);
  }

  bool write_fields_only(PlStream& strm) const
  { // For debugging:
    // strm.printf("%s mode=%s flags=%s", filename_.c_str(), mode_.c_str(),
    //             MyFileBlob_options.as_string(flags_).c_str());
    strm.printf("%s", filename_.c_str());
    if ( !file_ )
      strm.printf("-CLOSED");
    return true;
  }

  bool portray(PlStream& strm) const
  { strm.printf("MyFileBlob(");
    write_fields_only(strm);
    strm.printf(")");
    return true;
  }
};

PREDICATE(my_file_open, 4)
{ auto ref = std::unique_ptr<PlBlob>(new MyFileBlob(A2, A3, A4));
  return A1.unify_blob(&ref);
}

PREDICATE(my_file_close, 1)
{ auto ref = PlBlobV<MyFileBlob>::cast_ex(A1, my_file_blob);
  if ( !ref->close() ) // TODO: get the error code
    throw ref->MyFileBlobError("my_file_blob_close_error");
  return true;
}

PREDICATE(my_file_filename_atom, 2)
{ auto ref = PlBlobV<MyFileBlob>::cast_ex(A1, my_file_blob);
  return A2.unify_atom(ref->filename_);
}

PREDICATE(my_file_filename_string, 2)
{ auto ref = PlBlobV<MyFileBlob>::cast_ex(A1, my_file_blob);
  return A2.unify_string(ref->filename_);
}

PREDICATE(my_file_read, 3)
{ auto ref = PlBlobV<MyFileBlob>::cast_ex(A1, my_file_blob);
  return A3.unify_string(ref->read(A2.as_int64_t()));
}

// %! my_file_blob_portray(+Stream, +MyFileBlob) is det.
// % Hook predicate for
// %   user:portray(MyFileBlob) :-
// %     blob(MyFileBlob, my_file_blob), !,
// %     my_file_blob_portray(current_output, MyFileBlob).
PREDICATE(my_file_blob_portray, 2)
{ auto ref = PlBlobV<MyFileBlob>::cast_ex(A2, my_file_blob);
  PlStream strm(A1, 0);
  return ref->portray(strm);
}


static AtomMap<PlAtom, PlAtom> map_atom_atom("add", "atom_atom");

PREDICATE(atom_atom_find, 2)
{ auto value = map_atom_atom.find(A1.as_atom());
  return value.not_null() && A2.unify_atom(value);
}

PREDICATE(atom_atom_add, 2)
{ map_atom_atom.insert(A1.as_atom(), A2.as_atom());
  return true;
}

PREDICATE(atom_atom_erase, 1)
{ map_atom_atom.erase(A1.as_atom());
  return true;
}

PREDICATE(atom_atom_size, 1)
{ return A1.unify_integer(map_atom_atom.size());
}

static AtomMap<PlTerm, PlRecord> map_atom_term("insert", "atom_term");

PREDICATE(atom_term_find, 2)
{ auto value = map_atom_term.find(A1.as_atom());
  return value.not_null() && A2.unify_term(value);
}

PREDICATE(atom_term_insert, 2)
{ map_atom_term.insert(A1.as_atom(), A2);
  return true;
}

PREDICATE(atom_term_erase, 1)
{ map_atom_term.erase(A1.as_atom());
  return true;
}

PREDICATE(atom_term_size, 1)
{ return A1.unify_integer(map_atom_term.size());
}


// ============  map_str_str blob (std::map<std::string,std::string> =====

class MapStrStr;

static PL_blob_t map_str_str_blob = PL_BLOB_DEFINITION(MapStrStr, "map_str_str");

class MapStrStr : public PlBlob
{
public:
  explicit MapStrStr()
    : PlBlob(&map_str_str_blob) { }
  virtual ~MapStrStr() = default;

  MapStrStr(const MapStrStr&) = delete;
  MapStrStr(MapStrStr&&) = delete;
  MapStrStr& operator =(const MapStrStr&) = delete;
  MapStrStr& operator =(MapStrStr&&) = delete;

  PL_BLOB_SIZE

  typedef std::map<const std::string, std::string>::iterator iterator;
  typedef std::map<const std::string, std::string>::const_iterator const_iterator;

  iterator begin() { return data.begin(); }
  const_iterator cbegin() { return data.cbegin(); }

  iterator end() { return data.end(); }
  const_iterator cend() { return data.cend(); }

  void insert_or_assign(const std::string& key, const std::string& value)
  { data[key] = value;
    // C17: data.insert_or_assign(key, value);
    // C11: auto [it, success] = data.insert({key, value});
    //      if ( ! success )
    //        it->second = value;
  }

  void erase_if_present(const std::string& key)
  { auto it = data.find(key);
    if ( it != data.end() )
      data.erase(it);
  }

  iterator find(const std::string& key)
  { return data.find(key);
  }

  const_iterator find(const std::string& key) const
  { return data.find(key);
  }

  iterator lower_bound(const std::string& key)
  { return data.lower_bound(key);
  }

  const_iterator lower_bound(const std::string& key) const
  { return data.lower_bound(key);
  }

  void incr_ref()
  { std::lock_guard<std::mutex> lock_(refcount_lock);
    refcount++;
    register_ref();
  }

  void decr_ref() {
    std::lock_guard<std::mutex> lock_(refcount_lock);
    refcount--;
    unregister_ref();
  }

private:
  std::mutex refcount_lock;
  long int refcount = 0;
  std::map<const std::string, std::string> data;
};

static bool
str_starts_with(const std::string& str, const std::string& prefix)
{ // TODO: C20 provides std::string::starts_with()
  return str.length() >= prefix.length() &&
    str.substr(0, prefix.length()) == prefix;
}

class MapStrStrEnumState
{
public:
  MapStrStrEnumState(MapStrStr* _ref, const std::string& _prefix)
    : ref(_ref), // initialization of ref must be first
      it(_prefix.empty() ? ref->begin() : ref->lower_bound(_prefix)),
      prefix(_prefix)
  { ref->incr_ref();
  }

  explicit MapStrStrEnumState() = delete;
  explicit MapStrStrEnumState(const MapStrStrEnumState&) = delete;
  explicit MapStrStrEnumState(MapStrStrEnumState&&) = delete;
  MapStrStrEnumState& operator =(const MapStrStrEnumState&) = delete;

  ~MapStrStrEnumState()
  { ref->decr_ref();
  }

  bool key_starts_with_prefix() const
  { return it != ref->end() && str_starts_with(it->first, prefix);
  }

private:
  MapStrStr *ref = nullptr;

public:
  MapStrStr::iterator it;
  std::string prefix;
};

// %! create_map_str_str(-Map) is det.
PREDICATE(create_map_str_str, 1)
{ auto ref = std::unique_ptr<PlBlob>(new MapStrStr());
  return A1.unify_blob(&ref);
}

// %! insert_or_assign_map_str_str(+Map, +Key:string, +Value:string) is det.
PREDICATE(insert_or_assign_map_str_str, 3)
{ auto ref = PlBlobV<MapStrStr>::cast_ex(A1, map_str_str_blob);
  ref->insert_or_assign(A2.as_string(), A3.as_string());
  return true;
}

// %! erase_if_present_map_str_str(+Map, +Key:string) is det.
PREDICATE(erase_if_present_map_str_str, 2)
{ auto ref = PlBlobV<MapStrStr>::cast_ex(A1, map_str_str_blob);
  ref->erase_if_present(A2.as_string());
  return true;
}

// %! find_map_str_str(+Map, +Key:string, -Value:string) is semidet.
PREDICATE(find_map_str_str, 3)
{ auto ref = PlBlobV<MapStrStr>::cast_ex(A1, map_str_str_blob);
  auto it = ref->find(A2.as_string());
  if ( it == ref->end() )
    return false;
  return A3.unify_string(it->second);
}

// %! enum_map_str_str(+Map, +Prefix:string, -Key:string, -Value:string) is nodet.
PREDICATE_NONDET(enum_map_str_str, 4)
{ // "state" needs to be acquired so that automatic cleaup deletes it
  auto state = handle.context_unique_ptr<MapStrStrEnumState>();
  const auto control = handle.foreign_control();
  if ( control == PL_PRUNED )
    return true;

  // Can use A1, A2, etc. after we know control != PL_PRUNED

  auto ref = PlBlobV<MapStrStr>::cast_ex(A1, map_str_str_blob);
  PlTerm prefix(A2), key(A3), value(A4);
  std::string prefix_str(prefix.get_nchars(CVT_STRING|CVT_ATOM|REP_UTF8));

  if ( key.is_ground() )
  { assert(control == PL_FIRST_CALL);
    const auto key_str(key.get_nchars(CVT_STRING|CVT_ATOM|REP_UTF8));
    if ( !str_starts_with(key_str, prefix_str) )
      return false;
    const auto f = ref->find(key_str);
    return f != ref->cend() && value.unify_string(f->second);
  }

  if ( control == PL_FIRST_CALL )
    state.reset(new MapStrStrEnumState(ref, prefix_str));
  else
    assert(control == PL_REDO);

  PlFrame fr;
  for ( ; state->key_starts_with_prefix() ; state->it++ )
  { if ( key.unify_string(state->it->first ) &&
         value.unify_string(state->it->second) )
    { state->it++;
      if ( state->key_starts_with_prefix() )
        PL_retry_address(state.release());
      else
        return true;
    }
    fr.rewind();
  }
  return false;
}

static const
PlOptionsFlag<unsigned int>
nchars_flags("nchars-flags",
             // compound flags are first, so, e.g. CVT_NUMBER subsumes CVT_RATIONAL, CVT_FLOAT
             { {"xinteger",        CVT_XINTEGER}, // must be before CVT_INTEGER, CVT_NUMBER
               {"all",             CVT_ALL},
               {"atomic",          CVT_ATOMIC},
               {"number",          CVT_NUMBER},

               {"atom",            CVT_ATOM},
               {"string",          CVT_STRING},
               {"integer",         CVT_INTEGER},
               {"list",            CVT_LIST},
               {"rational",        CVT_RATIONAL},
               {"float",           CVT_FLOAT},
               {"variable",        CVT_VARIABLE},
               {"write",           CVT_WRITE},
               {"write_canonical", CVT_WRITE_CANONICAL},
               {"writeq",          CVT_WRITEQ},

               {"exception",       CVT_EXCEPTION},
               {"varnofail",       CVT_VARNOFAIL},

               {"stack",           BUF_STACK},
               {"malloc",          BUF_MALLOC},
               {"allow_stack",     BUF_ALLOW_STACK},

               {"utf8",            REP_UTF8},
               {"mb",              REP_MB},
               {"diff_list",       PL_DIFF_LIST} });

static
std::string nchars_flags_string(unsigned int flags)
{ return nchars_flags.as_string(flags);
}

static const std::map<const std::string, unsigned int> write_option_to_flag =
{ { "atom",            CVT_ATOM },
  { "string",          CVT_STRING },
  { "list",            CVT_LIST },
  { "integer",         CVT_INTEGER },
  { "rational",        CVT_RATIONAL },
  { "float",           CVT_FLOAT },
  { "variable",        CVT_VARIABLE },
  { "write",           CVT_WRITE },
  { "write_canonical", CVT_WRITE_CANONICAL },
  { "writeq",          CVT_WRITEQ },

  { "exception",       CVT_EXCEPTION },
  { "varnofail",       CVT_VARNOFAIL },

  { "stack",           BUF_STACK },
  { "malloc",          BUF_MALLOC },
  { "allow_stack",     BUF_ALLOW_STACK },

  { "utf8",            REP_UTF8 },
  { "mb",              REP_MB },
  { "diff_list",       PL_DIFF_LIST },

  // combination flags:
  { "number",          CVT_NUMBER },
  { "atomic",          CVT_ATOMIC },
  { "all",             CVT_ALL },
  { "xinteger",        CVT_XINTEGER },
};

// For verifying the test_cpp.pl values:
// PREDICATE(p_flags, 0)
// { Sdprintf("%x\n", CVT_XINTEGER|CVT_ALL|CVT_ATOMIC|CVT_NUMBER);
//   Sdprintf("%x\n", CVT_ALL|CVT_ATOMIC|CVT_NUMBER);
//   Sdprintf("%x\n", CVT_ATOM|CVT_STRING|CVT_INTEGER|CVT_LIST|CVT_RATIONAL|CVT_FLOAT|CVT_VARIABLE|CVT_NUMBER|CVT_ATOMIC|CVT_WRITE|CVT_WRITE_CANONICAL|CVT_WRITEQ|CVT_ALL|CVT_XINTEGER);
//   Sdprintf("%x\n", CVT_ATOMIC|CVT_LIST);
//   Sdprintf("%x\n", CVT_NUMBER|CVT_ATOM|CVT_STRING);
//   return true;
// }

static
unsigned int nchars_flag(PlTerm list)
{ unsigned int flags = 0;
  PlTerm_tail tail(list);
  PlTerm_var e;
  while ( tail.next(e) )
  { e.must_be_atomic();
    const auto it = write_option_to_flag.find(e.as_string());
    if ( it == write_option_to_flag.cend() )
      throw PlDomainError("write-option", e);
    flags |= it->second;
  }
  PlCheckFail(tail.close());
  return flags;
}

PREDICATE(nchars_flags, 2)
{ return A2.unify_integer(nchars_flag(A1));
}

PREDICATE(nchars_flags_string, 2)
{ return A2.unify_string(nchars_flags_string(A1.as_uint()));
}

PREDICATE(get_nchars_string, 4)
{ unsigned int flags =
    A2.is_integer() ? A2.as_uint() : nchars_flag(A2);
  return A3.unify_string(A1.get_nchars(flags)) &&
    A4.unify_string(nchars_flags_string(flags));
}

NAMED_PREDICATE("#", hash, 2)
{ return A2.unify_string(A1.as_string());
}

PREDICATE(malloc_free, 2)
{ // For verifying that example code of unique_ptr with PL_free() compiles
  std::unique_ptr<void, decltype(&PL_free)> ptr(PL_malloc(100), &PL_free);

  PlStringBuffers _string_buffers;
  size_t len;
  char *str = nullptr;
  int rc = Plx_get_nchars(A1.unwrap(), &len, &str, BUF_MALLOC|CVT_ALL|CVT_WRITEQ|CVT_VARIABLE|REP_UTF8|CVT_EXCEPTION);
  std::unique_ptr<char, decltype(&PL_free)> _str(str, &PL_free);
  return rc && A2.unify_string(std::string(str, len));
}

static std::vector<std::string> lookup_unifies =
  { "item(one, 1)",
    "item(two, 2)",
    "item(three, 3)",
  };

PREDICATE(lookup_unify, 1)
{ PlFrame fr;
  for ( auto& s : lookup_unifies )
  { if ( A1.unify_term(PlCompound(s)) )
      return true;
    fr.rewind();
  }
  return false;
}

PREDICATE(free_blob, 1)
{ return Plx_free_blob(A1.as_atom().unwrap());
}

PREDICATE(nil_repr, 1)
{ char buf[100];
  snprintf(buf, sizeof buf, "%p", nullptr);
  return A1.unify_string(buf);
}

PREDICATE(compile_only_stream, 0)
{ // Has the various methods for PlStream, to ensure that they compile
  PlStream strm(Scurrent_input);
  int i;
  bool b;
  size_t s;
  int64_t i64;
  char *c;
  ssize_t ss;
  char buf[10];
  PlTerm_var ex;
  IOENC enc;
  PL_locale *loc1 = nullptr, *loc2;
  strm.release();
  strm.check_stream();
  i = strm.set_timeout(10);
  i = strm.unit_size();
  //   int putc(int c);
  //   int getc();
  //   int ungetc(int c);
  b = strm.canrepresent(10);
  i = strm.putcode(11);
  i = strm.getcode();
  i = strm.peekcode();
  i = strm.putw(13);
  i = strm.getw();
  s = strm.fread(buf, sizeof buf, 1);
    s = strm.fwrite(buf, sizeof buf, 1);
  i = strm.feof();
  i = strm.fpasteof();
  i = strm.ferror();
  strm.clearerr();
  i = strm.seterr(1, "foo");
  i = strm.set_exception(ex.unwrap());
  i = strm.setenc(ENC_ANSI, &enc);
  i = strm.setlocale(loc1, &loc2);
  i = strm.flush();
  i64 = strm.size();
  // [[deprecated("Use seek64()")]] int seek(int64_t pos, int whence);
  // [[deprecated("Use tell64()")]] int64_t tell();
  i = strm.close();
  i = strm.gcclose(1);
  c = strm.gets(buf, 3);
  ss = strm.read_pending(buf, 3, 0);
  s = strm.pending();
  i = strm.fputs("foo");
  i = strm.printf("%s", "foo");
  i = strm.printfX("%s", "foo");
  // i = strm.vprintf(const char *fm, va_list args);
  i = strm.lock();
  i = strm.tryLock();
  i = strm.unlock();
  i = strm.fileno();
  // int	closehook(void (*hook)(IOSTREAM *s));
  strm.setbuffer(buf, sizeof buf);

  i64 = strm.tell64();
  i = strm.seek64(1, SEEK_CUR);

  i = strm.checkBOM();
  i = strm.writeBOM();

  { int64_t  ip;    b = strm.qlf_get_int64(&ip);  }
  { int32_t  ip;    b = strm.qlf_get_int32(&ip);  }
  { uint32_t ip;    b = strm.qlf_get_uint32(&ip); }
  { double   fp;    b = strm.qlf_get_double(&fp); }
  { atom_t   a;     b = strm.qlf_get_atom(&a);    }
  { int64_t  i = 0; b = strm.qlf_put_int64(i);    }
  { int32_t  i = 0; b = strm.qlf_put_int32(i);    }
  { uint32_t i = 0; b = strm.qlf_put_uint32(i);   }
  { double f = 0.0; b = strm.qlf_put_double(f);   }
  { atom_t   a = 0; b = strm.qlf_put_atom(a);     }

  (void)i;
  (void)b;
  (void)s;
  (void)i64;
  (void)c;
  (void)ss;
  (void)enc;
  (void)loc1;
  (void)loc2;

  return false;
}


// Tests for PlTermScoped

bool
unify_atom_list(const std::vector<std::string>& array, PlTerm list)
{ PlTermScoped tail(list);
  term_t save_head = PlTerm::null; // For checking that PL_free_term_ref() is called
  for( auto item : array )
  { PlTermScoped head; // var term
    if ( save_head != PlTerm::null &&
         save_head != head.unwrap() )
      throw PlUnknownError("unify_atom_list head not reused");
    save_head = head.unwrap();
    PlCheckFail(tail.unify_list(head, tail));
    PlCheckFail(head.unify_chars(PL_ATOM, item));
  }
  return tail.unify_nil();
}

// The same code as unify_atom_list, using the C interface:
int
unify_atom_list_c(char **array, size_t len, term_t list)
{ term_t tail;

  if ( !(tail=PL_copy_term_ref(list)) )
    return FALSE;

  for(size_t i=0; i<len; i++)
  { term_t head;

    if ( !(head = PL_new_term_ref()) ||
	 !PL_unify_list(tail, head, tail) ||
	 !PL_unify_chars(head, PL_ATOM, (size_t)-1, array[i]) )
    {  PL_free_term_ref(head);
      return FALSE;
    }
    PL_free_term_ref(head);
  }

  if ( PL_unify_nil(tail) )
  { PL_free_term_ref(tail);
    return TRUE;
  }
  PL_free_term_ref(tail);
  return FALSE;
}

// unify_atom_list(In, Out)
PREDICATE(unify_atom_list, 2)
{ std::vector<std::string> array;
  PlTerm_tail tail(A1);
  PlTerm_var e;
  while( tail.next(e) )
    array.push_back(e.as_string());
  return tail.close() && unify_atom_list(array, A2);
}

// unify_atom_list_c(In, Out) - same as unify_atom_list/2
//                              but uses C API
PREDICATE(unify_atom_list_c, 2)
{ std::vector<std::string> array;
  PlTerm_tail tail(A1);
  PlTerm_var e;
  while( tail.next(e) )
    array.push_back(e.as_string());
  if ( ! tail.close() )
    return false;
  char** array2 = (char**)calloc(sizeof (char*), array.size());
  for( size_t i = 0; i < array.size(); i++ )
    array2[i] = strdup(array[i].c_str());

  int rc = unify_atom_list_c(array2, array.size(), A2.unwrap());

  for( size_t i = 0; i < array.size(); i++ )
    free(array2[i]);
  free(array2);
  return rc;
}

PREDICATE(term_release, 0) // TODO: make this into a proper test
{ PlStream strm(Scurrent_output);
  PlTermScoped t1;
  strm.printf("term_release: t1=%zd\n", t1.unwrap());
  if ( t1.is_null() )
    throw PlUnknownError("PlTermScoped t1 ctor didn't get a term");
  PlTermScoped t2;
  if ( t2.is_null() )
    throw PlUnknownError("PlTermScoped t2 ctor didn't get a term");
  if ( t1.unwrap() == t2.unwrap() )
    throw PlUnknownError("PlTermScoped t1 == t2");

  term_t save_t1 = t1.unwrap(), save_t2 = t2.unwrap();

  t1.swap(t2);
  if ( t1.unwrap() != save_t2 )
    throw PlUnknownError("PlTermScoped t1.swap(t2) 1 failed (1)");
  if ( t2.unwrap() != save_t1 )
    throw PlUnknownError("PlTermScoped t1.swap(t2) 1 failed (2)");
  std::swap(t1, t2); // TODO: add test that this swap is called when sorting a vector
  if ( t1.unwrap() != save_t1 )
    throw PlUnknownError("PlTermScoped swap(t1,t2) failed (1)");
  if ( t2.unwrap() != save_t2 )
    throw PlUnknownError("PlTermScoped swap(t1,t2) failed (2)");

  PlTermScoped t3(t1.release());
  if ( t1.not_null() )
    throw PlUnknownError("PlTermScoped t3(t1.release()) failed");
  if ( t3.unwrap() != save_t1 )
    throw PlUnknownError("PlTermScoped t3 ctor failed");

  PlTermScoped t4;
  t4.reset();
  if ( t4.not_null() )
    throw PlUnknownError("PlTermScoped t4.reset() failed");
  t4.reset(t3.release());
  if ( t3.not_null() )
    throw PlUnknownError("PlTermScoped t4.reset(t3.release()) failed");
  if ( t4.unwrap() != save_t1 )
    throw PlUnknownError("PlTermScoped t4 != save_t1");

  PlTermScoped t5(std::move(t4));
  if ( t4.not_null() )
    throw PlUnknownError("PlTermScoped std::move(t4) failed");
  if ( t5.unwrap() != save_t1 )
    throw PlUnknownError("PlTermScoped t5 != save_t1");

  PlTermScoped t6;
  t6 = std::move(t5);
  if ( t5.not_null() )
    throw PlUnknownError("PlTermScoped std::move(t5) failed");
  if ( t6.unwrap() != save_t1 )
    throw PlUnknownError("PlTermScoped t6 != save_t1");

  // TODO: why doesn't the following compile? (See t6, which does):
  // PlTermScoped t7 = std::move(t6);

  return true;
}

// record_ext(Term, External:string)
// if External is a variable, unifies it with the external record of Term
// else unifies Term with the conversion from the external record
PREDICATE(record_ext, 2)
{ PlTerm term(A1), external(A2);
  if ( external.is_variable() )
  { PlRecordExternalCopy ext(term);
    return external.unify_string(ext.data());
  }
  return term.unify_term(
      PlRecordExternalCopy::term(external.get_nchars(CVT_STRING|CVT_EXCEPTION)));
}

// Same as record_ext/2, but calls different method
PREDICATE(record_ext2, 2)
{ PlTerm term(A1), external(A2);
  if ( external.is_variable() )
  { PlRecordExternalCopy ext(term);
    return external.unify_string(ext.data());
  }
  PlRecordExternalCopy ext(external.get_nchars(CVT_STRING|CVT_EXCEPTION));
  return term.unify_term(ext.term());
}
