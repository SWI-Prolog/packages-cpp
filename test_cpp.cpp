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

#define PROLOG_MODULE "user"
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "SWI-cpp2.h"
#include <iostream>
#include <sstream>
#include <unistd.h>
#include <errno.h>
#include <math.h>
#include <cassert>
#include <limits>
#include <string>
#include <map>
using namespace std;


PREDICATE(hello, 2)
{ std::stringstream buffer;
  buffer << "Hello " << A1.as_string() << endl;
  buffer << "Hello " << A1.as_string().c_str() << endl; // Same output as previous line
  buffer << "Hello " << A1.as_string(EncLatin1).c_str() << endl; // Also same, if it's ASCII
  buffer << "Hello " << A1.as_string(EncUTF8).c_str() << endl;
  buffer << "Hello " << A1.as_string(EncLocale).c_str() << endl; // Can vary by locale settings

  return A2.unify_string(buffer.str());
}

PREDICATE(hello2, 2)
{ PlAtom atom_a1(A1.as_atom());
  std::stringstream buffer;
  // The following have the same output as hello/1, if A1 is an atom
  buffer << "Hello2 " << atom_a1.as_string() << endl;
  buffer << "Hello2 " << A1.as_string().c_str() << endl;
  buffer << "Hello2 " << A1.as_string(EncLatin1).c_str() << endl;
  buffer << "Hello2 " << A1.as_string(EncUTF8).c_str() << endl;
  buffer << "Hello2 " << A1.as_string(EncLocale).c_str() << endl;

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
  if ( len > 0 )
    // TODO: use len when fixed: https://github.com/SWI-Prolog/swipl-devel/issues/1074
    return A2.unify_chars(PL_STRING|REP_UTF8, strlen(buf), buf);
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

PREDICATE(name_arity, 2)
{ std::stringstream buffer;

  buffer << "name = " << A1.name().as_string() << ", arity = " << A1.arity() << endl;
  return A2.unify_string(buffer.str());
}

PREDICATE(name_arity, 3)		/* name_arity(+Term, -Name, -Arity) */
{ PlTerm term(A1);
  PlTerm name(A2);
  PlTerm arity(A3);

  PlCheck(name.unify_atom(term.name()));
  PlCheck(arity.unify_integer(term.arity()));

  return true;
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

PREDICATE(hello, 0)
{ PlQuery q("write", PlTermv(PlTerm_atom("hello world\n")));
  PlCheck(q.next_solution());
  return true;
}

PREDICATE(hello_query, 2)
{ PlQuery q(A1.as_string(), PlTermv(A2));
  PlCheck(q.next_solution());
  // There's no need for calling q.cut() - it's done implicitly by the
  // query's destructor.
  return true;
}

PREDICATE(call_cut, 1)
{ PlQuery q(A1.as_string(), PlTermv());
  PlCheck(q.next_solution());
  q.cut();
  return true;
}

PREDICATE(hello_call, 1)
{ PlCheck(PlCall(A1));
  return true;
}

PREDICATE(atom_to_string, 2)
{ PlAtom a(A1.as_atom());
  PlCheck(A2.unify_string(a.as_string(EncUTF8)));
  return true;
}

PREDICATE(term_to_string, 2)
{ PlCheck(A2.unify_string(A1.as_string(EncUTF8)));
  return true;
}

PREDICATE(term, 1)
{ return A1.unify_term(PlCompound("hello", PlTermv(PlTerm_atom("world"))));
}

PlAtom ATOM_atom("atom");

PREDICATE(term, 2)
{ PlAtom a(A1.as_atom());

  if ( a.C_ == ATOM_atom.C_ )
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


PREDICATE(can_unify, 2)
{ PlFrame fr;

  bool rval = A1.unify_term(A2);
  fr.rewind();
  return rval;
}

PREDICATE(eq1, 2)
{ PlCheck(A1.unify_term(A2));
  return true;
}

PREDICATE(eq2, 2)
{ return A1.unify_term(A2);
}

PREDICATE(eq3, 2)
{ PlCheck(PL_unify(A1.C_, A2.C_));
  return true;
}

PREDICATE(write_list, 1)
{ PlTerm_tail tail(A1);
  PlTerm_var e;

  while(tail.next(e))
    cout << e.as_string() << endl;

  return true;
}

PREDICATE(cappend, 3)
{ PlTerm_tail l1(A1);
  PlTerm_tail l3(A3);
  PlTerm_var e;

  while(l1.next(e))
    PlCheck(l3.append(e));

  return A2.unify_term(l3);
}

// TODO: This doesn't do quite what's expected if there's an
//       exception.  Instead of returning the exception to Prolog, it
//       ends up in the debugger.
//       Possibly this is because PlCall needs the flags
//       PL_Q_CATCH_EXCEPTION and not PL_Q_PASS_EXCEPTION?
PREDICATE(cpp_call_, 3)
{ int flags = A2.as_int();
  int verbose = A3.as_bool();
  std::string flag_str;
  // if ( flags & PL_Q_DEBUG )        flag_str.append(",debug");
  // if ( flags & PL_Q_DETERMINISTIC) flag_str.append(",deterministic");
  if ( flags & PL_Q_NORMAL )          flag_str.append(",normal");
  if ( flags & PL_Q_NODEBUG )         flag_str.append(",nodebug");
  if ( flags & PL_Q_CATCH_EXCEPTION)  flag_str.append(",catch_exception");
  if ( flags & PL_Q_PASS_EXCEPTION)   flag_str.append(",pass_exception");
  if ( flags & PL_Q_ALLOW_YIELD)      flag_str.append(",allow_exception");
  if ( flags & PL_Q_EXT_STATUS)       flag_str.append(",ext_status");
  if ( flag_str.empty() )
    flag_str = "cpp_call";
  else
    flag_str = std::string("cpp_call(").append(flag_str.substr(1)).append(")");
  if ( verbose )
    cout << flag_str << ": " << A1.as_string() << endl;

  try {
    int rc = PlCall(A1, flags);
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
	cout << "... after call, rc=" << rc << ": " << status_str << endl;
    } else
    { if ( verbose )
	cout << "... after call, rc=" << rc << endl;
    }

    if ( rc )
    { if ( verbose )
	cout << "cpp_call result: rc=" << rc << ": " << A1.as_string() << endl;
    } else
    { PlException_qid ex;
      if ( ex.is_null() )
      { if ( verbose )
	  cout << "cpp_call failed" << endl;
      } else
      { if ( verbose )
	  cout << "cpp_call failed: ex: " << ex.as_string() << endl;
      }
    }
    return rc; // TODO: this is wrong with some query flags
  } catch ( PlException& ex )
  { if ( ex.is_null() )
    { if ( verbose )
	cout << "cpp_call except is_null" << endl;
    } else
    { if ( verbose )
	cout << "cpp_call exception: " << ex.as_string() << endl;
    }
    throw;
  }
}

PREDICATE(cpp_atom_codes, 2)
{ int rc = PlCall("atom_codes", PlTermv(A1, A2));
  if ( ! rc )
  { PlException_qid ex;
    if ( ex.is_null() )
      cout << "atom_codes failed" << endl;
    else
      cout << "atom_codes failed: ex: " << ex.as_string() << endl; // Shouldn't happen
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

  for(int i=0; i<end; i++)
    PlCheck(list.append(PlTerm_float(sqrt(double(i)))));

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
{ void *ptr = A1.as_pointer();
  free(ptr);
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
{ auto f = PlFunctor(A1.as_atom().as_string().c_str(), 1);
  return A3.unify_functor(f) &&
    A3[1].unify_term(A2);
}

PREDICATE(make_uint64, 2)
{ PlCheck(A2.unify_integer(A1.as_uint64_t()));
  return true;
}

PREDICATE(make_int64, 2)
{ int64_t i;
  // This function is for testing PlCheck()
  PlCheck(PL_get_int64_ex(A1.C_, &i));
  PlCheck(A2.unify_integer(i));
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
  PlCheck(A1.unify_atom(buf));
  return true;
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
  PlTerm_term_t t_t(PL_new_term_ref());
  PlTerm_term_t t_null(PlTerm::null);
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
  PlTerm_recorded t_rec(PlTerm_atom("xyz").record());
  PlTerm_string t_string1("abc");
  PlTerm_string t_string2(L"世界");
  const char codes[] = {81,82,83,0};
  PlTerm_list_codes s02(codes);
  PlTerm_list_chars s03("mno");
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
  // TODO: combine this test with t_something.integer(&x04) etc.
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

  // TODO: add comparisons, etc.

  //(void)x01;
  //(void)x01a;
  // TODO: std::string string() const;
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

  PlCheck(A1.unify_integer(i_int));
  PlCheck(A1.unify_integer(i_unsigned));
  PlCheck(A1.unify_integer(i_long));
  PlCheck(A1.unify_integer(i_unsigned_long));
  PlCheck(A1.unify_integer(i_size));
  PlCheck(A1.unify_integer(i_int32));
  PlCheck(A1.unify_integer(i_uint32));
  PlCheck(A1.unify_integer(i_int64));
  PlCheck(A1.unify_integer(i_uint64));

  return true;
}

// The following are for verifying some documentation details.

PREDICATE(c_PL_unify_nil, 1)          { return static_cast<foreign_t>(PL_unify_nil(A1.C_)); }

PREDICATE(cpp_unify_nil, 1)           { return A1.unify_nil(); }

PREDICATE(check_c_PL_unify_nil, 1)    { PlCheck(PL_unify_nil(A1.C_));    return true; }

// Repeat the above 4, for *_ex():

PREDICATE(c_PL_unify_nil_ex, 1)       { return static_cast<foreign_t>(PL_unify_nil_ex(A1.C_)); }

PREDICATE(cpp_unify_nil_ex, 1)        { return A1.unify_nil_ex(); }

PREDICATE(check_c_PL_unify_nil_ex, 1) { PlCheck(PL_unify_nil_ex(A1.C_)); return true; }



PREDICATE(c_PL_get_nil, 1)            { return static_cast<foreign_t>(PL_get_nil(A1.C_)); }

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
  if ( ctxt->i >= ctxt->high )
    return true; // Last result: succeed without a choice point

  ctxt.keep();
  PL_retry_address(ctxt.get()); // Succeed with a choice point
}



// For benchmarking `throw PlThrow()` vs `return false`
// Times are given for 10 million failures
// e.g.: time((between(1,10000000,X), unify_zero_0(X))).
// Baseline: time((between(1,10000000,X), fail)).  0.44 sec

// 0.68 sec  - essentially the same for time((... X=0).
static foreign_t
unify_zero_0(term_t a1)
{ return static_cast<foreign_t>(PL_unify_integer(a1, 0));
}

// If you wish to use the C-style install_test_cpp() style instead, you
// need to use extern "C" to ensure that names don't get mangled.
// So, it's easier to use the PlRegister class (which might need
// modification to more than one argument).

static PlRegister _x_unify_zero_4_1(nullptr, "unify_zero_0", unify_zero_0);

// 0.68 sec
PREDICATE(unify_zero_1, 1)
{ if ( !PL_unify_integer(A1.C_, 0) )
    return false;
  return true;
}

// 10.9 sec
PREDICATE(unify_zero_2, 1)
{ if ( !PL_unify_integer(A1.C_, 0) )
    throw PlFail();
  return true;
}

// 13.5 sec
PREDICATE(unify_zero_3, 1)
{ PlCheck( PL_unify_integer(A1.C_, 0) );
  return true;
}

// 15.1 sec
PREDICATE(unify_zero_4, 1)
{ PlCheck(A1.unify_integer(0));
  return true;
}

// 0.71 sec
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


// Predicates for checking native integer handling
// See https://en.cppreference.com/w/cpp/types/numeric_limits
// TODO: typeid(ty).name() (needs #include <typeinfo>, #include <typeindex>)

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
  DECLS_ROW(size_t)   \
  DECLS_ROW(int32_t)  \
  DECLS_ROW(uint32_t) \
  DECLS_ROW(uint64_t) \
  DECLS_ROW(int64_t)  \
  DECLS_ROW(intptr_t) \
  DECLS_ROW(uintptr_t)

#define X(name, x_type, x_min, x_max)                    \
    {name,                                               \
     PlCompound("int_info",                              \
		PlTermv(PlTerm_atom(name),               \
			PlTerm_integer(sizeof (x_type)),  \
			PlTerm_integer(x_min),             \
			PlTerm_integer(x_max))).record() },

typedef std::map<const std::string, record_t> IntInfo;

static const IntInfo int_info = { DECLS };
#undef X

struct IntInfoContext
{ IntInfo::const_iterator it;
  explicit IntInfoContext()
    : it(int_info.cbegin()) { }
};

static bool
int_info_(const std::string name, PlTerm result)
{ const auto it = int_info.find(name);
  if ( it == int_info.cend() )
    return false;

  PlTerm t = PlTerm_recorded(it->second);
  return PlRewindOnFail([result,t]() -> bool { return result.unify_term(t); });
}

PREDICATE_NONDET(int_info, 2)
{ PlForeignContextPtr<IntInfoContext> ctxt(handle);

  // When PL_PRUNED is called A1 is not bound;
  // therefore, we need to do the switch on PL_foreign_control(handle)
  // before checking A1.is_variable(). We can't put the test for
  // A1.is_variable outside the PL_foreign_control(handle) switch
  // because when PL_PRUNED happens, A1 might not be a variable. That
  // is, we can't use A1.is_variable() as a way of checking whether we
  // should do backtracking or not. So, we need to do an extra test
  // for PL_FIRST_CALL and not allocate ctxt for backtracking if
  // !A1.is_variable().  (There are, of course, other ways of
  // structuring this code.)

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
      if ( !A1.is_variable() ) // int_info is a map, so unique on lookup
	return int_info_(A1.as_string(), A2);
      ctxt.set(new IntInfoContext());
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
  while ( ctxt->it != int_info.cend() )
  { if ( int_info_(ctxt->it->first, A2 ) )
    { PlCheck(A1.unify_atom(ctxt->it->first));
      ctxt->it++;
      if ( ctxt->it == int_info.cend() )
	return true; // Last result: no choice point
      ctxt.keep();
      PL_retry_address(ctxt.get()); // Succeed with choice point
    }
    ctxt->it++;
  }
  return false;
}


PREDICATE(type_error_string, 3)
{ PlException e(PlTypeError("foofoo", A1));
  std::wstring msg(e.as_wstring());
  PlCheck(A2.unify_string(msg));
  PlCheck(A3.unify_term(e));
  return true;
}


// Re-implementing w_atom_ffi_/2 in ffi4pl.c:

PREDICATE(w_atom_cpp_, 2)
{ auto stream = A1, t = A2;
  IOSTREAM* s;
  PlCheck(PL_get_stream(stream.C_, &s, SIO_INPUT));
  { PlStringBuffers _string_buffers;
    size_t len;
    const pl_wchar_t *sa = PL_atom_wchars(t.as_atom().C_, &len);
    Sfprintf(s, "/%Ws/%zd", sa, len);
  }
  return TRUE;
}


/* TODO: Move the "cpp_options" predicate and the associated tests
	 to somewhere in main SWI-Prolog system. */

static PL_option_t scan_options[] =
{ PL_OPTION("quoted",   OPT_BOOL),
  PL_OPTION("length",   OPT_SIZE),
  PL_OPTION("callback", OPT_TERM),
  PL_OPTION("token",    OPT_ATOM),
  PL_OPTION("descr",    OPT_STRING),
  PL_OPTIONS_END
};

// cpp_options(+Options:list, +Opt_all:bool, -Result)
//   Result is: cpp_options(Quoted,Length,Callback,Token,Descr)
PREDICATE(cpp_options, 3)
{ auto options = A1, opt_all = A2, result = A3;
  int         quoted     = false;
  size_t      length     = 10;
  PlTerm_var  callback;
  PlAtom      token(PlAtom::null);
  const char *descr      = "";
  bool        opt_all_v  = opt_all.as_bool();
  int         flags      = opt_all_v ? OPT_ALL : 0;

  PlStringBuffers _string_buffers; // for descr's contents
  PlCheck(PL_scan_options(options.C_, flags, "cpp_options", scan_options,
			  &quoted, &length, &callback.C_, &token.C_, &descr));

  PlCheck(result.unify_term(PlCompound("options",
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

PREDICATE(throw_domain_ffi, 1)
{ return PL_domain_error("footype", A1.C_);
}

PREDICATE(throw_domain_cpp1, 1)
{ throw PlDomainError("footype", A1);
}

PREDICATE(throw_domain_cpp2, 1)
{ PlCheck(PL_domain_error("footype", A1.C_));
  return false; // Should never reach here
}

PREDICATE(throw_domain_cpp3, 1)
{ PL_domain_error("footype", A1.C_);
  throw PlFail();
}

PREDICATE(throw_domain_cpp4, 1)
{ return PlDomainError("footype", A1).plThrow();
}
