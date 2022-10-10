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
#include <math.h>
#include <cassert>
#include <string>
#include <map>
using namespace std;


PREDICATE(hello, 1)
{ cout << "Hello " << A1.as_string() << endl;
  cout << "Hello " << A1.as_string().c_str() << endl; // Same output as previous line
  cout << "Hello " << A1.as_string(EncLatin1).c_str() << endl; // Also same, if it's ASCII
  cout << "Hello " << A1.as_string(EncUTF8).c_str() << endl;
  cout << "Hello " << A1.as_string(EncLocale).c_str() << endl; // Can vary by locale settings

  return true;
}

PREDICATE(hello2, 1)
{ PlAtom atom_a1(A1);
  // The following have the same output as hello/1, if A1 is an atom
  cout << "Hello2 " << atom_a1.as_string() << endl;
  cout << "Hello2 " << A1.as_string().c_str() << endl;
  cout << "Hello2 " << A1.as_string(EncLatin1).c_str() << endl;
  cout << "Hello2 " << A1.as_string(EncUTF8).c_str() << endl;
  cout << "Hello2 " << A1.as_string(EncLocale).c_str() << endl;
  return true;
}

PREDICATE(hello2, 2)
{ PlAtom atom_a1(A1);
  PlCheck(A2.unify_string(atom_a1.as_string(EncUTF8)));
  return true;
}

PREDICATE(hello3, 1)
{ PlAtom atom_a1(A1);

  // Iostream doesn't work because `<<` doesn't support std::wstring:
  //   cout << "Hello3 " << atom_a1.wstring() << endl; /* Same output as hello/1 */

  // If %s is used, an error will occur if A1 has a non-ascii
  // character in it. In addition, a NUL ('\0') in the atom will cause
  // the rest of the atom to not be printed.

  if ( Sfprintf(Suser_output, "Hello3 %Ws\n", atom_a1.as_wstring().c_str()) > 0 )
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
{ cout << "name = " << A1.name().as_string() << ", arity = " << A1.arity() << endl;

  return true;
}

PREDICATE(name_arity, 3)		/* name_arity(+Term, -Name, -Arity) */
{ PlTerm term(A1);
  PlTerm name(A2);
  PlTerm arity(A3);

  PlCheck(name.unify_atom(term.name()));
  PlCheck(arity.unify_uint64(term.arity()));

  return true;
}

PREDICATE(list_modules, 0)
{ PlTermv av(1);

  PlQuery q("current_module", av);
  while( q.next_solution() )
    cout << av[0].as_string() << endl;

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
  if ( A1.as_string() == "string" )
    return A2.unify_string("hello world");
  if ( A1.as_string() == "code_list" )
    return A2.unify_list_codes("hello world");
  if ( A1.as_string() == "char_list" )
    return A2.unify_list_chars("hello world");
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
  { if ( !l3.append(e) )
      return false;
  }

  return A2.unify_term(l3);
}

PREDICATE(call_atom, 1)
{ try
  { return static_cast<foreign_t>(PlCall(A1.as_wstring().c_str())); // TODO: PlCall(A1.wstring())
  } catch ( PlTypeError &ex )
  { cerr << "Type Error caught in C++" << endl;
    cerr << "Message: \"" << ex.as_string() << "\"" << endl;
    throw;
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
{ PlCheck(A2.unify_uint64(A1.as_uint64_t())); // TODO: unify_integer(...)
  return true;
}

PREDICATE(make_int64, 2)
{ int64_t i;
  // This function is for testing PlCheck()
  PlCheck(PL_get_int64_ex(A1.C_, &i));
  PlCheck(A2.unify_int64(i)); // TODO: unify_integer(...)
  return true;
}

/* The manual example uses gethostname(), but portability thereof is not
   trivial and we should not introduce portability issues on tests that
   are not about portability.
*/

static int
no_gethostname(char *buf, size_t len)
{ strcpy(buf, "example.org");

  return 0;
}

PREDICATE(hostname, 1)
{ char buf[255+1]; // SUSv2; POSIX.1 has a smaller HOST_NAME_MAX+1

  if ( no_gethostname(buf, sizeof buf) == 0 )
    return A1.unify_atom(buf);

  return false;
}

PREDICATE(hostname2, 1)
{ char buf[255+1]; // SUSv2; POSIX.1 has a smaller HOST_NAME_MAX+1
  if ( no_gethostname(buf, sizeof buf) != 0 )
    throw PlFail();
  PlCheck(A1.unify_atom(buf));
  return true;
}


PREDICATE(ensure_PlTerm_forward_declarations_are_implemented, 0)
{ /******************************************
   * This code is not intended to be        *
   * executed; only compiled, to check that *
   * implementations exist where expected.  *
   ******************************************/
  PlTerm_var t_var;
  PlTerm_atom t_atom1("abc");
  PlTerm_atom t_atom2(L"ABC");
  PlTerm_atom t_atom3(PlAtom("an atom"));
  PlTerm_atom p_atom4(std::string("abc"));
  PlTerm_atom p_atom5(std::wstring(L"世界"));
  PlTerm_term_t t_t(PL_new_term_ref());
  PlTerm_term_t t_null; // null
  PlTerm_integer t_int1(INT_MAX);
  PlTerm_integer t_int1b(INT_MIN);
  PlTerm_integer t_int2(LONG_MAX);
  PlTerm_integer t_int2b(LONG_MIN);
  PlTerm_int64 t_int64(INT64_MAX);
  PlTerm_int64 t_int64b(INT64_MIN);
  PlTerm_uint64 t_uint64(UINT64_MAX);
  PlTerm_uint64 t_uint64b(0);
  PlTerm_size_t p_size(static_cast<size_t>(-1));
  PlTerm_size_t p_size2(SIZE_MAX);
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
  // PlAtom a5(t_atom1); // TODO: why doesn't this work?
  PlAtom atom_null;
  // Unsafe
  //const char *   x01 = t_var.as_string().c_str();
  //const wchar_t *x01a = t_var.as_wstring().c_str();
  const std::string s01 = atom3.as_string();
  const std::wstring s01b = atom4.as_wstring();
  const std::string s02a = t_var.as_string();
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

  // TODO: add comparisons, etc.

  //(void)x01;
  //(void)x01a;
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
  PlCheck(A1.unify_uint64(i_unsigned));
  PlCheck(A1.unify_integer(i_long));
  PlCheck(A1.unify_uint64(i_unsigned_long));
  PlCheck(A1.unify_uint64(i_size)); // TODO: unify_size_t(...)
  PlCheck(A1.unify_integer(i_int32));
  PlCheck(A1.unify_uint64(i_uint32));
  PlCheck(A1.unify_int64(i_int64));   // TODO: unify_integer(...)
  PlCheck(A1.unify_uint64(i_uint64)); // TODO: unify_integer(...)

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

// 0.68 sec
static foreign_t
unify_zero_0(term_t a1)
{ return static_cast<foreign_t>(PL_unify_integer(a1, 0));
}

// If you wish to use the C-style install_cpp4pl() style instead, you
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

// end of benchmarking predicates


// Predicates for checking native integer handling

#define DECLS \
  X("int",           int,           INT_MIN,               INT_MAX)    \
  X("unsigned",      unsigned,      0,                     UINT_MAX)   \
  X("long",          long,          LONG_MIN,              LONG_MAX)   \
  X("unsigned long", unsigned long, 0,                     ULONG_MAX)  \
  X("size_t",        size_t,        0,                     SIZE_MAX) \
  X("int32_t",       int32_t,       INT32_MIN,             INT32_MAX) \
  X("uint32_t",      uint32_t,      0,                     UINT32_MAX) \
  X("uint64_t",      uint64_t,      0,                     UINT64_MAX) \
  X("int64_t",       int64_t,       INT64_MIN,             INT64_MAX) \
  X("intptr_t",      intptr_t,      INTPTR_MIN,            INTPTR_MAX) \
  X("uintptr_t",     uintptr_t,     0,                     UINTPTR_MAX)

#define X(name, x_type, x_min, x_max)  \
    {name, \
     PlCompound("int_info", \
                PlTermv(PlTerm_atom(name), \
                        PlTerm_size_t(sizeof (x_type)), \
                        PlTerm_int64(x_min), \
                        PlTerm_uint64(x_max))).record() },

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
  if ( A1.is_variable() )
  { switch( PL_foreign_control(handle) )
    { case PL_FIRST_CALL:
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
  } else
  { return int_info_(A1.as_string(), A2);
  }
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
