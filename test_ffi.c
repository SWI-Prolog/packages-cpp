/*  Part of SWI-Prolog

    Author:        Peter Ludemann
    E-mail:        peter.ludemann@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2023, SWI-Prolog Solutions b.v.
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

/* This is used by test_ffi.pl */

/* This tests the C interface and not the C++ interface.
   But it was most convenient to put the test here. */

#define _CRT_SECURE_NO_WARNINGS 1
#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <assert.h>
#include <string.h>
#include <wctype.h>

#ifdef O_DEBUG
#define DEBUG(g) g
#else
#define DEBUG(g) (void)0
#endif

/* range_ffi/3 is used in regression tests
   - PL_foreign_context() passing an int for the context.
 */
static foreign_t
range_ffi(term_t t_low, term_t t_high, term_t t_result, control_t handle)
{ intptr_t result = 0;

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
      { long r;
	if ( !PL_get_long_ex(t_low, &r) )
	  PL_fail;
	result = r;
      }
      break;
    case PL_REDO:
      result = PL_foreign_context(handle);
      break;
    case PL_PRUNED:
      PL_succeed;
    default:
      assert(0);
  }

  { long high;
    if ( !PL_get_long_ex(t_high, &high) ||
	 result >= high ||
	 !PL_unify_integer(t_result, result) )
      PL_fail;
    if ( result + 1 == high )
      PL_succeed; /* Last result: succeed without a choice point */
    PL_retry(result + 1); /* Succeed with a choice point */
  }
}

/* range_ffialloc/3 is used in regression tests:
   - PL_foreign_context_address() and malloc()-ed context.
*/
struct range_ctxt
{ long i;
  long high;
};

static foreign_t
range_ffialloc(term_t t_low, term_t t_high, term_t t_result, control_t handle)
{ struct range_ctxt *ctxt;

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
      { long low, high;
	if ( !PL_get_long_ex(t_low, &low) ||
             !PL_get_long_ex(t_high, &high) )
	  PL_fail;
	if ( !(ctxt = malloc(sizeof *ctxt) ) )
	  return (foreign_t)PL_resource_error("memory");
	ctxt->i = low;
        ctxt->high = high;
      }
      break;
    case PL_REDO:
      ctxt = PL_foreign_context_address(handle);
      break;
    case PL_PRUNED:
      ctxt = PL_foreign_context_address(handle);
      free(ctxt);
      PL_succeed;
    default:
      assert(0);
      PL_fail;
  }

  if ( ctxt->i >= ctxt->high ||
       !PL_unify_integer(t_result, ctxt->i) )
  { free(ctxt);
    PL_fail;
  }

  ctxt->i += 1;
  if ( ctxt->i == ctxt->high )
  { free(ctxt);
    PL_succeed; /* Last result: succeed without a choice point */
  }
  PL_retry_address(ctxt); /* Succeed with a choice point */
}

// Regression test for https://github.com/SWI-Prolog/packages-pcre/issues/20
static foreign_t
w_atom_ffi_(term_t stream, term_t t)
{ IOSTREAM* s;
  atom_t a;
  if ( !PL_get_stream(stream, &s, SIO_OUTPUT) ||
       !PL_get_atom_ex(t, &a) )
    return FALSE;
  PL_STRINGS_MARK();
    size_t len;
    const pl_wchar_t *sa = PL_atom_wchars(a, &len);
    SfprintfX(s, "/%Ws/%zd", sa, len);
  PL_STRINGS_RELEASE();
  return TRUE;
}

/* Regression test for https://github.com/SWI-Prolog/packages-pcre/issues/20
 * (big-endian, little-endian issues). */
static foreign_t
atom_ffi_(term_t stream, term_t t)
{ IOSTREAM* s;
  atom_t a;
  if ( !PL_get_stream(stream, &s, SIO_INPUT) ||
       !PL_get_atom_ex(t, &a) )
    return FALSE;
  PL_STRINGS_MARK();
    size_t len;
    const char *sa = PL_atom_nchars(a, &len);
    Sfprintf(s, "/%s/%zd", sa, len);
  PL_STRINGS_RELEASE();
  return TRUE;
}

static PL_option_t ffi_options[] =
{ PL_OPTION("quoted",   OPT_BOOL),
  PL_OPTION("length",   OPT_SIZE),
  PL_OPTION("callback", OPT_TERM),
  PL_OPTION("token",    OPT_ATOM),
  PL_OPTION("descr",    OPT_STRING),
  PL_OPTIONS_END
};

/* This is a slight variant of the example in foreign.doc - it unifies
   the callback value with the 1st argument and prints out the other
   values.
   TODO: make this compatible with cpp_options in test_cpp.c
*/
static foreign_t
ffi_options_(term_t a1, term_t options)
{ int    quoted     = FALSE;
  size_t length     = 10;
  term_t callback   = PL_new_term_ref(); /* default is a variable */
  atom_t token      = ATOM_nil;
  const char *descr = "";
  int rc;

  PL_STRINGS_MARK();
    rc = PL_scan_options(options, 0, "ffi_options", ffi_options,
		         &quoted, &length, &callback, &token, &descr) &&
      PL_unify_term(a1,
		    PL_FUNCTOR_CHARS, "options", 5,
		    PL_BOOL,        quoted,
		    PL_INT64,       (int64_t)length,
		    PL_TERM,        callback,
		    PL_ATOM,        token,
		    PL_UTF8_STRING, descr);
  PL_STRINGS_RELEASE();
  return rc;
}


/* ffi_term_chars() must be done inside PL_STRINGS_{MARK,RELEASE}
   Besides (term_t)0, it also accepts (term_t)-1 for "no exception",
   which isn't part of SWI-Prolog, but is convenient for this code
   (and is also used in isCauthInOuterQuery() in pl-wam.c)
 */
static const char *
ffi_term_chars(term_t t)
{ char *s;

  if ( t == (term_t)-1 )
    return "<no-exception>";
  if ( !t )
    return "<null-term>";

  if ( PL_get_nchars(t, NULL, &s, CVT_ALL|CVT_WRITEQ|CVT_EXCEPTION) )
    return s;

  return "<invalid term>";
}

/* Unify A1 and A2 if use_unify, else
   Unify A2 with A1.as_string() */
static int
unify_term_as_term_or_string(term_t A1, term_t A2, int use_unify)
{ if ( A1 && use_unify )
    return PL_unify(A1, A2);

  char buf[1000]; /* TODO: malloc as big as needed */
  int u_rc;

  PL_STRINGS_MARK();
    if ( A1 )
    { char *s;

      int nchars_rc = PL_get_nchars(A1, NULL, &s,
				    CVT_ALL|CVT_WRITEQ|REP_UTF8|CVT_EXCEPTION);
      if ( nchars_rc )
	SsnprintfX(buf, sizeof buf, "<%" PRIuPTR ">:%Us", A1, s);
      else
        Ssnprintf(buf, sizeof buf, "<unknown-term:%" PRIuPTR ">", A1);
    } else
    { Ssnprintf(buf, sizeof buf, "%s", "<null-term>");
    }
    u_rc = PL_unify_chars(A2, PL_STRING|REP_UTF8, (size_t)-1, buf);
  PL_STRINGS_RELEASE();

  return u_rc;
}

/* Additional pseudo-flags for controlling what happens after
   PL_next_solution() in ffi_call_().
   If this is updated, you must also update query_flag/2 in test_ffi.pl
   and query_flags_str()
*/

#define XX_Q_CLEAR_RETURN_TRUE  0x01000
#define XX_Q_CLOSE_QUERY        0x02000
#define XX_Q_EXC_TERM           0x04000

/* For debugging: turn the query call flags into human-readable form.
   This is mainly intended for verifying that query_flags/2 has done
   what we expect.
*/
static foreign_t
query_flags_str_(term_t flags_t, term_t flags_str_t)
{ char flags_str[200];
  int flags;
  if ( !PL_get_integer_ex(flags_t, &flags) )
    return FALSE;

  flags_str[0] = '\0';
  flags_str[1] = '\0';
  #ifdef PL_Q_DEBUG
  if ( flags&PL_Q_DEBUG )             strcat(flags_str, ",debug");
  #endif
  #ifdef PL_Q_DETERMINITIC
  if ( flags&PL_Q_DETERMINISTIC )     strcat(flags_str, ",deterministic");
  #endif
  if ( flags&PL_Q_NORMAL )            strcat(flags_str, ",normal");
  if ( flags&PL_Q_NODEBUG )           strcat(flags_str, ",nodebug");
  if ( flags&PL_Q_CATCH_EXCEPTION )   strcat(flags_str, ",catch_exception");
  if ( flags&PL_Q_PASS_EXCEPTION )    strcat(flags_str, ",pass_exception");
  if ( flags&PL_Q_ALLOW_YIELD )       strcat(flags_str, ",allow_yield");
  if ( flags&PL_Q_EXT_STATUS )        strcat(flags_str, ",ext_status");
  if ( flags&XX_Q_CLEAR_RETURN_TRUE ) strcat(flags_str, ",clear_return_true");
  if ( flags&XX_Q_CLOSE_QUERY )       strcat(flags_str, ",close_query");
  if ( flags&XX_Q_EXC_TERM )          strcat(flags_str, ",exc_term");

  return PL_unify_string_chars(flags_str_t, &flags_str[1]);
}

static foreign_t
query_rc_status_str_(term_t rc_t, term_t flags_t, term_t rc_bool_t,
		     term_t status_str_t)
{ int rc, flags, rc_bool;
  const char* status_str;
  if ( !PL_get_integer_ex(rc_t, &rc) ||
       !PL_get_integer_ex(flags_t, &flags) )
    return FALSE;

  if ( flags&PL_Q_EXT_STATUS )
  { switch ( rc )
    { case PL_S_EXCEPTION: rc_bool = FALSE; status_str = "exception"; break;
      case PL_S_FALSE:     rc_bool = FALSE; status_str = "false";     break;
      case PL_S_TRUE:      rc_bool = TRUE;  status_str = "true";      break;
      case PL_S_LAST:      rc_bool = TRUE;  status_str = "last";      break;
      case PL_S_YIELD:     rc_bool = TRUE;  status_str = "yield";     break;
      default:             rc_bool = FALSE; status_str = "???";       break;
    }
  } else
  { if (rc )
    { rc_bool = TRUE;
      status_str = "TRUE";
    } else
    { rc_bool = FALSE;
      status_str = "FALSE";
    }
  }

  return PL_unify_bool(rc_bool_t, rc_bool) &&
    PL_unify_string_chars(status_str_t, status_str);
}

/** ffi_call_exc_(+Goal, +Flags, Exc_0, Exc_qid, Exc_0_2, NextRc)
   where Flags is an integer (see flags/2 for constructing this)
   and Exc_0, Exc_qid are unified with the string form of
   PL_exception(0), PL_exception(qid) after PL_next_solution() and
   Exc_0_2 is unified with the string form of PL_exception(0) after
   PL_cut_query() [in all cases, only if the exception isn't 0].
   - if XX_Q_EXC_ERM, then Exc_0, Exc_qid, Exc_0_2 are unified as a term
     or "<null-term>"
   The exceptions are returned as strings to get around problems
   with lifetimes of terms (probably only needed for
   PL_exception(qid), but done for all, for uniformity).  Note the
   pseudo flag XX_Q_CLEAR_RETURN_TRUE - this is for getting the
   exceptions and return code on failure or when an exception
   happens.  See the test code for examples of using it.
*/
static foreign_t
ffi_call_exc_(term_t goal, term_t flags_t,
              term_t exc_0_t, term_t exc_qid_t, term_t exc_0_2_t,
              term_t next_rc_t)
{ /* Do not cache call_pred because the "current module" could
     be different with each call */
  /* TODO: Using call/1 is a slightly inefficient way of doing things;
     better would be to use the functor in the goal and call it
     directly (I think) */
  predicate_t call_pred = PL_predicate("call", 1, NULL);
  int flags;
  if ( !PL_get_integer_ex(flags_t, &flags) )
    return FALSE;

  { qid_t qid;
    int next_rc, cut_rc;
    qid = PL_open_query(0, flags, call_pred, goal);
    if ( !qid )
      return FALSE;
    next_rc = PL_next_solution(qid);
    if ( !PL_unify_integer(next_rc_t, next_rc) )
    { PL_close_query(qid);
      return FALSE;
    }
    { term_t exc_0 = PL_exception(0);
      term_t exc_qid = PL_exception(qid);
      if ( ! unify_term_as_term_or_string(exc_0, exc_0_t, flags&XX_Q_EXC_TERM) ||
           ! unify_term_as_term_or_string(exc_qid, exc_qid_t, flags&XX_Q_EXC_TERM) )
      { PL_close_query(qid);
        return FALSE;
      }
    }
    cut_rc = (flags&XX_Q_CLOSE_QUERY) ? PL_close_query(qid) : PL_cut_query(qid);
    { term_t exc_0_2 = PL_exception(0);
      if ( !unify_term_as_term_or_string(exc_0_2, exc_0_2_t, flags&XX_Q_EXC_TERM) )
        return FALSE;
    }
    if ( flags&XX_Q_CLEAR_RETURN_TRUE )
    { PL_clear_exception();
      return TRUE;
    }
    return next_rc && cut_rc;
  }
}

/* For debugging: unit tests can swallow debug output when there's
   a system crash, so use sdprintf_() or sdprintfnl_() instead. */
/* TODO: is this needed?   :- set_test_options([output(always)]). */
static foreign_t
sdprintf_(term_t t)
{ PL_STRINGS_MARK();
    Sdprintf("%s", ffi_term_chars(t));
  PL_STRINGS_RELEASE();
  return TRUE;
}

/* Same as sdprintf_, but with a newline */
static foreign_t
sdprintfnl_(term_t t)
{ PL_STRINGS_MARK();
    Sdprintf("%s\n", ffi_term_chars(t));
  PL_STRINGS_RELEASE();
  return TRUE;
}


/* Fake values for extern char **environ, for testing */
static const char *test_environ[] =
  {"SHELL=/bin/bash",
   "TERMCAP=",
   "PWD=/home/programmer/src/swipl-devel",
   "LANG=en_US.UTF-8",
   NULL};

/* Get the values of `test_environ` into a Prolog list, building the
   list head-to-tail. Compared to ffi_get_environ2_(), this will be
   faster if the `env` argument is instantiated, but a bit slower if
   it is uninstantiated. */
static foreign_t
ffi_get_environ1_(term_t env)
{ term_t tail = PL_copy_term_ref(env);
  term_t item = PL_new_term_ref();

  // test_environ is used here instead of `extern char **environ`
  for(const char **e = test_environ; *e; e++)
  { if ( !PL_unify_list(tail, item, tail) ||
         !PL_unify_atom_chars(item, *e) )
      return FALSE;
  }
  return PL_unify_nil(tail);
}

/* This builds the list tail-to-head and then unifies it with the
   argument `env`. This will be slightly faster than
   ffi_get_environ1_() `env` is uninstantiated, but slightly slower
   otherwise. */
static foreign_t
ffi_get_environ2_(term_t env)
{ term_t item = PL_new_term_ref();
  term_t l = PL_new_term_ref();

  PL_put_nil(l);
  // test_environ is used here instead of `extern char **environ`
  int n;
  for(n = 0; test_environ[n]; n++) { } // position to end
  while( --n >= 0 )
  { if ( !PL_put_atom_chars(item, test_environ[n]) ||
         !PL_cons_list(l, item, l) )
      return FALSE;
  }
  return PL_unify(l, env);
}

static foreign_t
ffi_write_atoms_(term_t Stream, term_t l)
{ term_t head = PL_new_term_ref();   /* the elements */
  term_t tail = PL_copy_term_ref(l); /* copy (we modify tail) */
  IOSTREAM* stream;
  int rc = TRUE;

  if ( !PL_get_stream(Stream, &stream, SIO_OUTPUT) )
    return FALSE;

  while( rc && PL_get_list_ex(tail, head, tail) )
  { PL_STRINGS_MARK();
      char *s;
      if ( (rc=PL_get_chars(head, &s, CVT_ATOM|REP_UTF8|CVT_EXCEPTION)) )
        Sfprintf(stream, "%s\n", s);
    PL_STRINGS_RELEASE();
  }

  return ( PL_release_stream(stream) &&
	   rc &&
	   PL_get_nil_ex(tail) );
}


static foreign_t
ffi_write_int32_(term_t Stream, term_t i)
{  int32_t v;
  if ( ! PL_cvt_i_int32(i, &v) )
    return FALSE;

  IOSTREAM* stream;
  if ( !PL_get_stream(Stream, &stream, SIO_OUTPUT) )
    return FALSE;

  PL_qlf_put_int32(v, stream);
  return PL_release_stream(stream);
}

static foreign_t
ffi_read_int32_(term_t Stream, term_t i)
{ IOSTREAM* stream;
  int32_t v;

  if ( !PL_get_stream(Stream, &stream, SIO_OUTPUT) )
    return FALSE;

  PL_qlf_get_int32(stream, &v);

  int rc = PL_unify_integer(i, v);
  return PL_release_stream(stream) && rc;
}

static foreign_t
ffi_write_int64_(term_t Stream, term_t i)
{ int64_t v;
  if ( ! PL_cvt_i_int64(i, &v) )
    return FALSE;

  IOSTREAM* stream;
  if ( !PL_get_stream(Stream, &stream, SIO_OUTPUT) )
    return FALSE;

  PL_qlf_put_int64(v, stream);
  return PL_release_stream(stream);
}

static foreign_t
ffi_read_int64_(term_t Stream, term_t i)
{ IOSTREAM* stream;
  if ( !PL_get_stream(Stream, &stream, SIO_OUTPUT) )
    return FALSE;

  int64_t v;
  PL_qlf_get_int64(stream, &v);

  int rc = PL_unify_int64(i, v);
  return PL_release_stream(stream) && rc;
}

static foreign_t
throw_instantiation_error_ffi(term_t culprit)
{ return PL_instantiation_error(culprit);
}

static foreign_t
throw_uninstantiation_error_ffi(term_t culprit)
{ return PL_uninstantiation_error(culprit);
}

static foreign_t
throw_representation_error_ffi(term_t resource)
{ char *resource_s;
  if ( !PL_get_atom_chars(resource, &resource_s) )
    return FALSE;
  return PL_representation_error(resource_s);
}

static foreign_t
throw_type_error_ffi(term_t expected, term_t culprit)
{ char *expected_s;
  if ( !PL_get_atom_chars(expected, &expected_s) )
    return FALSE;
  return PL_type_error(expected_s, culprit);
}

static foreign_t
throw_domain_error_ffi(term_t expected, term_t culprit)
{ char *expected_s;
  if ( !PL_get_atom_chars(expected, &expected_s) )
    return FALSE;
  return PL_domain_error(expected_s, culprit);
}

static foreign_t
throw_existence_error_ffi(term_t type, term_t culprit)
{ char *type_s;
  if ( !PL_get_atom_chars(type, &type_s) )
    return FALSE;
  return PL_existence_error(type_s, culprit);
}

static foreign_t
throw_permission_error_ffi(term_t operation,
                           term_t type, term_t culprit)
{ char *operation_s, *type_s;
  if ( !PL_get_atom_chars(operation, &operation_s) ||
       !PL_get_atom_chars(type, &type_s) )
    return FALSE;
  return PL_permission_error(operation_s, type_s, culprit);
}

static foreign_t
throw_resource_error_ffi(term_t resource)
{ char *resource_s;
  if ( !PL_get_atom_chars(resource, &resource_s) )
    return FALSE;
  return PL_resource_error(resource_s);
}


/* TODO: remove - this is for debugging int_info/2 in test_cpp.cpp */
static foreign_t
int_info_ffi(term_t name_a, term_t i1_a, term_t i2_a, term_t i3_a, term_t tv)
{ char *name;
  int i1, i2, i3;
  if ( !PL_get_atom_chars(name_a, &name) ||
       !PL_get_integer_ex(i1_a, &i1) ||
       !PL_get_integer_ex(i2_a, &i2) ||
       !PL_get_integer_ex(i3_a, &i3) )
    return FALSE;
  term_t name_t = PL_new_term_ref();
  term_t i1_t = PL_new_term_ref();
  term_t i2_t = PL_new_term_ref();
  term_t i3_t = PL_new_term_ref();
  if ( !PL_put_atom_chars(name_t, name) ||
       !PL_put_int64(i1_t, (int64_t)i1) ||
       !PL_put_int64(i2_t, (int64_t)i2) ||
       !PL_put_int64(i3_t, (int64_t)i3) )
    return FALSE;
  term_t a0 = PL_new_term_refs(4);
  if ( !a0 ||
       !PL_put_term(a0+0, name_t) ||
       !PL_put_term(a0+1, i1_t) ||
       !PL_put_term(a0+2, i2_t) ||
       !PL_put_term(a0+3, i3_t) )
    return FALSE;
  functor_t f = PL_new_functor(PL_new_atom("int_info"), 4);
  assert(f != 0);
  term_t c = PL_new_term_ref();
  if ( !PL_cons_functor_v(c, f, a0) )
    return FALSE;
  return PL_unify(c, tv);
}


/* These are used for testing install/uninstall */
static char* range_ffi_str;
#define RANGE_FFI_STR_LEN 100
#define RANGE_FFI_STR_CONTENTS "RANGE_FFI"


install_t
install_test_ffi(void)
{ PL_register_foreign("range_ffi", 3, range_ffi, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("range_ffialloc", 3, range_ffialloc, PL_FA_NONDETERMINISTIC);
  range_ffi_str = malloc(RANGE_FFI_STR_LEN);
  assert(range_ffi_str);
  strncpy(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN);
  assert(0 == strncmp(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN));
  DEBUG(Sdprintf("install_range_test_ffi %s\n", range_ffi_str));

  PL_register_foreign("w_atom_ffi_",  2, w_atom_ffi_,   0);
  PL_register_foreign("atom_ffi_",    2, atom_ffi_,     0);
  PL_register_foreign("ffi_options",  2, ffi_options_,  0);
  PL_register_foreign("ffi_call_exc", 6, ffi_call_exc_, 0); /* TODO: PL_FA_META */
  PL_register_foreign("sdprintf",     1, sdprintf_,     0);
  PL_register_foreign("sdprintfnl",   1, sdprintfnl_,   0);
  PL_register_foreign("query_flags_str",     2, query_flags_str_,     0);
  PL_register_foreign("query_rc_status_str", 4, query_rc_status_str_, 0);
  PL_register_foreign("ffi_get_environ1", 1, ffi_get_environ1_, 0);
  PL_register_foreign("ffi_get_environ2", 1, ffi_get_environ2_, 0);
  PL_register_foreign("ffi_write_atoms",  2, ffi_write_atoms_,  0);
  PL_register_foreign("ffi_write_int32",  2, ffi_write_int32_, 0);
  PL_register_foreign("ffi_read_int32",   2, ffi_read_int32_, 0);
  PL_register_foreign("ffi_write_int64",  2, ffi_write_int64_, 0);
  PL_register_foreign("ffi_read_int64",   2, ffi_read_int64_, 0);
  PL_register_foreign("throw_instantiation_error_ffi",   1, throw_instantiation_error_ffi, 0);
  PL_register_foreign("throw_uninstantiation_error_ffi", 1, throw_uninstantiation_error_ffi, 0);
  PL_register_foreign("throw_representation_error_ffi",  1, throw_representation_error_ffi, 0);
  PL_register_foreign("throw_type_error_ffi",            2, throw_type_error_ffi, 0);
  PL_register_foreign("throw_domain_error_ffi",          2, throw_domain_error_ffi, 0);
  PL_register_foreign("throw_existence_error_ffi",       2, throw_existence_error_ffi, 0);
  PL_register_foreign("throw_permission_error_ffi",      3, throw_permission_error_ffi, 0);
  PL_register_foreign("throw_resource_error_ffi",        1, throw_resource_error_ffi, 0);
  PL_register_foreign("int_info_ffi",                    5, int_info_ffi, 0);
}

install_t
uninstall_test_ffi(void)
{ /* If run with ASAN, this also tests that cleanup is done */
  #ifdef O_DEBUG
    Sdprintf("uninstall_range_test_ffi %s\n", range_ffi_str);
  #endif
  assert(0 == strncmp(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN));
  free(range_ffi_str);
}
