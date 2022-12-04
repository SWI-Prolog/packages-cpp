/*  Part of SWI-Prolog

    Author:        Peter Ludemann
    E-mail:        peter.ludemann@gmail.com
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

/* This is used by test_ffi.pl */

/* This tests the C interface and not the C++ interface.
   But it was most convenient to put the test here. */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <assert.h>
#include <string.h>
#include <wctype.h>

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

/* range_ffialloc/3 is used in regression tests
   - PL_foreign_context_address() and malloc()-ed context.
*/
struct range_ctxt
{ long i;
};

static foreign_t
range_ffialloc(term_t t_low, term_t t_high, term_t t_result, control_t handle)
{ struct range_ctxt *ctxt;

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
      { long low;
	if ( !PL_get_long_ex(t_low, &low) )
	  PL_fail;
	if ( !(ctxt = malloc(sizeof *ctxt) ) )
	  return (foreign_t)PL_resource_error("memory");
	ctxt->i = low;
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

  { long high;
    if ( !PL_get_long_ex(t_high, &high) ||
	 ctxt->i >= high ||
	 !PL_unify_integer(t_result, ctxt->i) )
    { free(ctxt);
      PL_fail;
    }
    ctxt->i += 1;
    if ( ctxt->i == high )
    { free(ctxt);
      PL_succeed; /* Last result: succeed without a choice point */
    }
    PL_retry_address(ctxt); /* Succeed with a choice point */
  }
}

static char* range_ffi_str;
#define RANGE_FFI_STR_LEN 100
#define RANGE_FFI_STR_CONTENTS "RANGE_FFI"

static foreign_t
w_atom_ffi_(term_t stream, term_t t)
{ IOSTREAM* s;
  atom_t a;
  if ( !PL_get_stream(stream, &s, SIO_INPUT) ||
       !PL_get_atom_ex(t, &a) )
    return FALSE;
  PL_STRINGS_MARK();
  { size_t len;
    const pl_wchar_t *sa = PL_atom_wchars(a, &len);
    Sfprintf(s, "/%Ws/%zd", sa, len);
  }
  PL_STRINGS_RELEASE();
  return TRUE;
}

static foreign_t
atom_ffi_(term_t stream, term_t t)
{ IOSTREAM* s;
  atom_t a;
  if ( !PL_get_stream(stream, &s, SIO_INPUT) ||
       !PL_get_atom_ex(t, &a) )
    return FALSE;
  PL_STRINGS_MARK();
  { const char *sa = PL_atom_nchars(a, NULL);
    Sfprintf(s, "/%s/", sa);
  }
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
		       &quoted, &length, &callback, &token, &descr);

  if ( rc )
  { return PL_unify_term(a1,
			 PL_FUNCTOR_CHARS, "options", 5,
			   PL_BOOL,        quoted,
			   PL_INT64,       (int64_t)length,
			   PL_TERM,        callback,
			   PL_ATOM,        token,
			   PL_UTF8_STRING, descr);
  }

  PL_STRINGS_RELEASE();
  return rc;
}


static foreign_t
ffi_call_(term_t goal, term_t flags_t)
{ int flags;
  int rc;
  qid_t qid;
  predicate_t call_pred = PL_predicate("call", 1, "user");
  if ( !PL_get_integer(flags_t, &flags) )
    return FALSE;

  PL_STRINGS_MARK();
  char *s;
  size_t len;
  rc = PL_get_nchars(goal, &len, &s, CVT_ALL|CVT_WRITEQ|BUF_STACK|CVT_EXCEPTION);
  if ( rc )
  { char flags_str[500];
    flags_str[0] = '\0';
    flags_str[1] = '\0';
    // if ( flags & PL_Q_DEBUG )         strcat(flags_str, ",debug");
    // if ( flags & PL_Q_DETERMINISTIC ) strcat(flags_str, ",deterministic");
    if ( flags & PL_Q_NORMAL )           strcat(flags_str, ",normal");
    if ( flags & PL_Q_NODEBUG )          strcat(flags_str, ",nodebug");
    if ( flags & PL_Q_CATCH_EXCEPTION )  strcat(flags_str, ",catch_exception");
    if ( flags & PL_Q_PASS_EXCEPTION )   strcat(flags_str, ",pass_exception");
    if ( flags & PL_Q_ALLOW_YIELD )      strcat(flags_str, ",allow_yield");
    if ( flags & PL_Q_EXT_STATUS )       strcat(flags_str, ",ext_status");
    Sdprintf("ffi_call (%s): %s\n", flags_str+1, s);
  }
  PL_STRINGS_RELEASE();
  if ( !rc )
  { Sdprintf(" ... ffi_call PL_get_nchars rc=%d\n", rc);
    return rc;
  }
  qid = PL_open_query(0, flags, call_pred, goal);
  if ( !qid )
    { Sdprintf(" *** ffi_call open_query failed\n");
      return FALSE;
  }
  rc = PL_next_solution(qid);
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
    Sdprintf(" ... ffi_call next_solution rc=%d: %s\n", rc, status_str);
  } else
  { Sdprintf(" ... ffi_call next_solution rc=%d\n", rc);
  }
  rc = PL_cut_query(qid);
  Sdprintf(" ... fif_call cut_query rc=%d\n", rc);

  return TRUE;
}


install_t
install_test_ffi(void)
{ PL_register_foreign("range_ffi", 3, range_ffi, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("range_ffialloc", 3, range_ffialloc, PL_FA_NONDETERMINISTIC);
  range_ffi_str = malloc(RANGE_FFI_STR_LEN);
  assert(range_ffi_str);
  strncpy(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN);
  assert(0 == strncmp(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN));
  #ifdef O_DEBUG
    Sdprintf("install_range_test_ffi %s\n", range_ffi_str);
  #endif

  PL_register_foreign("w_atom_ffi_", 2, w_atom_ffi_, 0);
  PL_register_foreign("atom_ffi_", 2, atom_ffi_, 0);
  PL_register_foreign("ffi_options", 2, ffi_options_, 0);
  PL_register_foreign("ffi_call_", 2, ffi_call_, 0);
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
