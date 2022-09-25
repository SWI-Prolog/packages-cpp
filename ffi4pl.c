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
  { const char *sa = PL_atom_chars(a);
    Sfprintf(s, "/%s/", sa);
  }
  PL_STRINGS_RELEASE();
  return TRUE;
}


install_t
install_ffi4pl(void)
{ PL_register_foreign("range_ffi", 3, range_ffi, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("range_ffialloc", 3, range_ffialloc, PL_FA_NONDETERMINISTIC);
  range_ffi_str = malloc(RANGE_FFI_STR_LEN);
  assert(range_ffi_str);
  strncpy(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN);
  assert(0 == strncmp(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN));
  #ifdef O_DEBUG
    Sdprintf("install_range_ffi4pl %s\n", range_ffi_str);
  #endif

  PL_register_foreign("w_atom_ffi_", 2, w_atom_ffi_, 0);
  PL_register_foreign("atom_ffi_", 2, atom_ffi_, 0);
}

install_t
uninstall_ffi4pl(void)
{ /* If run with ASAN, this also tests that cleanup is done */
  #ifdef O_DEBUG
    Sdprintf("uninstall_range_ffi4pl %s\n", range_ffi_str);
  #endif
  assert(0 == strncmp(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN));
  free(range_ffi_str);
}
