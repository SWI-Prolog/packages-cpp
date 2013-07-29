/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      Vu University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef _SWI_CPP_H
#define _SWI_CPP_H

#include <SWI-Prolog.h>
#include <string.h>
#include <wchar.h>
#ifndef __APPLE__
#include <malloc.h>
#endif

#ifdef __BORLANDC__
#define __inline inline
#endif

#ifdef _MSC_VER			/* MSVC doesn't know throw doesn't return */
#define PL_THROWN(value)	return value;
#else
#define PL_THROWN(v)		(void)0
#endif

class PlTerm;
class PlTermv;

		 /*******************************
		 *	 PROLOG CONSTANTS	*
		 *******************************/

class PlFunctor
{
public:
  functor_t functor;

  PlFunctor(const char *name, int arity)
  { functor = PL_new_functor(PL_new_atom(name), arity);
  }
  PlFunctor(const wchar_t *name, int arity)
  { functor = PL_new_functor(PL_new_atom_wchars(wcslen(name), name), arity);
  }
};


class PlAtom
{
public:
  atom_t handle;

  PlAtom(atom_t h)
  { handle = h;
  }
  PlAtom(const char *text)
  { handle = PL_new_atom(text);
  }
  PlAtom(const wchar_t *text)
  { handle = PL_new_atom_wchars(wcslen(text), text);
  }
  PlAtom(const PlTerm &t);

  operator const char *(void)
  { return PL_atom_chars(handle);
  }
  operator const wchar_t *(void)
  { return PL_atom_wchars(handle, NULL);
  }

  int operator ==(const char *s)
  { return strcmp(s, PL_atom_chars(handle)) == 0;
  }
  int operator ==(const wchar_t *s)
  { return wcscmp(s, PL_atom_wchars(handle, NULL)) == 0;
  }
  int operator ==(const PlAtom &a)
  { return handle == a.handle;
  }
};

		 /*******************************
		 *     GENERIC PROLOG TERM	*
		 *******************************/


class PlTerm
{
public:
  term_t ref;

  PlTerm();
  PlTerm(term_t t)
  { ref = t;
  }

					/* C --> PlTerm */
  PlTerm(const char *text);
  PlTerm(const wchar_t *text);
  PlTerm(long val);
  PlTerm(double val);
  PlTerm(const PlAtom &a);
  PlTerm(void *ptr);

					/* PlTerm --> C */
  operator term_t(void) const
  { return ref;
  }
  operator char *(void) const;
  operator wchar_t *(void) const;
  operator long(void) const;
  operator int(void) const;
  operator double(void) const;
  operator PlAtom(void) const;
  operator void *(void) const;

  int type()
  { return PL_term_type(ref);
  }

					/* Compounds */
  PlTerm operator [](int index) const;
  int arity();
  const char *name();

					/* UNIFY */
  int operator =(const PlTerm &t2);	/* term */
  int operator =(const PlAtom &a);	/* atom */
  int operator =(const char *v);	/* atom (from char *) */
  int operator =(const wchar_t *v);	/* atom (from wchar_t *) */
  int operator =(long v);		/* integer */
  int operator =(int v);		/* integer */
  int operator =(double v);		/* float */
  int operator =(const PlFunctor &f);	/* functor */

					/* Comparison standard order terms */
  int operator ==(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) == 0;
  }
  int operator !=(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) != 0;
  }
  int operator <(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) < 0;
  }
  int operator >(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) > 0;
  }
  int operator <=(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) <= 0;
  }
  int operator >=(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) >= 0;
  }
					/* comparison (long) */
  int operator ==(long v);
  int operator !=(long v);
  int operator <(long v);
  int operator >(long v);
  int operator <=(long v);
  int operator >=(long v);

					/* comparison (string) */
  int operator ==(const char *s);
  int operator ==(const wchar_t *s);
  int operator ==(const PlAtom &a);
};


		 /*******************************
		 *	   TERM VECTOR		*
		 *******************************/

class PlTermv
{
public:
  term_t a0;
  int    size;

  PlTermv(int n)
  { a0   = PL_new_term_refs(n);
    size = n;
  }
  PlTermv(int n, term_t t0)
  { a0   = t0;
    size = n;
  }

					/* create from args */
  PlTermv(PlTerm m0);
  PlTermv(PlTerm m0, PlTerm m1);
  PlTermv(PlTerm m0, PlTerm m1, PlTerm m2);
  PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3);
  PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3, PlTerm m4);

  PlTerm operator [](int n) const;
};

		 /*******************************
		 *	 SPECIALISED TERMS	*
		 *******************************/

class PlCompound : public PlTerm
{
public:

  PlCompound(const char *text);
  PlCompound(const wchar_t *text);
  PlCompound(const char *functor, const PlTermv &args);
  PlCompound(const wchar_t *functor, const PlTermv &args);
};


class PlString : public PlTerm
{
public:

  PlString(const char *text);
  PlString(const char *text, size_t len);
  PlString(const wchar_t *text);
  PlString(const wchar_t *text, size_t len);
};


class PlCodeList : public PlTerm
{
public:

  PlCodeList(const char *text);
  PlCodeList(const wchar_t *text);
};


class PlCharList : public PlTerm
{
public:

  PlCharList(const char *text);
  PlCharList(const wchar_t *text);
};


		 /*******************************
		 *	      EXCEPTIONS	*
		 *******************************/

class PlException : public PlTerm
{
public:
  PlException()
  { term_t ex = PL_exception(0);
    if ( ex )
      ref = ex;
    else
      PL_fatal_error("No exception");
  }

  PlException(const PlTerm &t)
  { ref = t.ref;
  }

  operator const char *(void);
  operator const wchar_t *(void);

  int plThrow()
  { return PL_raise_exception(ref);
  }

  void cppThrow();
};


class PlTypeError : public PlException
{
public:

  PlTypeError(const PlTerm &t) : PlException(t) {}

  PlTypeError(const char *expected, PlTerm actual) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("type_error",
					      PlTermv(expected, actual)),
				   PlTerm())))
  {
  }
};


class PlDomainError : public PlException
{
public:

  PlDomainError(const PlTerm &t) : PlException(t) {}

  PlDomainError(const char *expected, PlTerm actual) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("domain_error",
					      PlTermv(expected, actual)),
				   PlTerm())))
  {
  }
};


class PlResourceError : public PlException
{
public:
  PlResourceError() : PlException() {}

  PlResourceError(const PlTerm &t) : PlException(t) {}

  PlResourceError(const char *resource) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("resource_error",
					      PlTermv(PlTerm(resource))),
				   PlTerm())))
  {
  }
};


class PlTermvDomainError : public PlException
{
public:

  PlTermvDomainError(int size, int n) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("domain_error",
					      PlTermv(PlCompound("argv",
								 size),
						      PlTerm((long)n))),
				   PlTerm())))
  {
  }
};


		 /*******************************
		 *     PLTERM IMPLEMENTATION	*
		 *******************************/

__inline
PlTerm::PlTerm()
{ if ( !(ref = PL_new_term_ref()) )
    throw PlResourceError();
}

__inline
PlTerm::PlTerm(const char *text)
{ if ( !(ref = PL_new_term_ref()) ||
       !PL_put_atom_chars(ref, text) )
    throw PlResourceError();
}

__inline
PlTerm::PlTerm(const wchar_t *text)
{ if ( !(ref = PL_new_term_ref()) ||
       !PL_unify_wchars(ref, PL_ATOM, (size_t)-1, text) )
    throw PlResourceError();
}

__inline
PlTerm::PlTerm(long val)
{ if ( !(ref = PL_new_term_ref()) ||
       !PL_put_integer(ref, val) )
    throw PlResourceError();
}

__inline
PlTerm::PlTerm(double val)
{ ref = PL_new_term_ref();

  if ( !PL_put_float(ref, val) )
    throw PlResourceError();
}

__inline
PlTerm::PlTerm(const PlAtom &a)
{ if ( !(ref = PL_new_term_ref()) )
    throw PlResourceError();

  PL_put_atom(ref, a.handle);
}

__inline
PlTerm::PlTerm(void *ptr)
{ if ( !(ref = PL_new_term_ref()) ||
       !PL_put_pointer(ref, ptr) )
    throw PlResourceError();
}

		 /*******************************
		 *  SPECIALISED IMPLEMENTATIONS *
		 *******************************/

__inline
PlString::PlString(const char *text) : PlTerm()
{ if ( !PL_put_string_chars(ref, text) )
    throw PlResourceError();
}

__inline
PlString::PlString(const char *text, size_t len) : PlTerm()
{ if ( !PL_put_string_nchars(ref, len, text) )
    throw PlResourceError();
}

__inline
PlString::PlString(const wchar_t *text) : PlTerm()
{ if ( !PL_unify_wchars(ref, PL_STRING, (size_t)-1, text) )
    throw PlResourceError();
}

__inline
PlString::PlString(const wchar_t *text, size_t len) : PlTerm()
{ if ( !PL_unify_wchars(ref, PL_STRING, len, text) )
    throw PlResourceError();
}

__inline
PlCodeList::PlCodeList(const char *text) : PlTerm()
{ if ( !PL_put_list_codes(ref, text) )
    throw PlResourceError();
}

__inline
PlCharList::PlCharList(const char *text) : PlTerm()
{ if ( !PL_put_list_chars(ref, text) )
    throw PlResourceError();
}

__inline
PlCodeList::PlCodeList(const wchar_t *text) : PlTerm()
{ if ( !PL_unify_wchars(ref, PL_CODE_LIST, (size_t)-1, text) )
    throw PlResourceError();
}

__inline
PlCharList::PlCharList(const wchar_t *text) : PlTerm()
{ if ( !PL_unify_wchars(ref, PL_CHAR_LIST, (size_t)-1, text) )
    throw PlResourceError();
}


		 /*******************************
		 *             LISTS		*
		 *******************************/

class PlTail : public PlTerm
{
public:

  PlTail(const PlTerm &l)
  { if ( PL_is_variable(l.ref) || PL_is_list(l.ref) )
    { if ( !(ref = PL_copy_term_ref(l.ref)) )
	throw PlResourceError();
    } else
      throw PlTypeError("list", l.ref);
  }

					/* building */
  int append(const PlTerm &e)
  { term_t tmp, ex;

    if ( (tmp = PL_new_term_ref()) &&
	 PL_unify_list(ref, tmp, ref) &&
	 PL_unify(tmp, e.ref) )
    { PL_reset_term_refs(tmp);
      return TRUE;
    }

    if ( (ex = PL_exception(0)) )
      throw PlResourceError(ex);

    return FALSE;
  }
  int close()
  { return PL_unify_nil(ref);
  }

					/* enumerating */
  int next(PlTerm &t)
  { if ( PL_get_list(ref, t, ref) )
      return TRUE;

    if ( PL_get_nil(ref) )
      return FALSE;

    throw PlTypeError("list", ref);
    PL_THROWN(FALSE);
  }
};


		 /*******************************
		 *	     REGISTER		*
		 *******************************/


class PlRegister
{
public:

  PlRegister(const char *module, const char *name, int arity,
	    foreign_t (f)(term_t t0, int a, control_t ctx))
  { PL_register_foreign_in_module(module, name, arity, (void *)f, PL_FA_VARARGS);
  }

  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0))
  { PL_register_foreign_in_module(module, name, 1, (void *)f, 0);
  }
  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1))
  { PL_register_foreign_in_module(module, name, 2, (void *)f, 0);
  }
  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1, PlTerm a2))
  { PL_register_foreign_in_module(module, name, 3, (void *)f, 0);
  }

  // for non-deterministic calls
  PlRegister(const char *module, const char *name, int arity,
             foreign_t (f)(term_t t0, int a, control_t ctx), short flags)
  { PL_register_foreign_in_module(module, name, arity, (void *)f, flags);
  }

};


		 /*******************************
		 *	 CALLING PROLOG		*
		 *******************************/

class PlFrame
{
public:
  fid_t fid;

  PlFrame()
  { fid = PL_open_foreign_frame();
  }

  ~PlFrame()
  { PL_close_foreign_frame(fid);
  }

  void rewind()
  { PL_rewind_foreign_frame(fid);
  }
};


class PlQuery
{
public:
  qid_t qid;

  PlQuery(const char *name, const PlTermv &av)
  { predicate_t p = PL_predicate(name, av.size, "user");

    qid = PL_open_query((module_t)0, PL_Q_CATCH_EXCEPTION, p, av.a0);
    if ( !qid )
      throw PlResourceError();
  }
  PlQuery(const char *module, const char *name, const PlTermv &av)
  { atom_t ma = PL_new_atom(module);
    atom_t na = PL_new_atom(name);
    module_t m = PL_new_module(ma);
    predicate_t p = PL_pred(PL_new_functor(na, av.size), m);

    PL_unregister_atom(ma);
    PL_unregister_atom(na);

    qid = PL_open_query(m, PL_Q_CATCH_EXCEPTION, p, av.a0);
    if ( !qid )
      throw PlResourceError();
  }

  ~PlQuery()
  { PL_cut_query(qid);
  }

  int next_solution();
};


__inline int
PlCall(const char *predicate, const PlTermv &args)
{ PlQuery q(predicate, args);
  return q.next_solution();
}

__inline int
PlCall(const char *module, const char *predicate, const PlTermv &args)
{ PlQuery q(module, predicate, args);
  return q.next_solution();
}

__inline int
PlCall(const char *goal)
{ PlQuery q("call", PlTermv(PlCompound(goal)));
  return q.next_solution();
}

__inline int
PlCall(const wchar_t *goal)
{ PlQuery q("call", PlTermv(PlCompound(goal)));
  return q.next_solution();
}



		 /*******************************
		 *	    ATOM (BODY)		*
		 *******************************/

__inline
PlAtom::PlAtom(const PlTerm &t)
{ atom_t a;

  if ( PL_get_atom(t.ref, &a) )
    handle = a;
  else
    throw PlTypeError("atom", t);
}


		 /*******************************
		 *	    TERM (BODY)		*
		 *******************************/

					/* PlTerm --> C */

__inline PlTerm::operator char *(void) const
{ char *s;

  if ( PL_get_chars(ref, &s, CVT_ALL|CVT_WRITEQ|BUF_RING) )
    return s;

  throw PlTypeError("text", ref);
  PL_THROWN(NULL);
}

__inline PlTerm::operator wchar_t *(void) const
{ wchar_t *s;

  if ( PL_get_wchars(ref, NULL, &s, CVT_ALL|CVT_WRITEQ|BUF_RING) )
    return s;

  throw PlTypeError("text", ref);
  PL_THROWN(NULL);
}

__inline PlTerm::operator long(void) const
{ long v;

  if ( PL_get_long(ref, &v) )
    return v;

  throw PlTypeError("integer", ref);
  PL_THROWN(0L);
}

__inline PlTerm::operator int(void) const
{ int v;

  if ( PL_get_integer(ref, &v) )
    return v;

  throw PlTypeError("integer", ref);
  PL_THROWN(0);
}

__inline PlTerm::operator double(void) const
{ double v;

  if ( PL_get_float(ref, &v) )
    return v;

  throw PlTypeError("float", ref);
  PL_THROWN(0.0);
}

__inline PlTerm::operator PlAtom(void) const
{ atom_t v;

  if ( PL_get_atom(ref, &v) )
    return PlAtom(v);

  throw PlTypeError("atom", ref);
  PL_THROWN((atom_t)0);
}

__inline PlTerm::operator void *(void) const
{ void *ptr;

  if ( PL_get_pointer(ref, &ptr) )
    return ptr;

  throw PlTypeError("pointer", ref);
  PL_THROWN(NULL);
}

					/* compounds */

__inline PlTerm
PlTerm::operator [](int index) const
{ PlTerm t;

  if ( PL_get_arg(index, ref, t.ref) )
    return t;

  if ( !PL_is_compound(ref) )
    throw PlTypeError("compound", ref);
  else
  { if ( !PL_put_integer(t.ref, index) )
      throw PlResourceError();

    if ( index < 1 )
      throw PlDomainError("not_less_than_zero", t.ref);
    else
      throw PlDomainError("arity", t.ref); /* TBD: proper exception */
  }
  PL_THROWN((term_t)0);
}


__inline int
PlTerm::arity()
{ atom_t name;
  int arity;

  if ( PL_get_name_arity(ref, &name, &arity) )
    return arity;

  throw PlTypeError("compound", ref);
  PL_THROWN(0);
}


__inline const char *
PlTerm::name()
{ atom_t name;
  int arity;

  if ( PL_get_name_arity(ref, &name, &arity) )
    return PL_atom_chars(name);

  throw PlTypeError("compound", ref);
  PL_THROWN(NULL);
}


					/* Unification */

__inline int PlTerm::operator =(const PlTerm &t2)	/* term = term */
{ int rc = PL_unify(ref, t2.ref);
  term_t ex;

  if ( !rc && (ex=PL_exception(0)) )
    throw PlResourceError(ex);
  return rc;
}

__inline int PlTerm::operator =(const PlAtom &a)	/* term = atom */
{ int rc = PL_unify_atom(ref, a.handle);
  term_t ex;

  if ( !rc && (ex=PL_exception(0)) )
    throw PlResourceError(ex);
  return rc;
}

__inline int PlTerm::operator =(const char *v)		/* term = atom */
{ int rc = PL_unify_atom_chars(ref, v);
  term_t ex;

  if ( !rc && (ex=PL_exception(0)) )
    throw PlResourceError(ex);
  return rc;
}

__inline int PlTerm::operator =(const wchar_t *v)	/* term = atom */
{ int rc = PL_unify_wchars(ref, PL_ATOM, (size_t)-1, v);
  term_t ex;

  if ( !rc && (ex=PL_exception(0)) )
    throw PlResourceError(ex);
  return rc;
}

__inline int PlTerm::operator =(long v)
{ int rc = PL_unify_integer(ref, v);
  term_t ex;

  if ( !rc && (ex=PL_exception(0)) )
    throw PlResourceError(ex);
  return rc;
}

__inline int PlTerm::operator =(int v)
{ int rc = PL_unify_integer(ref, v);
  term_t ex;

  if ( !rc && (ex=PL_exception(0)) )
    throw PlResourceError(ex);
  return rc;
}

__inline int PlTerm::operator =(double v)
{ int rc = PL_unify_float(ref, v);
  term_t ex;

  if ( !rc && (ex=PL_exception(0)) )
    throw PlResourceError(ex);
  return rc;
}

__inline int PlTerm::operator =(const PlFunctor &f)
{ int rc = PL_unify_functor(ref, f.functor);
  term_t ex;

  if ( !rc && (ex=PL_exception(0)) )
    throw PlResourceError(ex);
  return rc;
}

					/* comparison */


__inline int PlTerm::operator ==(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 == v;

  throw PlTypeError("integer", ref);
  PL_THROWN(0);
}

__inline int PlTerm::operator !=(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 != v;

  throw PlTypeError("integer", ref);
  PL_THROWN(0);
}

__inline int PlTerm::operator <(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 < v;

  throw PlTypeError("integer", ref);
  PL_THROWN(0);
}

__inline int PlTerm::operator >(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 > v;

  throw PlTypeError("integer", ref);
  PL_THROWN(0);
}

__inline int PlTerm::operator <=(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 <= v;

  throw PlTypeError("integer", ref);
  PL_THROWN(0);
}

__inline int PlTerm::operator >=(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 >= v;

  throw PlTypeError("integer", ref);
  PL_THROWN(0);
}

				      /* comparison (string) */

__inline int PlTerm::operator ==(const char *s)
{ char *s0;

  if ( PL_get_chars(ref, &s0, CVT_ALL) )
    return strcmp(s0, s) == 0;

  throw PlTypeError("text", ref);
  PL_THROWN(0);
}

__inline int PlTerm::operator ==(const wchar_t *s)
{ wchar_t *s0;

  if ( PL_get_wchars(ref, NULL, &s0, CVT_ALL) )
    return wcscmp(s0, s) == 0;

  throw PlTypeError("text", ref);
  PL_THROWN(0);
}

__inline int PlTerm::operator ==(const PlAtom &a)
{ atom_t v;

  if ( PL_get_atom(ref, &v) )
    return v == a.handle;

  throw PlTypeError("atom", ref);
  PL_THROWN(0);
}


		 /*******************************
		 *	   COMPOUND (BODY)	*
		 *******************************/


__inline
PlCompound::PlCompound(const char *text) : PlTerm()
{ term_t t = PL_new_term_ref();

  if ( !PL_chars_to_term(text, t) )
    throw PlException(t);

  PL_put_term(ref, t);
}

__inline
PlCompound::PlCompound(const wchar_t *text) : PlTerm()
{ term_t t = PL_new_term_ref();

  if ( !PL_wchars_to_term(text, t) )
    throw PlException(t);

  PL_put_term(ref, t);
}

__inline
PlCompound::PlCompound(const char *functor, const PlTermv &args) : PlTerm()
{ if ( !PL_cons_functor_v(ref,
			  PL_new_functor(PL_new_atom(functor), args.size),
			  args.a0) )
    throw PlResourceError();
}

__inline
PlCompound::PlCompound(const wchar_t *functor, const PlTermv &args) : PlTerm()
{ if ( !PL_cons_functor_v(
	    ref,
	    PL_new_functor(PL_new_atom_wchars(wcslen(functor), functor),
			   args.size),
	    args.a0) )
    throw PlResourceError();
}

		 /*******************************
		 *	   TERMV (BODY)		*
		 *******************************/


__inline PlTermv::PlTermv(PlTerm m0)
{ size = 1;
  a0 = m0.ref;
}

__inline PlTermv::PlTermv(PlTerm m0, PlTerm m1)
{ size = 2;
  if ( !(a0 = PL_new_term_refs(2)) )
    throw PlResourceError();
  PL_put_term(a0+0, m0);
  PL_put_term(a0+1, m1);
}

__inline PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2)
{ size = 3;
  if ( !(a0 = PL_new_term_refs(3)) )
    throw PlResourceError();
  PL_put_term(a0+0, m0);
  PL_put_term(a0+1, m1);
  PL_put_term(a0+2, m2);
}

__inline PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3)
{ size = 4;
  if ( !(a0 = PL_new_term_refs(4)) )
    throw PlResourceError();
  PL_put_term(a0+0, m0);
  PL_put_term(a0+1, m1);
  PL_put_term(a0+2, m2);

  PL_put_term(a0+3, m3);
}

__inline PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2,
			  PlTerm m3, PlTerm m4)
{ size = 5;
  if ( !(a0 = PL_new_term_refs(5)) )
    throw PlResourceError();
  PL_put_term(a0+0, m0);
  PL_put_term(a0+1, m1);
  PL_put_term(a0+2, m2);
  PL_put_term(a0+3, m3);
  PL_put_term(a0+4, m4);
}


__inline PlTerm
PlTermv::operator [](int n) const
{ if ( n < 0 || n >= size )
    throw PlTermvDomainError(size, n);

  return PlTerm(a0+n);
}


		 /*******************************
		 *	EXCEPTIONS (BODY)       *
		 *******************************/

__inline PlException::operator const char *(void)
{ PlFrame fr;
#ifdef USE_PRINT_MESSAGE
  PlTermv av(2);

  av[0] = PlCompound("print_message",
		     PlTermv("error", ref));
  PlQuery q("$write_on_string", av);
  if ( q.next_solution() )
    return (char *)av[1];
#else
  PlTermv av(2);
  av[0] = PlTerm(ref);
  PlQuery q("$messages", "message_to_string", av);
  if ( q.next_solution() )
    return (char *)av[1];
#endif
  return "[ERROR: Failed to generate message.  Internal error]\n";
}


__inline PlException::operator const wchar_t *(void)
{ PlFrame fr;
#ifdef USE_PRINT_MESSAGE
  PlTermv av(2);

  av[0] = PlCompound("print_message",
		     PlTermv("error", ref));
  PlQuery q("$write_on_string", av);
  if ( q.next_solution() )
    return (wchar_t *)av[1];
#else
  PlTermv av(2);
  av[0] = PlTerm(ref);
  PlQuery q("$messages", "message_to_string", av);
  if ( q.next_solution() )
    return (wchar_t *)av[1];
#endif
  return L"[ERROR: Failed to generate message.  Internal error]\n";
}


__inline void
PlException::cppThrow()
{ term_t a = PL_new_term_ref();
  atom_t name;
  int arity;

  if ( PL_get_arg(1, ref, a) &&
       PL_get_name_arity(a, &name, &arity) )
  { const char *s = PL_atom_chars(name);

    if ( strcmp(s, "type_error") == 0 )
      throw PlTypeError(ref);
    if ( strcmp(s, "domain_error") == 0 )
      throw PlDomainError(ref);
    if ( strcmp(s, "resource_error") == 0 )
      throw PlResourceError(ref);
  }

  throw *this;
}


		 /*******************************
		 *	    QUERY (BODY)	*
		 *******************************/

__inline int
PlQuery::next_solution()
{ int rval;

  if ( !(rval = PL_next_solution(qid)) )
  { term_t ex;

    if ( (ex = PL_exception(qid)) )
      PlException(ex).cppThrow();
  }
  return rval;
}


		 /*******************************
		 *	      ENGINE		*
		 *******************************/

class PlError
{
public:
  char *message;

  PlError(const char *msg)
  { size_t len = strlen(msg)+1;
    message = new char[len];
#ifdef _MSC_VER				/* Yek */
#pragma warning( push )
#pragma warning (disable:4996)
#endif
    strncpy(message, msg, len);
#ifdef _MSC_VER
#pragma warning( pop )
#endif
  }
};


class PlEngine
{
public:

  PlEngine(int argc, char **argv)
  { if ( !PL_initialise(argc, argv) )
      throw PlError("failed to initialise");
  }

  PlEngine(char *av0)
  { int ac = 0;
    char **av = (char **)malloc(sizeof(char *) * 2);

    av[ac++] = av0;

    if ( !PL_initialise(1, av) )
      throw PlError("failed to initialise");
  }

  ~PlEngine()
  { PL_cleanup(0);
  }
};


		 /*******************************
		 *     REGISTER PREDICATES	*
		 *******************************/

#ifndef PROLOG_MODULE
#define PROLOG_MODULE (const char*)NULL
#endif

#define NAMED_PREDICATE(plname, name, arity) \
	static foreign_t \
	pl_ ## name ## __ ## arity(PlTermv PL_av); \
	static foreign_t \
	_pl_ ## name ## __ ## arity(term_t t0, int a, control_t c) \
	{ (void)a; (void)c; \
          try \
	  { \
	    return pl_ ## name ## __ ## arity(PlTermv(arity, t0)); \
	  } catch ( PlException &ex ) \
	  { return ex.plThrow(); \
	  } \
	} \
	static PlRegister _x ## name ## __ ## arity(PROLOG_MODULE, plname, arity, \
					    _pl_ ## name ## __ ## arity); \
	static foreign_t pl_ ## name ## __ ## arity(PlTermv PL_av)

#define NAMED_PREDICATE0(plname, name) \
	static foreign_t \
	pl_ ## name ## __0(void); \
	static foreign_t \
	_pl_ ## name ## __0(term_t t0, int a, control_t c) \
	{ (void)t0; (void)a; (void)c; \
          try \
	  { \
	    return pl_ ## name ## __0(); \
	  } catch ( PlException &ex ) \
	  { return ex.plThrow(); \
	  } \
	} \
	static PlRegister _x ## name ## __0(PROLOG_MODULE, plname, 0, \
					    _pl_ ## name ## __0); \
	static foreign_t pl_ ## name ## __0(void)

#define NAMED_PREDICATE_NONDET(plname, name, arity)          \
	static foreign_t \
	pl_ ## name ## __ ## arity(PlTermv PL_av, foreign_t handle);       \
	static foreign_t \
	_pl_ ## name ## __ ## arity(term_t t0, int a, control_t c) \
	{ (void)a; \
          try \
	  { \
	    return pl_ ## name ## __ ## arity(PlTermv(arity, t0), (foreign_t)c); \
	  } catch ( PlException &ex ) \
	  { return ex.plThrow(); \
	  } \
	} \
        static PlRegister _x ## name ## __ ## arity(PROLOG_MODULE, plname, arity, \
                                                    _pl_ ## name ## __ ## arity, \
                                                    PL_FA_NONDETERMINISTIC | PL_FA_VARARGS); \
	static foreign_t pl_ ## name ## __ ## arity(PlTermv PL_av, foreign_t handle)

#define PREDICATE0(name)              NAMED_PREDICATE0(#name, name)
#define PREDICATE(name, arity)        NAMED_PREDICATE(#name, name, arity)
#define PREDICATE_NONDET(name, arity) NAMED_PREDICATE_NONDET(#name, name, arity)

#define PL_A1  PL_av[0]
#define PL_A2  PL_av[1]
#define PL_A3  PL_av[2]
#define PL_A4  PL_av[3]
#define PL_A5  PL_av[4]
#define PL_A6  PL_av[5]
#define PL_A7  PL_av[6]
#define PL_A8  PL_av[7]
#define PL_A9  PL_av[8]
#define PL_A10 PL_av[9]

#ifndef PL_SAFE_ARG_MACROS
#define A1	PL_A1
#define A2	PL_A2
#define A3	PL_A3
#define A4	PL_A4
#define A5	PL_A5
#define A6	PL_A6
#define A7	PL_A7
#define A8	PL_A8
#define A9	PL_A9
#define A10	PL_A10
#endif

#endif /*_SWI_CPP_H*/
