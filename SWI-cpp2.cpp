/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2024, University of Amsterdam
			      VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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

/* If you wish, you can append SWI-cpp2.cpp file to SWI-pp2.h ...
   to do this, you need this definition:

   #define _SWI_CPP2_CPP_inline inline

*/

#ifndef _SWI_CPP2_CPP
#define _SWI_CPP2_CPP

#ifndef _SWI_CPP2_CPP_inline
#define _SWI_CPP2_CPP_inline
#endif

#include "SWI-cpp2.h"


_SWI_CPP2_CPP_inline
static
bool ex_is_resource_error(PlTerm ex)
{ // TODO: move the static PlFunctor to outside this function: https://github.com/SWI-Prolog/swipl-devel/issues/1155
  static PlFunctor FUNCTOR_error_2("error", 2);
  static PlFunctor FUNCTOR_resource_error_1("resource_error", 1);
  // The following doesn't check details of  the resource error; if desired
  // these can be added by ex[1][1].unify_atom(ATOM_stack), ATOM_memory, etc
  return ( ex.is_functor(FUNCTOR_error_2) &&
           ex[1].is_functor(FUNCTOR_resource_error_1) );
}


_SWI_CPP2_CPP_inline
void
PlWrap_fail(qid_t qid)
{ PlTerm ex(PL_exception(qid));
  if ( ex.not_null() )
  { // The error(resource_error(stack), _) exception is special because
    // nothing can be put on the stack, so all we can do is report failure
    // to the Prolog engine, which will take care of things.
    // This means, of course, that a user catch(PlException&) won't catch
    // this particular exception.
    if ( ex_is_resource_error(ex) )
      throw PlExceptionFail();
    const PlException ex2(ex);
    Plx_clear_exception(); // See https://swi-prolog.discourse.group/t/cpp2-exceptions/6040/66
    throw ex2;
  }
}


_SWI_CPP2_CPP_inline
void
PlEx_fail(qid_t qid)
{ PlTerm ex(PL_exception(qid));
  if ( ex.not_null() )
  { // The error(resource_error(stack), _) exception is special because
    // nothing can be put on the stack, so all we can do is report failure
    // to the Prolog engine, which will take care of things.
    // This means, of course, that a user catch(PlException&) won't catch
    // this particular exception.
    if ( ex_is_resource_error(ex) )
      throw PlExceptionFail();
    const PlException ex2(ex);
    Plx_clear_exception(); // See https://swi-prolog.discourse.group/t/cpp2-exceptions/6040/66
    throw ex2;
  } else
  { // TODO: get the name of the PL_...() function that caused the problem:
    throw PlUnknownError("False return code without exception");
  }
}


// TODO: add unit test for unrepresentable string - should throw error
_SWI_CPP2_CPP_inline
const std::string
PlTerm::get_nchars(unsigned int flags) const
{ flags &= ~static_cast<unsigned int>(BUF_STACK|BUF_MALLOC|BUF_ALLOW_STACK);
  flags |= static_cast<unsigned int>(BUF_DISCARDABLE|CVT_EXCEPTION);
  char *s = nullptr;
  size_t len = 0;
  PlStringBuffers _string_buffers;
  if ( _get_nchars(&len, &s, flags) )
    return std::string(s, len);
  PlEx_fail();
  return "<---get_nchars error --->"; // Should never get here
}

_SWI_CPP2_CPP_inline
const std::wstring
PlTerm::get_wchars(unsigned int flags) const
{ flags &= ~static_cast<unsigned int>(BUF_STACK|BUF_MALLOC|BUF_ALLOW_STACK);
  flags |= static_cast<unsigned int>(BUF_DISCARDABLE|CVT_EXCEPTION);
  pl_wchar_t *s;
  size_t len;
  PlStringBuffers _string_buffers;
  if ( _get_wchars(&len, &s, flags) )
    return std::wstring(s, len);
  PlEx_fail();
  return L"<---get_wchars error --->"; // Should never get here
}

#define _MUST_BE_TYPE(must_be_name, is_test, type_name)  \
_SWI_CPP2_CPP_inline \
void \
PlTerm::must_be_name() const \
{ if ( !is_test() ) \
    throw PlTypeError(type_name, *this); \
}

_MUST_BE_TYPE(must_be_attvar,         is_attvar,         "attvar")
_MUST_BE_TYPE(must_be_variable,       is_variable,       "variable")
_MUST_BE_TYPE(must_be_ground,         is_ground,         "ground")
_MUST_BE_TYPE(must_be_atom,           is_atom,           "atom")
_MUST_BE_TYPE(must_be_integer,        is_integer,        "integer")
_MUST_BE_TYPE(must_be_string,         is_string,         "string")
_MUST_BE_TYPE(must_be_atom_or_string, is_atom_or_string, "atom or string")
_MUST_BE_TYPE(must_be_float,          is_float,          "float")
_MUST_BE_TYPE(must_be_rational,       is_rational,       "rational")
_MUST_BE_TYPE(must_be_compound,       is_compound,       "compound")
_MUST_BE_TYPE(must_be_callable,       is_callable,       "callable")
_MUST_BE_TYPE(must_be_list,           is_list,           "list")
_MUST_BE_TYPE(must_be_dict,           is_dict,           "dict")
_MUST_BE_TYPE(must_be_pair,           is_pair,           "pair")
_MUST_BE_TYPE(must_be_atomic,         is_atomic,         "atomic")
_MUST_BE_TYPE(must_be_number,         is_number,         "number")
_MUST_BE_TYPE(must_be_acyclic,        is_acyclic,        "acyclic")

#undef _MUST_BE_TYPE

_SWI_CPP2_CPP_inline
PlModule
PlContext()
{ return PlModule(Plx_context());
}

_SWI_CPP2_CPP_inline
PlException
PlGeneralError(PlTerm inside)
{ return PlException(PlCompound("error", PlTermv(inside, PlTerm_var())));
}

_SWI_CPP2_CPP_inline
PlException
PlTypeError(const std::string& expected, PlTerm actual)
{ // See PL_type_error()
  return PlGeneralError(PlCompound("type_error",
                                   PlTermv(PlTerm_atom(expected), actual)));
}

_SWI_CPP2_CPP_inline
PlException
PlDomainError(const std::string& expected, PlTerm actual)
{ // See PL_domain_error()
  return PlGeneralError(PlCompound("domain_error",
                                   PlTermv(PlTerm_atom(expected), actual)));
}

_SWI_CPP2_CPP_inline
PlException
PlDomainError(PlTerm expected, PlTerm actual)
{ // See PL_domain_error()
  // This is used by
  //    PlDomainError(PlCompound("argv", PlTermv(PlTerm_integer(size_))), ...)
  //  for an out-of-bounds indexing error
  return PlGeneralError(PlCompound("domain_error",
                                   PlTermv(expected, actual)));
}

_SWI_CPP2_CPP_inline
PlException
PlInstantiationError(PlTerm t)
{ // See PL_instantiation_error()
  return PlGeneralError(PlCompound("instantiation_error", PlTermv(t)));
}

_SWI_CPP2_CPP_inline
PlException
PlUninstantiationError(PlTerm t)
{ // See PL_uninstantiation_error()
  return PlGeneralError(PlCompound("uninstantiation_error", PlTermv(t)));
}

_SWI_CPP2_CPP_inline
PlException
PlRepresentationError(const std::string& resource)
{ // See PL_representation_error()
  return PlGeneralError(PlCompound("representation_error", PlTermv(PlAtom(resource))));

}

_SWI_CPP2_CPP_inline
PlException
PlExistenceError(const std::string& type, PlTerm actual)
{ // See PL_existence_error()
  return PlGeneralError(PlCompound("existence_error",
                                   PlTermv(PlTerm_atom(type), actual)));
}

_SWI_CPP2_CPP_inline
PlException
PlPermissionError(const std::string& op, const std::string& type, PlTerm obj)
{ // See: Use PL_permission_error()
  return PlGeneralError(PlCompound("permission_error",
                                   PlTermv(PlTerm_atom(op), PlTerm_atom(type), obj)));
}

_SWI_CPP2_CPP_inline
PlException
PlResourceError(const std::string& resource)
{ // See PL_resource_error()
  return PlGeneralError(PlCompound("resource_error",
                                   PlTermv(PlTerm_atom(resource))));
}

_SWI_CPP2_CPP_inline
PlException
PlUnknownError(const std::string& description)
{ // For PlWrap()
  return PlGeneralError(PlCompound("unknown_error",
                                   PlTermv(PlTerm_atom(description))));
}




		 /*******************************
		 *	ATOM IMPLEMENTATION	*
		 *******************************/

_SWI_CPP2_CPP_inline
const std::string
PlAtom::mbchars(unsigned int flags) const
{ PlStringBuffers _string_buffers;
  size_t len;
  char *s;
  Plx_atom_mbchars(unwrap(), &len, &s, CVT_EXCEPTION|flags);
  return std::string(s, len);
}

_SWI_CPP2_CPP_inline
const std::wstring
PlAtom::wchars() const
{ PlStringBuffers _string_buffers;
  size_t len;
  const wchar_t *s = Plx_atom_wchars(unwrap(), &len);
  return std::wstring(s, len);
}


_SWI_CPP2_CPP_inline
bool PlBlob::write(IOSTREAM *s, int flags) const
{ if ( Sfprintf(s, "<%s>(%p", blob_t_->name, this) < 0 )
    return false;
  { bool rc = true;
    try
    { if ( !write_fields(s, flags) )
        return false;
    } PREDICATE_CATCH(rc = false)
    if ( !rc )
      return false;
  }
  if ( Sfprintf(s, ")") < 0 )
    return false;
  return true;
}

_SWI_CPP2_CPP_inline
void PlBlob::save(IOSTREAM *fd) const
{ (void)PL_warning("Cannot save reference to <%s>(%p)", blob_t_->name, this);
  throw PlFail();
}

_SWI_CPP2_CPP_inline
PlAtom PlBlob::load(IOSTREAM *fd)
{ (void)PL_warning("Cannot load reference to <%s>", blob_t_->name);
  PL_system_error("Cannot load reference to <%s>", blob_t_->name);
  return PlAtom(PlAtom::null);
}

_SWI_CPP2_CPP_inline
PlTerm PlBlob::symbol_term() const
{ if ( symbol_.not_null() )
    return PlTerm_atom(symbol_);
  return PlTerm_var();
}

_SWI_CPP2_CPP_inline
bool PlTerm::unify_blob(const PlBlob* blob) const
{ return PlTerm::unify_blob(static_cast<const void*>(blob),
                            blob->blob_size_(), blob->blob_t_);
}

_SWI_CPP2_CPP_inline
bool PlTerm::unify_blob(std::unique_ptr<PlBlob>* b) const
{ std::unique_ptr<PlBlob> blob(std::move(*b));
  // if std::move is not supported, the above can be replaced by:
  //   std:unique_ptr<PlBlob> blob;
  //   blob.swap(*b);
  if ( !unify_blob(blob.get()) )
    return false;
  (void)blob.release(); // Pass ownership to the Prolog blob (`this`)
  return true;
}



		 /*******************************
		 *	    TERM (BODY)		*
		 *******************************/

					/* PlTerm --> C */

_SWI_CPP2_CPP_inline
PlTerm
PlTerm::copy_term_ref() const
{ return PlTerm(Plx_copy_term_ref(unwrap()));
}


_SWI_CPP2_CPP_inline
void
PlTerm::as_nil() const
{ get_nil_ex();
}

_SWI_CPP2_CPP_inline
double
PlTerm::as_float() const
{ double v;
  get_float_ex(&v);
  return v;
}

_SWI_CPP2_CPP_inline
PlAtom
PlTerm::as_atom() const
{ PlAtom v(PlAtom::null);
  get_atom_ex(&v);
  return v;
}

_SWI_CPP2_CPP_inline
bool
PlTerm::eq_if_atom(PlAtom a) const
{ PlAtom v(PlAtom::null);
  return get_atom(&v) && v == a;
}

_SWI_CPP2_CPP_inline
void *
PlTerm::as_pointer() const
{ void *ptr;
  get_pointer_ex(&ptr);
  return ptr;
}

_SWI_CPP2_CPP_inline
PlRecord
PlTerm::record() const
{ return PlRecord(*this);
}

_SWI_CPP2_CPP_inline
PlTerm::PlTerm(const PlRecord& r)
  : WrappedC<term_t>(r.term().unwrap())
{ }


		 /*******************************
		 *             LISTS		*
		 *******************************/

_SWI_CPP2_CPP_inline
PlTerm_tail::PlTerm_tail(PlTerm l)
{ if ( l.is_variable() || l.is_list() )
    reset(l.copy_term_ref());
  else
    throw PlTypeError("list", l);
}

_SWI_CPP2_CPP_inline
bool
PlTerm_tail::append(PlTerm e)
{ PlTerm_var tmp;
  if ( unify_list(tmp, *this) &&
       tmp.unify_term(e) )
  { tmp.reset_term_refs();
    return true;
  }

  return false;
}

_SWI_CPP2_CPP_inline
bool PlTerm_tail::next(PlTerm& t)
{ if ( Plx_get_list(unwrap(), t.unwrap(), unwrap()) )
    return true;

  if ( get_nil() )
    return false;

  throw PlTypeError("list", *this);
}

_SWI_CPP2_CPP_inline
bool
PlRewindOnFail(std::function<bool()> f)
{ PlFrame frame;
  bool rc = f();
  if ( !rc )
    frame.discard();  // Same as frame.rewind(); destructor's frame.close()
  return rc;
}

_SWI_CPP2_CPP_inline
PlQuery
PlCurrentQuery()
{ return PlQuery(Plx_current_query());
}

_SWI_CPP2_CPP_inline
int
PlCall(const std::string& predicate, const PlTermv& args, int flags /* = PL_Q_PASS_EXCEPTION */ )
{ PlQuery q(predicate, args, flags);
  return q.next_solution();
}

_SWI_CPP2_CPP_inline
int
PlCall(const std::string& module, const std::string& predicate, const PlTermv& args, int flags /* = PL_Q_PASS_EXCEPTION */ )
{ PlQuery q(module, predicate, args, flags);
  return q.next_solution();
}

_SWI_CPP2_CPP_inline
int
PlCall(const std::string& goal, int flags /* = PL_Q_PASS_EXCEPTION */ )
{ PlQuery q("call", PlTermv(PlCompound(goal)), flags);
  return q.next_solution();
}

_SWI_CPP2_CPP_inline
int
PlCall(const std::wstring& goal, int flags /* = PL_Q_PASS_EXCEPTION */)
{ PlQuery q("call", PlTermv(PlCompound(goal)), flags);
  return q.next_solution();
}

_SWI_CPP2_CPP_inline
int
PlCall(PlTerm goal, int flags /* = PL_Q_PASS_EXCEPTION */ )
{ PlQuery q("call", PlTermv(goal), flags);
  return q.next_solution();
}



					/* compounds */

_SWI_CPP2_CPP_inline
PlTerm
PlTerm::operator [](size_t index) const
{ PlTerm t;

  if ( Plx_get_arg(index, unwrap(), t.unwrap()) )
    return t;

  if ( !is_compound() )
    throw PlTypeError("compound", *this);

  /* Construct error term and throw it */
  Plx_put_uint64(t.unwrap(), index);
  if ( index < 1 )
    throw PlDomainError("not_less_than_zero", t);
  else
    throw PlDomainError("arity", t); /* TODO: arity(t.unwrap()) - see PlTermv::operator[] */
}

_SWI_CPP2_CPP_inline
size_t
PlTerm::arity() const
{ size_t arity;
  if ( get_name_arity(nullptr, &arity) )
    return arity;
  throw PlTypeError("compound", *this);
}

_SWI_CPP2_CPP_inline
PlAtom
PlTerm::name() const
{ atom_t name;
  size_t arity;
  if ( Plx_get_name_arity(unwrap(), &name, &arity) )
    return PlAtom(name);
  throw PlTypeError("compound", *this);
}

_SWI_CPP2_CPP_inline
bool
PlTerm::name_arity(PlAtom *name, size_t *arity) const
{ atom_t name_a;
  if ( Plx_get_name_arity(unwrap(), &name_a, arity) )
  { if ( name )
      *name = PlAtom(name_a);
    return true;
  }
  return false;
}



					/* comparison */

_SWI_CPP2_CPP_inline
bool
PlTerm::operator ==(int64_t v) const
{ int64_t v0;
  get_int64_ex(&v0);
  return v0 == v;
}

_SWI_CPP2_CPP_inline
bool
PlTerm::operator !=(int64_t v) const
{ int64_t v0;
  get_int64_ex(&v0);
  return v0 != v;
}

_SWI_CPP2_CPP_inline
bool
PlTerm::operator <(int64_t v) const
{ int64_t v0;
  get_int64_ex(&v0);
  return  v0 < v;
}

_SWI_CPP2_CPP_inline
bool
PlTerm::operator >(int64_t v) const
{ int64_t v0;
  get_int64_ex(&v0);
  return v0 > v;
}

_SWI_CPP2_CPP_inline
bool
PlTerm::operator <=(int64_t v) const
{ int64_t v0;
  get_int64_ex(&v0);
  return v0 <= v;
}

_SWI_CPP2_CPP_inline
bool
PlTerm::operator >=(int64_t v) const
{ int64_t v0;
  get_int64_ex(&v0);
  return v0 >= v;
}

				      /* comparison (string) */

_SWI_CPP2_CPP_inline
bool
PlTerm::eq(const char *s) const
{ char *s0;

  if ( get_chars(&s0, CVT_ALL) )
    return strcmp(s0, s) == 0;

  throw PlTypeError("text", *this);
}

_SWI_CPP2_CPP_inline
bool
PlTerm::eq(const wchar_t *s) const
{ wchar_t *s0;

  if ( Plx_get_wchars(unwrap(), nullptr, &s0, CVT_ALL) )
    return wcscmp(s0, s) == 0;

  throw PlTypeError("text", *this);
}

_SWI_CPP2_CPP_inline
bool
PlTerm::eq(const std::string& s) const
{ char *s0;

  // Doesn't handle non-NUL terminated - but it's only used by deprecated operator ==
  if ( get_chars(&s0, CVT_ALL) )
    return s.compare(s0) == 0; 

  throw PlTypeError("text", *this);
}

_SWI_CPP2_CPP_inline
bool
PlTerm::eq(const std::wstring& s) const
{ // Doesn't handle non-NUL terminated - but it's only used by deprecated operator ==
  return s.compare(get_wchars(CVT_ALL)) == 0;

  throw PlTypeError("text", *this);
}

_SWI_CPP2_CPP_inline
bool
PlTerm::eq(PlAtom a) const
{ atom_t v;

  if ( Plx_get_atom(unwrap(), &v) )
    return v == a.unwrap();

  throw PlTypeError("atom", *this);
}


		 /*******************************
		 *	   COMPOUND (BODY)	*
		 *******************************/

_SWI_CPP2_CPP_inline
PlCompound::PlCompound(const wchar_t *text)
{ term_t t = Plx_new_term_ref();
  if ( !Plx_wchars_to_term(text, t) )
    throw PlException(PlTerm(t));
  Plx_put_term(unwrap(), t);
}

_SWI_CPP2_CPP_inline
PlCompound::PlCompound(const std::string& text, PlEncoding enc)
{ term_t t = Plx_new_term_ref();
  PlEx<bool>(t != (term_t)0);

  // TODO: PL_put_term_from_chars() should take an unsigned int flags
  PlEx<int>(Plx_put_term_from_chars(t, static_cast<int>(enc)|CVT_EXCEPTION, text.size(), text.data()));
  Plx_put_term(unwrap(), t);
}

_SWI_CPP2_CPP_inline
PlCompound::PlCompound(const std::wstring& text)
{  term_t t = Plx_new_term_ref();
  PlEx<bool>(t != (term_t)0);

  // TODO: what is wchar_t equivalent of PL_put_term_from_chars()?
  if ( !Plx_wchars_to_term(text.c_str(), t) ) // TODO: use text.size()
    throw PlException(PlTerm(t));
  Plx_put_term(unwrap(), t);
}

_SWI_CPP2_CPP_inline
PlCompound::PlCompound(const char *functor, const PlTermv& args)
{ functor_t f = Plx_new_functor(Plx_new_atom(functor), args.size());
  PlEx<bool>(f != (functor_t)0);
  Plx_cons_functor_v(unwrap(), f, args.termv());
}

_SWI_CPP2_CPP_inline
PlCompound::PlCompound(const wchar_t *functor, const PlTermv& args)
{ functor_t f = Plx_new_functor(Plx_new_atom_wchars(wcslen(functor), functor), args.size());
  PlEx<bool>(f != (functor_t)0);
  Plx_cons_functor_v(unwrap(), f, args.termv());
}

_SWI_CPP2_CPP_inline
PlCompound::PlCompound(const std::string& functor, const PlTermv& args)
{ functor_t f = Plx_new_functor(Plx_new_atom_nchars(functor.size(), functor.data()), args.size());
  Plx_cons_functor_v(unwrap(), f, args.termv());
}

_SWI_CPP2_CPP_inline
PlCompound::PlCompound(const std::wstring& functor, const PlTermv& args)
{ functor_t f = Plx_new_functor(Plx_new_atom_wchars(functor.size(), functor.data()), args.size());
  Plx_cons_functor_v(unwrap(), f,  args.termv());
}

		 /*******************************
		 *         TERMV (BODY)         *
		 *******************************/

_SWI_CPP2_CPP_inline
PlTermv::PlTermv(PlAtom a)
  : size_(1),
    a0_(PlTerm_atom(a).unwrap())
{ PlEx<bool>(a0_ != (term_t)0);
}

_SWI_CPP2_CPP_inline
PlTermv::PlTermv(PlTerm m0)
  : size_(1),
    a0_(m0.unwrap())
{ // Assume that m0 is valid
}

_SWI_CPP2_CPP_inline
PlTermv::PlTermv(PlTerm m0, PlTerm m1)
  : size_(2),
    a0_(Plx_new_term_refs(2))
{ PlEx<bool>(a0_ != (term_t)0);
  Plx_put_term(a0_+0, m0.unwrap());
  Plx_put_term(a0_+1, m1.unwrap());
}

_SWI_CPP2_CPP_inline
PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2)
  : size_(3),
    a0_(Plx_new_term_refs(3))
{ PlEx<bool>(a0_ != (term_t)0);
  Plx_put_term(a0_+0, m0.unwrap());
  Plx_put_term(a0_+1, m1.unwrap());
  Plx_put_term(a0_+2, m2.unwrap());
}

_SWI_CPP2_CPP_inline
PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3)
  : size_(4),
    a0_(Plx_new_term_refs(4))
{ PlEx<bool>(a0_ != (term_t)0);
  Plx_put_term(a0_+0, m0.unwrap());
  Plx_put_term(a0_+1, m1.unwrap());
  Plx_put_term(a0_+2, m2.unwrap());
  Plx_put_term(a0_+3, m3.unwrap());
}

_SWI_CPP2_CPP_inline
PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3, PlTerm m4)
  : size_(5),
    a0_(Plx_new_term_refs(5))
{ PlEx<bool>(a0_ != (term_t)0);
  Plx_put_term(a0_+0, m0.unwrap());
  Plx_put_term(a0_+1, m1.unwrap());
  Plx_put_term(a0_+2, m2.unwrap());
  Plx_put_term(a0_+3, m3.unwrap());
  Plx_put_term(a0_+4, m4.unwrap());
}

_SWI_CPP2_CPP_inline
PlTerm
PlTermv::operator [](size_t n) const
{ if ( n >= size_ )
    throw PlDomainError(PlCompound("argv",
                                   PlTermv(PlTerm_integer(size_))),
                        PlTerm_integer(n));

  return PlTerm(a0_+n);
}


		 /*******************************
		 *	EXCEPTIONS (BODY)       *
		 *******************************/

_SWI_CPP2_CPP_inline
PlTerm
PlException::string_term() const
{ PlFrame fr;
  // Note that the result is a *term*, so it's unencoded (wchar_t
  // or equivalent) and will be encoded when it's output.
// TODO: remove USE_PRINT_MESSAGE code (obsolete)
//       - or use with_output_to(string(String), print_message(error, ...))
#ifdef USE_PRINT_MESSAGE
  PlTermv av(2);
  PlCheckFail(av[0].unify_term(PlCompound("print_message",
                                          PlTermv("error", term()))));
  PlQuery q("$write_on_string", av);
  if ( q.next_solution() )
    return av[1];
#else
  // '$messages':message_to_string(error(existence_error(procedure,unknown_predicate/1),context(system:call/1,_)), Str).
  // Str = "call/1: Unknown procedure: unknown_predicate/1"
  PlTermv av(2);
  PlCheckFail(av[0].unify_term(term()));
  PlQuery q("$messages", "message_to_string", av);
  if ( q.next_solution() )
    return av[1];
#endif
  // TODO: return term_.as_string()
  return PlTerm_string("[ERROR: Failed to generate message. Internal error]");
}

_SWI_CPP2_CPP_inline
const std::string
PlException::as_string(PlEncoding enc) const
{ // Use what_str_ to hold the string so that c_str() doesn't return
  // a pointer into the stack. Note that as_string() can cause an
  // exception (out of memory, either in generating the string or in
  // allocating the std::string) even though we specify "throw()" -
  // telling the truth "noexcept(false)" results in a compilation
  // error.
  (void)enc; // TODO: use this
  const_cast<PlException*>(this)->set_what_str();
  return what_str_;
}

_SWI_CPP2_CPP_inline
const char*
PlException::what() const throw()
{ const_cast<PlException*>(this)->set_what_str();
  return what_str_.c_str();
}

_SWI_CPP2_CPP_inline
void
PlException::set_what_str()
{ if ( what_str_.empty() )
  { // Doing a query inside a query is ... problematic:
    // TODO: const_cast<PlException*>(this)->what_str_ = string_term().as_string();
    what_str_ = term().as_string();
  }
}

_SWI_CPP2_CPP_inline
void
PlException::erase()
{ if ( term_rec_.not_null() )
    term_rec_.erase();
  term_rec_.set_null();
}

		 /*******************************
		 *	    QUERY (BODY)	*
		 *******************************/

_SWI_CPP2_CPP_inline
int
PlQuery::next_solution()
{ int rval = PL_next_solution(unwrap());

  if ( flags_ & PL_Q_EXT_STATUS )
  { // values are:
    //   PL_S_EXCEPTION, PL_S_FALSE PL_S_TRUE PL_S_LAST. PL_S_YIELD:
    return rval;
  } else
  { if ( rval )
      return rval;
  }
  // If we get here, rval is "false". The user must specifically
  // request PL_Q_CATCH_EXCEPTION; otherwise exception_qid() won't
  // give an appropriate value.
  if ( flags_ & PL_Q_CATCH_EXCEPTION )
    PlEx_fail(exception_qid());
  close_destroy();
  return rval;
}


		 /*******************************
		 *	    DEBUG		*
		 *******************************/

// This is used in SWI-cpp2-plx.h - currently its action is commented out.
// TODO: remove this when PlWrapDebug() is removed from SWI-cpp2-plx.h


#ifdef O_DEBUG
#include <SWI-Stream.h>
_SWI_CPP2_CPP_inline
void PlWrapDebug(const char*msg) {
  // Sdprintf("***PlWrapDebug %s\n", msg);
  // PL_check_stacks();
}
#endif

		 /*******************************
		 *	    PlString		*
		 *******************************/

_SWI_CPP2_CPP_inline
PlStream::PlStream(PlTerm stream, int flags)
{ Plx_get_stream(stream.unwrap(), &s_, flags);
  check_stream(); // Shouldn't happen
}

_SWI_CPP2_CPP_inline
PlStream::PlStream(IOSTREAM *s)
  : s_(Plx_acquire_stream(s))
{ check_stream(); // Shouldn't happen
}

_SWI_CPP2_CPP_inline
PlStream::~PlStream() noexcept
{ release();
}

_SWI_CPP2_CPP_inline
void PlStream::release()
{ if ( s_ )
    Plx_release_stream(s_);
  s_ = nullptr;
}

_SWI_CPP2_CPP_inline
void
PlStream::check_stream() const
{ if ( ! s_ )
    throw PlUnknownError("Stream not set");
}

/* JW: was using check_rc(), but somehown Apple Clang thinks ssize_t
   maps both to int32_t and int64_t.
 */
#define _SWI_CPP2_CPP_check_rc(rc_t, defn, call) \
_SWI_CPP2_CPP_inline \
rc_t \
PlStream::defn \
{ check_stream(); \
  rc_t rc = call; \
  if ( rc < 0 ) { release(); throw PlUnknownError("Stream error"); } \
  return rc; \
}

#define _SWI_CPP2_CPP_check_brc(rc_t, defn, call) \
_SWI_CPP2_CPP_inline \
rc_t \
PlStream::defn \
{ check_stream(); \
  rc_t rc = call; \
  if ( !rc ) { release(); throw PlUnknownError("Stream error"); } \
  return rc; \
}

#define _SWI_CPP2_CPP_nocheck(rc_t, defn, call) \
_SWI_CPP2_CPP_inline \
rc_t \
PlStream::defn \
{ check_stream(); \
  return call; \
}

_SWI_CPP2_CPP_check_rc(int, set_timeout(int tmo), Sset_timeout(s_, tmo))
_SWI_CPP2_CPP_check_rc(int, unit_size(), Sunit_size(s_))
_SWI_CPP2_CPP_nocheck(bool, canrepresent(int c), Scanrepresent(c, s_))
_SWI_CPP2_CPP_check_rc(int, putcode(int c), Sputcode(c, s_))
_SWI_CPP2_CPP_check_rc(int, getcode(), Sgetcode(s_))
_SWI_CPP2_CPP_check_rc(int, peekcode(), Speekcode(s_))
_SWI_CPP2_CPP_check_rc(int, putw(int w), Sputw(w, s_))
_SWI_CPP2_CPP_check_rc(int, getw(), Sgetw(s_))
_SWI_CPP2_CPP_nocheck(size_t, fread(void *data, size_t size, size_t elems), Sfread(data, size, elems, s_))
_SWI_CPP2_CPP_nocheck(size_t, fwrite(const void *data, size_t size, size_t elems), Sfwrite(data, size, elems, s_))
_SWI_CPP2_CPP_check_rc(int, feof(), Sfeof(s_))
_SWI_CPP2_CPP_check_rc(int, fpasteof(), Sfpasteof(s_))
_SWI_CPP2_CPP_check_rc(int, ferror(), Sferror(s_))
_SWI_CPP2_CPP_check_rc(int, seterr(int which, const char *message), Sseterr(s_, which, message))
_SWI_CPP2_CPP_check_rc(int, set_exception(term_t ex), Sset_exception(s_, ex))
_SWI_CPP2_CPP_check_rc(int, setenc(IOENC new_enc, IOENC *old_enc), Ssetenc(s_, new_enc, old_enc))
_SWI_CPP2_CPP_check_rc(int, setlocale(struct PL_locale *new_loc, struct PL_locale **old_loc), Ssetlocale(s_, new_loc, old_loc))
_SWI_CPP2_CPP_check_rc(int, flush(), Sflush(s_))
_SWI_CPP2_CPP_check_rc(int64_t, size(), Ssize(s_))
_SWI_CPP2_CPP_check_rc(int, seek(int64_t pos, int whence), Sseek64(s_, pos, whence))
_SWI_CPP2_CPP_check_rc(int64_t, tell(), Stell64(s_))
_SWI_CPP2_CPP_check_rc(int, close(), Sclose(s_))
_SWI_CPP2_CPP_check_rc(int, gcclose(int flags), Sgcclose(s_, flags))
_SWI_CPP2_CPP_check_rc(ssize_t, read_pending(char *buf, size_t limit, int flags), Sread_pending(s_, buf, limit, flags))
_SWI_CPP2_CPP_nocheck(size_t, pending(), Spending(s_))
_SWI_CPP2_CPP_check_rc(int, fputs(const char *q), Sfputs(q, s_))
_SWI_CPP2_CPP_check_rc(int, vprintf(const char *fm, va_list args), Svfprintf(s_, fm, args))
_SWI_CPP2_CPP_check_rc(int, lock(), Slock(s_))
_SWI_CPP2_CPP_check_rc(int, tryLock(), StryLock(s_))
_SWI_CPP2_CPP_check_rc(int, unlock(), Sunlock(s_))
_SWI_CPP2_CPP_check_rc(int, fileno(), Sfileno(s_))
_SWI_CPP2_CPP_check_rc(int64_t, tell64(), Stell64(s_))
_SWI_CPP2_CPP_check_rc(int, seek64(int64_t pos, int whence), Sseek64(s_, pos, whence))
_SWI_CPP2_CPP_check_rc(int, checkBOM(), ScheckBOM(s_))
_SWI_CPP2_CPP_check_rc(int, writeBOM(), SwriteBOM(s_))
_SWI_CPP2_CPP_check_brc(bool, qlf_get_int64(int64_t *ip), PL_qlf_get_int64(s_, ip))
_SWI_CPP2_CPP_check_brc(bool, qlf_get_int32(int32_t *ip), PL_qlf_get_int32(s_, ip))
_SWI_CPP2_CPP_check_brc(bool, qlf_get_uint32(uint32_t *ip), PL_qlf_get_uint32(s_, ip))
_SWI_CPP2_CPP_check_brc(bool, qlf_get_double(double *fp), PL_qlf_get_double(s_, fp))
_SWI_CPP2_CPP_check_brc(bool, qlf_get_atom(atom_t *a), PL_qlf_get_atom(s_, a))
_SWI_CPP2_CPP_check_brc(bool, qlf_put_int64(int64_t i), PL_qlf_put_int64(i, s_))
_SWI_CPP2_CPP_check_brc(bool, qlf_put_int32(int32_t i), PL_qlf_put_int32(i, s_))
_SWI_CPP2_CPP_check_brc(bool, qlf_put_uint32(uint32_t i), PL_qlf_put_uint32(i, s_))
_SWI_CPP2_CPP_check_brc(bool, qlf_put_double(double f), PL_qlf_put_double(f, s_))
_SWI_CPP2_CPP_check_brc(bool, qlf_put_atom(atom_t a), PL_qlf_put_atom(a, s_))

_SWI_CPP2_CPP_inline
void
PlStream::clearerr()
{ check_stream();
  Sclearerr(s_);
}

_SWI_CPP2_CPP_inline
char *
PlStream::gets(char *buf, int n)
{ check_stream();
  return Sfgets(buf, n, s_);
}

_SWI_CPP2_CPP_inline
void
PlStream::setbuffer(char *buf, size_t size)
{ check_stream();
  Ssetbuffer(s_, buf, size);
}

_SWI_CPP2_CPP_inline
int
PlStream::printf(const char *fm, ...)
{ va_list args;
  int rval;
  va_start(args, fm);
  rval = vprintf(fm, args);
  va_end(args);
  return rval;
}

_SWI_CPP2_CPP_inline
int
PlStream::printfX(const char *fm, ...)
{ va_list args;
  int rval;
  va_start(args, fm);
  rval = vprintf(fm, args);
  va_end(args);
  return rval;
}


#undef _SWI_CPP2_CPP_check_rc
#undef _SWI_CPP2_CPP_nocheck

#endif /*_SWI_CPP2_CPP*/
