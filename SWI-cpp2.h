/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2022, University of Amsterdam
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

/*********************************************************************

SWI-cpp2.h is a significant rewrite of SWI-cpp.h, taking into account
experiences with the original code. A discussion of these changes is
in https://swi-prolog.discourse.group/t/changes-to-swi-cpp-h/5601
and in the "Rational" section of the documentation
https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pl2cpp.html%27)

For porting from SWI-cpp.h to SWI-cpp2.h, please see the documentation
https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pl2cpp.html%27)

Wherever possible, SWI-cpp2.h tries to maintain backwards compatiblity
with SWI-cpp.h, but often that has not been possible due to a
combination of design choices in SWI-Prolog.h and the ways that
various compilers have implemented details of the C++ standard,
particularly integer conversions.

*********************************************************************/

#ifndef _SWI_CPP_H
#define _SWI_CPP_H

#include <SWI-Prolog.h>
#include <string.h>
#include <wchar.h>
#include <string>
#include <climits>

/* TODO: Move these PL_cvt_i_*() functions to SWI-Prolog.h */
#if INT_MAX == 0x7fffffff
#define PL_cvt_i_int32(p, c) PL_cvt_i_int(p, c)
#define PL_cvt_i_uint32(p, c) PL_cvt_i_uint(p, c)
#else
#error "Unexpected value for INT_MAX"
#endif

#if !(defined(__APPLE__) || defined(__FreeBSD__))
#include <malloc.h>
#endif

/* Define as 1 if undefined or defined as empty */
#if !defined(PL_ARITY_AS_SIZE) || (0-PL_ARITY_AS_SIZE-1)==1
#undef PL_ARITY_AS_SIZE
#define PL_ARITY_AS_SIZE 1
#endif

#ifndef ARITY_T
#if PL_ARITY_AS_SIZE
#define ARITY_T size_t
#else
#define ARITY_T int
#endif
#endif

class PlAtom;
class PlTerm;
class PlTermv;

		 /*******************************
		 * COMMON OPERATIONS (TEMPLATE) *
		 *******************************/

template <typename C_t> class WrappedC
{
public:
  C_t C_; // The wrapped value

  static const C_t null = 0;
  [[nodiscard]] bool is_null()  const { return C_ == null; }
  [[nodiscard]] bool not_null() const { return C_ != null; }

protected:
  void verify() const; // Throw exception if is_null()

public:
  WrappedC<C_t>() : C_(null) { }
  explicit WrappedC<C_t>(C_t v) : C_(v) { }
  WrappedC<C_t>(const WrappedC<C_t>&) = default;
  // WrappedC<C_t>& operator =(const WrappedC<C_t>&) = default; // deprecated/deleted in PlTerm
  operator bool() const = delete; // Use not_null() instead
  void reset() { C_ = null; }
  void reset(C_t v) { C_ = v; }
};


		 /*******************************
		 *	 PROLOG CONSTANTS	*
		 *******************************/

class PlFunctor : public WrappedC<functor_t>
{
public:
  PlFunctor() : WrappedC<functor_t>() { }
  PlFunctor(functor_t v) : WrappedC<functor_t>(v) { }
  explicit PlFunctor(const char *name, ARITY_T arity)
    : WrappedC<functor_t>(PL_new_functor(PL_new_atom(name), arity))
  { verify();
  }

  explicit PlFunctor(const wchar_t *name, ARITY_T arity)
    : WrappedC<functor_t>(PL_new_functor(PL_new_atom_wchars(wcslen(name), name), arity))
  { verify();
  }

  bool operator ==(functor_t to) = delete;
  // TODO: possibly define by: return comparing functor name, arity

  // TODO: use PlPredicate, PlModule when implemented:
  predicate_t pred(module_t m) const {
    return PL_pred(C_, m);
  }

  PlAtom name() const;

  ARITY_T arity() const {
    return PL_functor_arity_sz(C_);
  }
};


class PlAtom : public WrappedC<atom_t>
{
public:
  PlAtom() : WrappedC<atom_t>() { }
  explicit PlAtom(atom_t v) : WrappedC<atom_t>(v) { }
  explicit PlAtom(const char *text)
    : WrappedC<atom_t>(PL_new_atom(text))
  { verify();
  }
  explicit PlAtom(const wchar_t *text)
    : WrappedC<atom_t>(PL_new_atom_wchars(wcslen(text), text))
  { verify();
  }
  explicit PlAtom(const std::string& text)
    : WrappedC<atom_t>(PL_new_atom_nchars(text.size(), text.data()))
  { verify();
  }
  explicit PlAtom(const std::wstring& text)
    : WrappedC<atom_t>(PL_new_atom_wchars(text.size(), text.data()))
  { verify();
  }

  explicit PlAtom(const PlTerm& t);

  [[deprecated("use c_str()")]]   explicit operator const char *()    const { return c_str(); }
  [[deprecated("use wc_str()")]]  explicit operator const wchar_t *() const { return wc_str(); }
  [[deprecated("use string()")]]  explicit operator std::string()     const { return string(); }
  [[deprecated("use wstring()")]] explicit operator std::wstring()    const { return wstring(); }

  const char* c_str() const
  { return PL_atom_chars(C_);
  }
  const wchar_t *wc_str() const
  { return PL_atom_wchars(C_, nullptr);
  }
  const std::string string() const
  { size_t len;
    const char *s = PL_atom_nchars(C_, &len);
    return std::string(s, len);
  }
  const std::wstring wstring() const
  { size_t len;
    const wchar_t *s = PL_atom_wchars(C_, &len);
    return std::wstring(s, len);
  }

  [[nodiscard]] bool operator ==(const char *s) const
  { return strcmp(s, PL_atom_chars(C_)) == 0;
  }
  [[nodiscard]] bool operator ==(const wchar_t *s) const
  { return wcscmp(s, PL_atom_wchars(C_, nullptr)) == 0;
  }
  [[nodiscard]] bool operator ==(const std::string& s) const
  { size_t len;
    const char* s0 = PL_atom_nchars(C_, &len);
    return std::string(s0, len) == s;
  }
  [[nodiscard]] bool operator ==(const std::wstring& s) const
  { size_t len;
    const wchar_t* s0 = PL_atom_wchars(C_, &len);
    return std::wstring(s0, len) == s;
  }
  [[nodiscard]] bool operator ==(const PlAtom &a) const
  { return C_ == a.C_;
  }
  [[nodiscard]] [[deprecated("use PlAtom instead of atomt_t")]] bool operator ==(atom_t to) const
  { return C_ == to;
  }

  [[nodiscard]] bool operator !=(const char *s) const
  { return !(*this == s);
  }
  [[nodiscard]] bool operator !=(const wchar_t *s) const
  { return !(*this == s);
  }
  [[nodiscard]] bool operator !=(const PlAtom &a) const
  { return !(*this == a);
  }
  [[nodiscard]] [[deprecated("use PlAtom instead of atom_t")]] bool operator !=(atom_t to) const
  { return C_ != to;
  }

  void register_ref() const
  { PL_register_atom(C_);
  }

  void unregister_ref() const
  { PL_unregister_atom(C_);
  }

  void* blob_data(size_t *len, struct PL_blob_t **type) const
  { return PL_blob_data(C_, len, type);
  }
};

		 /*******************************
		 *     GENERIC PROLOG TERM	*
		 *******************************/


// A pseudo-exception for quick exist on failure, for use by the unify
// methods.  This is special-cased in the PREDICATE et al macros.
// Note that it is *not* a subclass of PlException.
class PlFail
{
public:
  explicit PlFail() {}
};


// Throw PlFail on failure or exception.  This exception is caught by
// the PREDICATE, which simply returns false ... if the failure was
// caused by an exception, SWI-Prolog will detect that and turn the
// failure into a Prolog exception.  Therefore, there is no need for
// calling PL_exception(0) and doing something different if there is a
// pending Prolog exception.
inline void
PlCheck(int rc)
{ if ( !rc )
    throw PlFail();
}

class PlTerm : public WrappedC<term_t>
{
protected:
  explicit PlTerm()
    : WrappedC<term_t>(PL_new_term_ref())
  { verify();
  }
  explicit PlTerm(term_t t) // See PlTerm_term_t for the public constructor
    : WrappedC<term_t>(t) {}

private:
  // Convenience methods for turning a SWI-Prolog exception into a C++
  // "throw".
  [[nodiscard]] static bool chk(int rc); // if failed due to exception, throw exception
  [[nodiscard]] static bool chk_throw(); // throw exception (either actual exception or PlFail)

public:
  PlTerm(const PlTerm&) = default;
  // PlTerm& operator =(const PlTerm&) = delete; // see below

  [[nodiscard]] int type()         const { return PL_term_type(C_); }
  [[nodiscard]] bool is_variable() const { return PL_is_variable(C_); }
  [[nodiscard]] bool is_ground()   const { return PL_is_ground(C_); }
  [[nodiscard]] bool is_atom()     const { return PL_is_atom(C_); }
  [[nodiscard]] bool is_integer()  const { return PL_is_integer(C_); }
  [[nodiscard]] bool is_string()   const { return PL_is_string(C_); }
  [[nodiscard]] bool is_float()    const { return PL_is_float(C_); }
  [[nodiscard]] bool is_rational() const { return PL_is_rational(C_); }
  [[nodiscard]] bool is_compound() const { return PL_is_compound(C_); }
  [[nodiscard]] bool is_callable() const { return PL_is_callable(C_); }
  [[nodiscard]] bool is_list()     const { return PL_is_list(C_); }
  [[nodiscard]] bool is_dict()     const { return PL_is_dict(C_); }
  [[nodiscard]] bool is_pair()     const { return PL_is_pair(C_); }
  [[nodiscard]] bool is_atomic()   const { return PL_is_atomic(C_); }
  [[nodiscard]] bool is_number()   const { return PL_is_number(C_); }
  [[nodiscard]] bool is_acyclic()  const { return PL_is_acyclic(C_); }
  [[nodiscard]] bool is_functor(const PlFunctor& f) const { return PL_is_functor(C_, f.C_); }

					/* PlTerm --> C */
  // TODO: remove operator const char* etc - ambiguous with operator bool()
  [[nodiscard]] [[deprecated("use c_str()")]]    explicit operator const char *()    const { return c_str(); }
  [[nodiscard]] [[deprecated("use wc_str()")]]   explicit operator const wchar_t *() const { return wc_str(); }
  [[nodiscard]] [[deprecated("use c_str()")]]    explicit operator std::string()     const { return string(); }
  [[nodiscard]] [[deprecated("use wc_str()")]]   explicit operator std::wstring()    const { return wstring(); }
  [[nodiscard]] [[deprecated("use as_long()")]]  explicit operator long()            const { return as_long(); }
  [[nodiscard]] [[deprecated("use as_int()")]]   explicit operator int()             const { return as_int(); }
  [[nodiscard]] [[deprecated("use uint32_t()")]] explicit operator uint32_t()        const { return uint32_t(); }
  //                                             explicit operator uint64_t()        const { return int64_t(); } // Conflicts with as_term_t()
  //                                             explicit operator size_t()          const { return size_t(); }  // Conflicts with as_term_t()
  [[nodiscard]] [[deprecated("use as_long()")]]  explicit operator double()          const { return as_float(); }
  [[nodiscard]] [[deprecated("use pointer()")]]  explicit operator void *()          const { return pointer(); }
  [[nodiscard]] [[deprecated("use atom()")]]     explicit operator PlAtom()          const { return atom(); }
  // TODO: define the integer() method for int, long, etc. using
  //       PL_cvt_i_int(), PL_cvt_i_uint(), etc.
  void integer( int32_t *v) const { *v = as_int32_t(); }
  void integer(uint32_t *v) const { *v = as_uint32_t(); }
  void integer( int64_t *v) const { *v = as_int64_t(); }
  void integer(uint64_t *v) const { *v = as_uint64_t(); }

  // All the conversion functions throw a PlTypeError exception if
  // they fail (because of the wrong Prolog type). If you want to be
  // safe, use is_XXX() first to verify the type.
  [[nodiscard]] const char *    c_str()       const;
  [[nodiscard]] const wchar_t * wc_str()      const;
  [[nodiscard]] std::string     string()      const;
  [[nodiscard]] std::wstring    wstring()     const;
  [[nodiscard]] long            as_long()     const;
  [[nodiscard]] int32_t         as_int32_t()  const;
  [[nodiscard]] uint32_t        as_uint32_t() const;
  [[nodiscard]] uint64_t        as_uint64_t() const;
  [[nodiscard]] int64_t         as_int64_t()  const;
  [[nodiscard]] size_t          as_size_t()   const;
  [[nodiscard]] int             as_int()      const;
  [[nodiscard]] unsigned        as_uint()     const;
  [[nodiscard]] unsigned long   as_ulong()    const;
  [[nodiscard]] bool            as_bool()     const; // atoms 'true', 'false'
                void            as_nil()      const;
  [[nodiscard]] double          as_float()    const;
  [[nodiscard]] double          as_double()   const { return as_float(); }
  [[nodiscard]] void *          pointer()     const;

  void nchars(size_t *len, char **s, unsigned int flags);

  [[nodiscard]] PlAtom atom() const;
  [[nodiscard]] bool get_if_atom(PlAtom *a) const
  { atom_t v;
    if (PL_get_atom(C_, &v) )
    { *a = PlAtom(v);
      return true;
    }
    return false;
  }

					/* Compounds */
  [[nodiscard]] PlTerm operator [](ARITY_T index) const;
  [[nodiscard]] ARITY_T arity() const;
  [[nodiscard]] const char *name() const;
  void name_arity(PlAtom *name, ARITY_T *arity) const;
  [[nodiscard]] PlTerm copy_term_ref() const
  { term_t t = PL_copy_term_ref(C_);
    verify();
    return PlTerm(t);
  }

  // The assignment operators have been removed because of possible
  // confusion with the standard assignment and copy operators. Also,
  // they have unusual semantics; normally an assignment operator
  // would have the form
  // PlTerm& PlTerm::operator =(const PlTerm&)
  // with implicit or explicit cast from, e.g. PlAtom to PlTerm

						/* UNIFY */
  [[deprecated("use unify_*/unify__*_check")]] [[nodiscard]] bool operator =(const PlTerm& t2)   { return unify_term(t2); }
  [[deprecated("use unify_*/unify__*_check")]] [[nodiscard]] bool operator =(const PlAtom& a)    { return unify_atom(a); }
  [[deprecated("use unify_*/unify__*_check")]] [[nodiscard]] bool operator =(const char *v)      { return unify_atom(v); }
  [[deprecated("use unify_*/unify__*_check")]] [[nodiscard]] bool operator =(const wchar_t *v)   { return unify_atom(v); }
  [[deprecated("use unify_*/unify__*_check")]] [[nodiscard]] bool operator =(long v)             { return unify_integer(v); }
  [[deprecated("use unify_*/unify__*_check")]] [[nodiscard]] bool operator =(double v)           { return unify_float(v); }
  [[deprecated("use unify_*/unify__*_check")]] [[nodiscard]] bool operator =(const PlFunctor& f) { return unify_functor(f); }

  [[nodiscard]] bool unify_term(const PlTerm& t2)        { return chk(PL_unify(C_, t2.C_)); }
  [[nodiscard]] bool unify_atom(const PlAtom& a)         { return chk(PL_unify_atom(C_, a.C_)); }
  [[nodiscard]] bool unify_atom(const char *v)           { return chk(PL_unify_atom_chars(C_, v)); }
  [[nodiscard]] bool unify_atom(const wchar_t *v)        { return chk(PL_unify_wchars(C_, PL_ATOM, static_cast<size_t>(-1), v)); }
  [[nodiscard]] bool unify_atom(const std::string& v)    { return chk(PL_unify_atom_nchars(C_, v.size(), v.data())); }
  [[nodiscard]] bool unify_atom(const std::wstring& v)   { return chk(PL_unify_wchars(C_, PL_ATOM, v.size(), v.data())); }
  [[nodiscard]] bool unify_list_codes(const char* v)     { return chk(PL_unify_list_codes(C_, v)); }
  [[nodiscard]] bool unify_list_chars(const char* v)     { return chk(PL_unify_list_chars(C_, v)); }
  [[nodiscard]] bool unify_integer(int32_t v)            { return chk(PL_unify_integer(C_, v)); }
  [[nodiscard]] bool unify_integer(uint32_t v)           { return chk(PL_unify_integer(C_, v)); }
  [[nodiscard]] bool unify_integer(int64_t v)            { return chk(PL_unify_int64(C_, v)); }
  [[nodiscard]] bool unify_integer(uint64_t v)           { return chk(PL_unify_uint64(C_, v)); }
  #if LONG_MAX == 0x7fffffff
  [[nodiscard]] bool unify_integer(long v)               { return chk(PL_unify_integer(C_, v)); }
  #endif
  [[nodiscard]] bool unify_float(double v)               { return chk(PL_unify_float(C_, v)); }
  [[nodiscard]] bool unify_string(const std::string& v)  { return chk(PL_unify_string_nchars(C_, v.size(), v.data())); }
  [[nodiscard]] bool unify_string(const std::wstring& v) { return chk(PL_unify_wchars(C_, PL_STRING, v.size(), v.data())); }
  [[nodiscard]] bool unify_functor(const PlFunctor& f)   { return chk(PL_unify_functor(C_, f.C_)); }
  [[nodiscard]] bool unify_pointer(void *ptr)            { return chk(PL_unify_pointer(C_, ptr)); }
  [[nodiscard]] bool unify_nil()                         { return chk(PL_unify_nil(C_)); }
  [[nodiscard]] bool unify_nil_ex()                      { return chk(PL_unify_nil_ex(C_)); }
  [[nodiscard]] bool unify_list(PlTerm h, PlTerm t)      { return chk(PL_unify_list(C_, h.C_, t.C_)); }
  [[nodiscard]] bool unify_list_ex(PlTerm h, PlTerm t)   { return chk(PL_unify_list_ex(C_, h.C_, t.C_)); }
  [[nodiscard]] bool unify_bool(bool val)                { return chk(PL_unify_bool(C_, val)); }
  [[nodiscard]] bool unify_bool_ex(bool val)             { return chk(PL_unify_bool_ex(C_, val)); }
  [[nodiscard]] bool unify_blob(void *blob, size_t len, PL_blob_t *type) { return chk(PL_unify_blob(C_, blob, len, type)); }
  [[nodiscard]] bool unify_chars(int flags, size_t len, const char *s)   { return chk(PL_unify_chars(C_, flags, len, s)); }

  /* unify_XXX_chk() throws PlFail on unification failure or error */
  void unify_term_check(       const PlTerm& v)       { PlCheck(unify_term(v)); }
  void unify_atom_check(       const PlAtom& v)       { PlCheck(unify_atom(v)); }
  void unify_atom_check(       const char *v)         { PlCheck(unify_atom(v)); }
  void unify_atom_check(       const wchar_t *v)      { PlCheck(unify_atom(v)); }
  void unify_atom_check(       const std::string& v)  { PlCheck(unify_atom(v)); }
  void unify_atom_check(       const std::wstring& v) { PlCheck(unify_atom(v)); }
  void unify_list_codes_check( const char* v)         { PlCheck(unify_list_codes(v)); }
  void unify_list_chars_check( const char* v)         { PlCheck(unify_list_codes(v)); }
  void unify_integer_check(          int32_t v)       { PlCheck(unify_integer(v)); }
  void unify_integer_check(          uint32_t v)      { PlCheck(unify_integer(v)); }
  void unify_integer_check(          int64_t v)       { PlCheck(unify_integer(v)); }
  void unify_integer_check(          uint64_t v)      { PlCheck(unify_integer(v)); }
  void unify_float_check(            double v)        { PlCheck(unify_float(v)); }
  void unify_string_check(     const std::string& v)  { PlCheck(unify_string(v)); }
  void unify_string_check(     const std::wstring& v) { PlCheck(unify_string(v)); }
  void unify_functor_check(    const PlFunctor& v)    { PlCheck(unify_functor(v)); }
  void unify_bool_check(             bool v)          { PlCheck(unify_bool(v)); }
  void unify_bool_ex_check(          bool v)          { PlCheck(unify_bool_ex(v)); }
  void unify_pointer_check(          void *v)         { PlCheck(unify_pointer(v)); }
  void unify_nil_check()                              { PlCheck(unify_nil()); }
  void unify_nil_ex_check()                           { PlCheck(unify_nil_ex()); }
  void unify_list_check(PlTerm h, PlTerm t)           { PlCheck(unify_list(h, t)); }
  void unify_list_ex_check(PlTerm h, PlTerm t)        { PlCheck(unify_list_ex(h, t)); }
  void unify_blob_check(void *blob, size_t len, PL_blob_t *type) { PlCheck(unify_blob(blob, len, type)); }
  void unify_chars_check(int flags, size_t len, const char *s)   { PlCheck(unify_chars(flags, len, s)); }

					/* Comparison standard order terms */
  [[nodiscard]] int compare(const PlTerm& t2) const       { return PL_compare(C_, t2.C_); }
  [[nodiscard]] bool operator == (const PlTerm& t2) const { return compare(t2) == 0; }
  [[nodiscard]] bool operator != (const PlTerm& t2) const { return compare(t2) != 0; }
  [[nodiscard]] bool operator <  (const PlTerm& t2) const { return compare(t2) <  0; }
  [[nodiscard]] bool operator >  (const PlTerm& t2) const { return compare(t2) >  0; }
  [[nodiscard]] bool operator <= (const PlTerm& t2) const { return compare(t2) <= 0; }
  [[nodiscard]] bool operator >= (const PlTerm& t2) const { return compare(t2) >= 0; }
					/* comparison (long) */
  [[nodiscard]] bool operator == (long v) const;
  [[nodiscard]] bool operator != (long v) const;
  [[nodiscard]] bool operator <  (long v) const;
  [[nodiscard]] bool operator >  (long v) const;
  [[nodiscard]] bool operator <= (long v) const;
  [[nodiscard]] bool operator >= (long v) const;

					/* comparison (atom, string) */
  [[nodiscard]] bool operator ==(const char *s) const;
  [[nodiscard]] bool operator ==(const wchar_t *s) const;
  [[nodiscard]] bool operator ==(const std::string& s) const;
  [[nodiscard]] bool operator ==(const std::wstring& s) const;
  [[nodiscard]] bool operator ==(const PlAtom& a) const;

  [[nodiscard]] bool operator !=(const char *s)         const { return !(*this == s); }
  [[nodiscard]] bool operator !=(const wchar_t *s)      const { return !(*this == s); }
  [[nodiscard]] bool operator !=(const std::string& s)  const { return !(*this == s); }
  [[nodiscard]] bool operator !=(const std::wstring& s) const { return !(*this == s); }
  [[nodiscard]] bool operator !=(const PlAtom& a)       const { return !(*this == a); }

  bool raise_exception() { return PL_raise_exception(C_); }

  static PlTerm exception(qid_t qid) { return PlTerm(PL_exception(qid)); }
};


class PlTerm_atom : public PlTerm
{
public:
  explicit PlTerm_atom(atom_t a);
  explicit PlTerm_atom(const PlAtom& a);
  explicit PlTerm_atom(const char *text);
  explicit PlTerm_atom(const wchar_t *text);
  explicit PlTerm_atom(const std::string& text);
  explicit PlTerm_atom(const std::wstring& text);
};

class PlTerm_var : public PlTerm
{
public:
  explicit PlTerm_var()
    : PlTerm() {}
};

class PlTerm_term_t : public PlTerm
{
public:
  explicit PlTerm_term_t(term_t t)
    : PlTerm(t) {}
};

class PlTerm_integer : public PlTerm
{
public:
  explicit PlTerm_integer(int32_t i);
  explicit PlTerm_integer(uint32_t i);
  explicit PlTerm_integer(int64_t i);
  explicit PlTerm_integer(uint64_t i);
  #if LONG_MAX == 0x7fffffff
  explicit PlTerm_integer(long i);
  explicit PlTerm_integer(unsigned long i);
  #endif
};

class PlTerm_float : public PlTerm
{
public:
  explicit PlTerm_float(double v);
};

class PlTerm_pointer : public PlTerm
{
public:
  explicit PlTerm_pointer(void * ptr);
};


		 /*******************************
		 *	   TERM VECTOR		*
		 *******************************/

class PlTermv
{
private:
  size_t size_;
  term_t a0_; // A vector of term_t

public:
  explicit PlTermv(size_t n)
    : size_(n),
      a0_(PL_new_term_refs(static_cast<int>(n))) {}
  explicit PlTermv(size_t n, const PlTerm& t0)
    : size_(n),
      a0_(t0.C_) {}

  term_t termv() const
  { return a0_;
  }

  size_t size() const
  { return size_;
  }

					/* create from args */
  explicit PlTermv(const PlAtom& a);
  explicit PlTermv(const PlTerm& m0);
  explicit PlTermv(const PlTerm& m0, const PlTerm& m1);
  explicit PlTermv(const PlTerm& m0, const PlTerm& m1, const PlTerm& m2);
  explicit PlTermv(const PlTerm& m0, const PlTerm& m1, const PlTerm& m2, const PlTerm& m3);
  explicit PlTermv(const PlTerm& m0, const PlTerm& m1, const PlTerm& m2, const PlTerm& m3, const PlTerm& m4);

  PlTerm operator [](size_t n) const;
};

		 /*******************************
		 *	 SPECIALISED TERMS	*
		 *******************************/

class PlCompound : public PlTerm
{
public:
  explicit PlCompound(const char *text);
  explicit PlCompound(const wchar_t *text);
  explicit PlCompound(const std::string& text);
  explicit PlCompound(const std::wstring& text);
  PlCompound(const char *functor, const PlTermv& args);
  PlCompound(const wchar_t *functor, const PlTermv& args);
  PlCompound(const std::string& functor, const PlTermv& args);
  PlCompound(const std::wstring& functor, const PlTermv& args);
};


class PlTerm_string : public PlTerm
{
public:
  PlTerm_string(const char *text);
  PlTerm_string(const char *text, size_t len);
  PlTerm_string(const wchar_t *text);
  PlTerm_string(const wchar_t *text, size_t len);
  PlTerm_string(const std::string& text);
  PlTerm_string(const std::wstring& text);
};


class PlTerm_list_codes : public PlTerm
{
public:
  PlTerm_list_codes(const char *text);
  PlTerm_list_codes(const wchar_t *text);
 // TODO: std::string, std::wstring
};


class PlTerm_list_chars : public PlTerm
{
public:
  PlTerm_list_chars(const char *text);
  PlTerm_list_chars(const wchar_t *text);
 // TODO: std::string, std::wstring
};


		 /*******************************
		 *	      EXCEPTIONS	*
		 *******************************/

class PlException : public PlTerm
{
public:
  explicit PlException()
    : PlTerm(PL_exception(0))
  { if ( is_null() )
      PL_fatal_error("No exception");
  }
  explicit PlException(const PlAtom& a)
    : PlTerm(PlTerm_atom(a).C_) {}
  explicit PlException(const PlTerm& t)
    : PlTerm(t) {}
  explicit PlException(term_t ex)
    : PlTerm(ex) {}

  // The following methods override PlTerm, but we can't use the
  // "override" keyword because the method isn't virtual and we don't
  // want the overhead of virtual methods - we want PlTerm to be a
  // thin wrapper on term_t.
  [[nodiscard]] [[deprecated("c_str")]] operator const char *() const { return c_str(); }
  [[nodiscard]] [[deprecated("wc_str")]] operator const wchar_t *() const { return wc_str(); }
  [[nodiscard]] const char *c_str() const;
  [[nodiscard]] const wchar_t *wc_str() const;

  // plThrow() is for the try-catch in PREDICATE - returns the result
  // of PL_raise_exception() as a foreign_t (which is always `false`).
  foreign_t plThrow()
  { return static_cast<foreign_t>(PL_raise_exception(C_));
  }

  // Translate some common errors into specific subclasses
  PlException adjust_for_throw() const;
};


class PlTypeError : public PlException
{
public:
  explicit PlTypeError(const PlTerm& t)
    : PlException(t) {}

  explicit PlTypeError(const char *expected, const PlTerm& actual) :
    PlException(PlCompound("error",
			   PlTermv(actual.is_variable() ?
				   static_cast<PlTerm>(PlTerm_atom("instantiation_error")) :
				   static_cast<PlTerm>(PlCompound("type_error",
								  PlTermv(PlTerm_atom(expected), actual))),
				   PlTerm_var())))
  {
  }
};

class PlDomainError : public PlException
{
public:
  explicit PlDomainError(const PlTerm& t)
    : PlException(t) {}

  explicit PlDomainError(const char *expected, const PlTerm& actual) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("domain_error",
					      PlTermv(PlTerm_atom(expected), actual)),
				   PlTerm_var())))
  {
  }
};


class PlInstantiationError : public PlException
{
public:
  explicit PlInstantiationError(const PlTerm& t) :
    PlException(t.is_variable() ?
	      PlCompound("error",
			 PlTermv(PlTerm_atom("instantiation_error"), t))
		: t) {}

  explicit PlInstantiationError() :
    PlException(PlCompound("error",
			   PlTermv(PlTerm_atom("instantiation_error"),
				   PlTerm_var())))
  {
  }
};


class PlExistenceError : public PlException
{
public:
  explicit PlExistenceError(const PlTerm& t)
    : PlException(t) {}

  explicit PlExistenceError(const char *type, PlTerm actual) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("existence_error",
					      PlTermv(PlTerm_atom(type), actual)),
				   PlTerm_var())))
  {
  }
};


class PlPermissionError : public PlException
{
public:
  explicit PlPermissionError(const PlTerm& t)
    : PlException(t) {}

  explicit PlPermissionError(const char *op, const char *type, const PlTerm& obj) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("permission_error",
					      PlTermv(PlTerm_atom(op), PlTerm_atom(type), obj)),
				   PlTerm_var())))
  {
  }
};


class PlResourceError : public PlException
{
public:
  explicit PlResourceError() : PlException() {}

  explicit PlResourceError(const PlTerm& t)
    : PlException(t) {}

  explicit PlResourceError(term_t ex)
    : PlException(ex) {}

  explicit PlResourceError(const char *resource) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("resource_error",
					      PlTermv(PlTerm_atom(resource))),
				   PlTerm_var())))
  {
  }
};


class PlTermvDomainError : public PlException
{
public:
  explicit PlTermvDomainError(size_t size, size_t n) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("domain_error",
					      PlTermv(PlCompound("argv",
								 PlTermv(PlTerm_integer(size))),
						      PlTerm_integer(n))),
				   PlTerm_var())))
  {
  }
};


		 /*******************************
		 *   PLFUNCTOR IMPLEMENTATION	*
		 *******************************/

template<typename C_t> inline void
WrappedC<C_t>::verify() const
{ if ( is_null() ) // For PlFunctor, no need to check name().is_null()
    throw PlResourceError();
}

inline PlAtom
PlFunctor::name() const
{ return PlAtom(PL_functor_name(C_));
}



		 /*******************************
		 *	ATOM IMPLEMENTATION	*
		 *******************************/

inline
PlAtom::PlAtom(const PlTerm& t)
  : WrappedC<atom_t>(t.atom().C_) {}


		 /*******************************
		 *     PLTERM IMPLEMENTATION	*
		 *******************************/

inline
PlTerm_atom::PlTerm_atom(atom_t a)
  : PlTerm(PL_new_term_ref())
{ PL_put_atom(C_, a); // always returns TRUE
}

inline
PlTerm_atom::PlTerm_atom(const PlAtom& a)
  : PlTerm(PL_new_term_ref())
{ PL_put_atom(C_, a.C_); // always returns TRUE
}

inline
PlTerm_atom::PlTerm_atom(const char *text)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_atom_chars(C_, text));
}

inline
PlTerm_atom::PlTerm_atom(const std::string& text)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_atom_nchars(C_, text.size(), text.data()));
}

inline
PlTerm_atom::PlTerm_atom(const wchar_t *text)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_unify_wchars(C_, PL_ATOM, static_cast<size_t>(-1), text));
}

inline
PlTerm_atom::PlTerm_atom(const std::wstring& text)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_unify_wchars(C_, PL_ATOM, text.size(), text.data()));
}

inline
PlTerm_integer::PlTerm_integer(int32_t val)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_integer(C_, val));
}

inline
PlTerm_integer::PlTerm_integer(uint32_t val)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_uint64(C_, val)); // There's no put_uint32 - TODO: check range?
}

inline
PlTerm_integer::PlTerm_integer(int64_t val)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_int64(C_, val));
}

inline
PlTerm_integer::PlTerm_integer(uint64_t val)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_uint64(C_, val));
}

#if LONG_MAX == 0x7fffffff
inline
PlTerm_integer::PlTerm_integer(long val)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_integer(C_, val));
}

inline
PlTerm_integer::PlTerm_integer(unsigned long val)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_uint64(C_, val)); // There's no put_uint32 - TODO: check range?
}
#endif


inline
PlTerm_float::PlTerm_float(double val)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_float(C_, val));
}

inline
PlTerm_pointer::PlTerm_pointer(void *ptr)
  : PlTerm(PL_new_term_ref())
{ PlCheck(PL_put_pointer(C_, ptr));
}


		 /*******************************
		 *	    TERM (BODY)		*
		 *******************************/

					/* PlTerm --> C */

inline const char *
PlTerm::c_str() const
{ char *s;
  if ( PL_get_chars(C_, &s, CVT_ALL|CVT_WRITEQ|BUF_STACK|CVT_EXCEPTION) )
    return s;
  throw PlException();
}

inline const wchar_t *
PlTerm::wc_str() const
{ wchar_t *s;
  if ( PL_get_wchars(C_, nullptr, &s, CVT_ALL|CVT_WRITEQ|BUF_STACK|CVT_EXCEPTION) )
    return s;
  throw PlException();
}

inline std::string
PlTerm::string() const
{ char *s;
  size_t len;
  if ( PL_get_nchars(C_, &len, &s, CVT_ALL|CVT_WRITEQ|BUF_STACK|CVT_EXCEPTION) )
    return std::string(s, len);
  throw PlException();
}

inline std::wstring
PlTerm::wstring() const
{ wchar_t *s;
  size_t len;
  if ( PL_get_wchars(C_, &len, &s, CVT_ALL|CVT_WRITEQ|BUF_STACK|CVT_EXCEPTION) )
    return std::wstring(s, len);
  throw PlException();
}

inline long
PlTerm::as_long() const
{ long v;
  if ( PL_get_long_ex(C_, &v) )
    return v;
  return chk_throw(); // always throws
}

inline int32_t
PlTerm::as_int32_t() const
{ int32_t v;
  if ( PL_get_integer_ex(C_, &v) )
    return v;
  return chk_throw(); // always throws
}

inline uint32_t
PlTerm::as_uint32_t() const
{ uint32_t v;
  if ( PL_cvt_i_uint32(C_, &v) )
    return v;
  return chk_throw(); // always throws
}

inline int64_t
PlTerm::as_int64_t() const
{ int64_t v;
  if ( PL_get_int64_ex(C_, &v) )
    return v;
  return chk_throw(); // always throws
}

inline uint64_t
PlTerm::as_uint64_t() const
{ uint64_t v;
  if ( PL_get_uint64_ex(C_, &v) )
    return v;
  return chk_throw(); // always throws
}

inline size_t
PlTerm::as_size_t() const
{ size_t v;
  if ( PL_get_size_ex(C_, &v) )
    return v;
  return chk_throw(); // always throws
}

inline int
PlTerm::as_int() const
{ int v;
  PlCheck(PL_cvt_i_int(C_, &v));
  return v;
}

inline unsigned
PlTerm::as_uint() const
{ unsigned v;
  PlCheck(PL_cvt_i_uint(C_, &v));
  return v;
}

inline unsigned long
PlTerm::as_ulong() const
{ unsigned long v;
  PlCheck(PL_cvt_i_ulong(C_, &v));
  return v;
}


inline bool
PlTerm::as_bool() const
{ int v;
  if ( PL_get_bool_ex(C_, &v) )
    return v;
  return chk_throw(); // always throws
}

inline void
PlTerm::as_nil() const
{ if ( PL_get_nil_ex(C_) )
    return;
  (void)chk_throw(); // always throws
}

inline double
PlTerm::as_float() const
{ double v;
  if ( PL_get_float_ex(C_, &v) )
    return v;
  return chk_throw(); // always throws
}

inline PlAtom
PlTerm::atom() const
{ atom_t v;
  if ( PL_get_atom_ex(C_, &v) )
    return PlAtom(v);
  (void)chk_throw(); // always throws
  return PlAtom();   // make the compiler happy
}

inline void *
PlTerm::pointer() const
{ void *ptr;
  if ( PL_get_pointer_ex(C_, &ptr) )
    return ptr;
  (void)chk_throw(); // always throws
  return nullptr;    // make the compiler happy
}

inline void
PlTerm::nchars(size_t *len, char **s, unsigned int flags)
{ if ( PL_get_nchars(C_, len, s, flags|CVT_EXCEPTION) )
    return;
  (void)chk_throw(); // always throws
}


		 /*******************************
		 *  SPECIALISED IMPLEMENTATIONS *
		 *******************************/

inline
PlTerm_string::PlTerm_string(const char *text)
{ PlCheck(PL_put_string_chars(C_, text));
}

inline
PlTerm_string::PlTerm_string(const char *text, size_t len)
{ PlCheck(PL_put_string_nchars(C_, len, text));
}

inline
PlTerm_string::PlTerm_string(const wchar_t *text)
{ PlCheck(PL_unify_wchars(C_, PL_STRING, static_cast<size_t>(-1), text));
}

inline
PlTerm_string::PlTerm_string(const wchar_t *text, size_t len)
{ PlCheck(PL_unify_wchars(C_, PL_STRING, len, text));
}

inline
PlTerm_string::PlTerm_string(const std::string& text)
{ PlCheck(PL_put_string_nchars(C_, text.size(), text.data()));
}

inline
PlTerm_string::PlTerm_string(const std::wstring& text)
{ PlCheck(PL_unify_wchars(C_, PL_STRING, text.size(), text.data()));
}

inline
PlTerm_list_codes::PlTerm_list_codes(const char *text)
{ PlCheck(PL_put_list_codes(C_, text));
}

inline
PlTerm_list_chars::PlTerm_list_chars(const char *text)
{ PlCheck(PL_put_list_chars(C_, text));
}

inline
PlTerm_list_codes::PlTerm_list_codes(const wchar_t *text)
{ PlCheck(PL_unify_wchars(C_, PL_CODE_LIST, static_cast<size_t>(-1), text));
}

inline
PlTerm_list_chars::PlTerm_list_chars(const wchar_t *text)
{ PlCheck(PL_unify_wchars(C_, PL_CHAR_LIST, static_cast<size_t>(-1), text));
}


		 /*******************************
		 *             LISTS		*
		 *******************************/

class PlTerm_tail : public PlTerm
{
public:
  explicit PlTerm_tail(const PlTerm& l)
  { if ( l.is_variable() || l.is_list() )
    { if ( !(C_ = l.copy_term_ref().C_) )
	throw PlResourceError();
    } else
      throw PlTypeError("list", l);
  }

					/* building */
  bool append(const PlTerm& e)
  { term_t tmp, ex;

    if ( (tmp = PL_new_term_ref()) &&
	 PL_unify_list(C_, tmp, C_) &&
	 PL_unify(tmp, e.C_) )
    { PL_reset_term_refs(tmp);
      return TRUE;
    }

    if ( (ex = PL_exception(0)) ) // TODO: use PlCheck()?
      throw PlResourceError(ex);

    return FALSE;
  }
  bool close()
  { return PL_unify_nil(C_);
  }

					/* enumerating */
  bool next(PlTerm& t)
  { if ( PL_get_list(C_, t.C_, C_) )
      return TRUE;

    if ( PL_get_nil(C_) )
      return FALSE;

    throw PlTypeError("list", *this);
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
  { PL_register_foreign_in_module(module, name, arity, reinterpret_cast<pl_function_t>(f), PL_FA_VARARGS);
  }

  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0))
  { PL_register_foreign_in_module(module, name, 1, reinterpret_cast<pl_function_t>(f), 0);
  }
  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1))
  { PL_register_foreign_in_module(module, name, 2, reinterpret_cast<pl_function_t>(f), 0);
  }
  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1, PlTerm a2))
  { PL_register_foreign_in_module(module, name, 3, reinterpret_cast<pl_function_t>(f), 0);
  }

  // for non-deterministic calls
  PlRegister(const char *module, const char *name, int arity,
	     foreign_t (f)(term_t t0, int a, control_t ctx), short flags)
  { PL_register_foreign_in_module(module, name, arity, reinterpret_cast<pl_function_t>(f), flags);
  }
};


		 /*******************************
		 *	 CALLING PROLOG		*
		 *******************************/

class PlFrame
{
private:
  fid_t fid_;

public:
  PlFrame()
  { fid_ = PL_open_foreign_frame();
  }

  ~PlFrame()
  { PL_close_foreign_frame(fid_);
  }

  void rewind()
  { PL_rewind_foreign_frame(fid_);
  }
};


class PlQuery
{
private:
  qid_t qid_;

public:
  PlQuery(predicate_t pred, const PlTermv& av)
    : qid_(PL_open_query(static_cast<module_t>(0), PL_Q_PASS_EXCEPTION, pred, av.termv()))
  { if ( !qid_ ) // TODO: use PlCheck()?
      throw PlResourceError();
  }
  PlQuery(const char *name, const PlTermv& av)
    : qid_(PL_open_query(static_cast<module_t>(0), PL_Q_PASS_EXCEPTION,
			 PL_predicate(name, static_cast<int>(av.size()), "user"),
			 av.termv()))
  { if ( !qid_ ) // TODO: use PlCheck()?
      throw PlResourceError();
  }
  PlQuery(const char *module, const char *name, const PlTermv& av)
  { atom_t ma = PL_new_atom(module);
    atom_t na = PL_new_atom(name);
    module_t m = PL_new_module(ma);
    predicate_t p = PL_pred(PL_new_functor(na, av.size()), m);

    PL_unregister_atom(ma);
    PL_unregister_atom(na);

    qid_ = PL_open_query(m, PL_Q_PASS_EXCEPTION, p, av.termv());
    if ( !qid_ ) // TODO: use PlCheck()?
      throw PlResourceError();
  }
  ~PlQuery()
  { if ( qid_ )
      PL_cut_query(qid_);
  }
  bool next_solution();
};


// TODO: add std::string, std::wstring versions of PlCall

inline int
PlCall(const char *predicate, const PlTermv& args)
{ PlQuery q(predicate, args);
  return q.next_solution();
}

inline int
PlCall(const char *module, const char *predicate, const PlTermv& args)
{ PlQuery q(module, predicate, args);
  return q.next_solution();
}

inline int
PlCall(const char *goal)
{ PlQuery q("call", PlTermv(PlCompound(goal)));
  return q.next_solution();
}

inline int
PlCall(const wchar_t *goal)
{ PlQuery q("call", PlTermv(PlCompound(goal)));
  return q.next_solution();
}

// TODO: PlCall(const std::string& goal)



					/* compounds */

inline PlTerm
PlTerm::operator [](ARITY_T index) const
{ PlTerm t;

  if ( PL_get_arg(index, C_, t.C_) )
    return t;

  if ( !PL_is_compound(C_) )
    throw PlTypeError("compound", *this);

  /* Construct error term and throw it */
  PlCheck(PL_put_uint64(t.C_, index));
  if ( index < 1 )
    throw PlDomainError("not_less_than_zero", t);
  else
    throw PlDomainError("arity", t); /* TODO: proper exception */
}


inline ARITY_T
PlTerm::arity() const
{ atom_t name;
  ARITY_T arity;
  if ( PL_get_name_arity(C_, &name, &arity) )
    return arity;
  throw PlTypeError("compound", *this);
}


inline const char *
PlTerm::name() const
{ atom_t name;
  ARITY_T arity;
  if ( PL_get_name_arity(C_, &name, &arity) )
    return PL_atom_chars(name);
  throw PlTypeError("compound", *this);
}

inline void
PlTerm::name_arity(PlAtom *name, ARITY_T *arity) const
{ atom_t name_a;
  if ( PL_get_name_arity(C_, &name_a, arity) )
    *name = PlAtom(name_a);
  else
    throw PlTypeError("compound", *this);
}

inline bool
PlTerm::chk(int rc)
{ if ( rc )
    return rc;
  term_t ex = PL_exception(0);
  if ( ex )
    throw PlException(ex).adjust_for_throw();
  return rc;
}

inline bool
PlTerm::chk_throw()
{ term_t ex = PL_exception(0);
  if ( ex )
    throw PlException(ex);
  throw PlFail();
  return false;
}


					/* comparison */


inline bool PlTerm::operator ==(long v) const
{ long v0;

  if ( PL_get_long(C_, &v0) )
    return v0 == v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator !=(long v) const
{ long v0;

  if ( PL_get_long(C_, &v0) )
    return v0 != v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator <(long v) const
{ long v0;

  if ( PL_get_long(C_, &v0) )
    return v0 < v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator >(long v) const
{ long v0;

  if ( PL_get_long(C_, &v0) )
    return v0 > v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator <=(long v) const
{ long v0;

  if ( PL_get_long(C_, &v0) )
    return v0 <= v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator >=(long v) const
{ long v0;

  if ( PL_get_long(C_, &v0) )
    return v0 >= v;

  throw PlTypeError("integer", *this);
}

				      /* comparison (string) */

inline bool PlTerm::operator ==(const char *s) const
{ char *s0;

  if ( PL_get_chars(C_, &s0, CVT_ALL) )
    return strcmp(s0, s) == 0;

  throw PlTypeError("text", *this);
}

inline bool PlTerm::operator ==(const wchar_t *s) const
{ wchar_t *s0;

  if ( PL_get_wchars(C_, nullptr, &s0, CVT_ALL) )
    return wcscmp(s0, s) == 0;

  throw PlTypeError("text", *this);
}

inline bool PlTerm::operator ==(const std::string& s) const
{ char *s0;

  if ( PL_get_chars(C_, &s0, CVT_ALL) )
    return s.compare(s0) == 0; // TODO: handle non-NUL terminated

  throw PlTypeError("text", *this);
}

inline bool PlTerm::operator ==(const PlAtom& a) const
{ atom_t v;

  if ( PL_get_atom(C_, &v) )
    return v == a.C_;

  throw PlTypeError("atom", *this);
}


		 /*******************************
		 *	   COMPOUND (BODY)	*
		 *******************************/

inline void
PlPutTerm(term_t to, term_t from)
{ PlCheck(PL_put_term(to, from));
}


inline
PlCompound::PlCompound(const char *text)
{ term_t t = PL_new_term_ref();

  if ( !PL_chars_to_term(text, t) )
    throw PlException(t);

  PlPutTerm(C_, t);
}

inline
PlCompound::PlCompound(const wchar_t *text)
{ term_t t = PL_new_term_ref();

  if ( !PL_wchars_to_term(text, t) )
    throw PlException(t);

  PlPutTerm(C_, t);
}

// TODO: Add option for handling UTF8 "text"
inline
PlCompound::PlCompound(const std::string& text)
{ term_t t = PL_new_term_ref();

  if ( !PL_put_term_from_chars(t, REP_ISO_LATIN_1, text.size(), text.data()) )
    throw PlException(t);

  PlPutTerm(C_, t);
}

inline
PlCompound::PlCompound(const std::wstring& text)
{ term_t t = PL_new_term_ref();

  // TODO: what is wchar_t equivalent of PL_put_term_from_chars()?
  if ( !PL_wchars_to_term(text.c_str(), t) ) // TODO: use text.size()
    throw PlException(t);

  PlPutTerm(C_, t);
}

inline
PlCompound::PlCompound(const char *functor, const PlTermv& args)
{ PlCheck(PL_cons_functor_v(C_,
                            PL_new_functor(PL_new_atom(functor), args.size()),
                            args.termv()));
}

inline
PlCompound::PlCompound(const wchar_t *functor, const PlTermv& args)
{ PlCheck(PL_cons_functor_v(
                            C_,
                            PL_new_functor(PL_new_atom_wchars(wcslen(functor), functor),
                                           args.size()),
                            args.termv()));
}

inline
PlCompound::PlCompound(const std::string& functor, const PlTermv& args)
{ PlCheck(PL_cons_functor_v(C_,
                            PL_new_functor(PL_new_atom_nchars(functor.size(), functor.data()), args.size()),
                            args.termv()));
}

inline
PlCompound::PlCompound(const std::wstring& functor, const PlTermv& args)
{ PlCheck(PL_cons_functor_v(C_,
                            PL_new_functor(PL_new_atom_wchars(functor.size(), functor.data()), args.size()),
                            args.termv()));
}

		 /*******************************
		 *         TERMV (BODY)         *
		 *******************************/


inline PlTermv::PlTermv(const PlAtom& a)
  : size_(1),
    a0_(PlTerm_atom(a).C_) {}

inline PlTermv::PlTermv(const PlTerm& m0)
  : size_(1),
    a0_(m0.C_) {}

inline PlTermv::PlTermv(const PlTerm& m0, const PlTerm& m1)
  : size_(2),
    a0_(PL_new_term_refs(2))
{ if ( !a0_ )
    throw PlResourceError();
  PlPutTerm(a0_+0, m0.C_);
  PlPutTerm(a0_+1, m1.C_);
}

inline PlTermv::PlTermv(const PlTerm& m0, const PlTerm& m1, const PlTerm& m2)
  : size_(3),
    a0_(PL_new_term_refs(3))
{ if ( !a0_ )
    throw PlResourceError();
  PlPutTerm(a0_+0, m0.C_);
  PlPutTerm(a0_+1, m1.C_);
  PlPutTerm(a0_+2, m2.C_);
}

inline PlTermv::PlTermv(const PlTerm& m0, const PlTerm& m1, const PlTerm& m2, const PlTerm& m3)
  : size_(4),
    a0_(PL_new_term_refs(4))
{ if ( !a0_ )
    throw PlResourceError();
  PlPutTerm(a0_+0, m0.C_);
  PlPutTerm(a0_+1, m1.C_);
  PlPutTerm(a0_+2, m2.C_);
  PlPutTerm(a0_+3, m3.C_);
}

inline PlTermv::PlTermv(const PlTerm& m0, const PlTerm& m1, const PlTerm& m2,
			const PlTerm& m3, const PlTerm& m4)
  : size_(5),
    a0_(PL_new_term_refs(5))
{ if ( !a0_ )
    throw PlResourceError();
  PlPutTerm(a0_+0, m0.C_);
  PlPutTerm(a0_+1, m1.C_);
  PlPutTerm(a0_+2, m2.C_);
  PlPutTerm(a0_+3, m3.C_);
  PlPutTerm(a0_+4, m4.C_);
}

inline PlTerm
PlTermv::operator [](size_t n) const
{ if ( n >= size_ )
    throw PlTermvDomainError(size_, n);

  return PlTerm_term_t(a0_+n);
}


		 /*******************************
		 *	EXCEPTIONS (BODY)       *
		 *******************************/

inline const char *
PlException::c_str() const
{ PlFrame fr;
#ifdef USE_PRINT_MESSAGE
  PlTermv av(2);

  av[0].unify_term_check(PlCompound("print_message",
                                    PlTermv("error", *this)));
  PlQuery q("$write_on_string", av);
  if ( q.next_solution() )
    return av[1].c_str();
#else
  PlTermv av(2);
  av[0].unify_term_check(*this);
  PlQuery q("$messages", "message_to_string", av);
  if ( q.next_solution() )
    return av[1].c_str();
#endif
  return "[ERROR: Failed to generate message.  Internal error]\n";
}


inline const wchar_t *
PlException::wc_str() const
{ PlFrame fr;
#ifdef USE_PRINT_MESSAGE
  PlTermv av(2);

  av[0].unify_term_check(PlCompound("print_message",
                                    PlTermv("error", *this)));
  PlQuery q("$write_on_string", av);
  if ( q.next_solution() )
    return av[1].wc_str();
#else
  PlTermv av(2);
  av[0].unify_term_check(PlTerm(*this));
  PlQuery q("$messages", "message_to_string", av);
  if ( q.next_solution() )
    return av[1].wc_str();
#endif
  return L"[ERROR: Failed to generate message.  Internal error]\n";
}


inline PlException
PlException::adjust_for_throw() const
{ term_t a = PL_new_term_ref();
  atom_t name;
  ARITY_T arity;

  if ( PL_get_arg(1, C_, a) &&
       PL_get_name_arity(a, &name, &arity) )
  { const char *s = PL_atom_chars(name);

    if ( strcmp(s, "type_error") == 0 )
      return PlTypeError(*this);
    if ( strcmp(s, "domain_error") == 0 )
      return PlDomainError(*this);
    if ( strcmp(s, "resource_error") == 0 )
      return PlResourceError(*this);
  }

  return *this;
}


		 /*******************************
		 *	    QUERY (BODY)	*
		 *******************************/

inline bool
PlQuery::next_solution()
{ int rval;

  if ( !(rval = PL_next_solution(qid_)) )
  { term_t ex;

    PL_close_query(qid_);
    qid_ = 0;

    if ( (ex = PL_exception(0)) )
      throw PlException(ex).adjust_for_throw();
  }
  return rval;
}


		 /*******************************
		 *	      ENGINE		*
		 *******************************/

class PlError
{
private:
  char *message_;

public:
  PlError(const char *msg)
  { size_t len = strlen(msg)+1;
    message_ = new char[len];
#ifdef _MSC_VER				/* Yek */
#pragma warning( push )
#pragma warning (disable:4996)
#endif
    strncpy(message_, msg, len);
#ifdef _MSC_VER
#pragma warning( pop )
#endif
  }

  ~PlError()
  { delete[] message_;
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
    char **av = static_cast<char**>(malloc(sizeof(char *) * 2));

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
#define PROLOG_MODULE static_cast<const char*>(nullptr)
#endif

#define NAMED_PREDICATE(plname, name, arity) \
	static foreign_t \
	pl_ ## name ## __ ## arity(PlTermv PL_av); \
	static foreign_t \
	_pl_ ## name ## __ ## arity(term_t t0, int a, control_t c) \
	{ (void)a; (void)c; \
	  try \
	  { \
	    return pl_ ## name ## __ ## arity(PlTermv(arity, PlTerm_term_t(t0))); \
	  } catch ( std::bad_alloc& ) \
	  { return PlResourceError("memory").plThrow(); \
	  } catch ( PlFail& ) \
	  { return false; \
	  } catch ( PlException& ex ) \
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
	  } catch ( std::bad_alloc& ) \
	  { return PlResourceError("memory").plThrow(); \
	  } catch ( PlFail& ) \
	  { return false; \
	  } catch ( PlException& ex ) \
	  { return ex.plThrow(); \
	  } \
	} \
	static PlRegister _x ## name ## __0(PROLOG_MODULE, plname, 0, \
					    _pl_ ## name ## __0); \
	static foreign_t pl_ ## name ## __0(void)

#define NAMED_PREDICATE_NONDET(plname, name, arity) \
	static foreign_t \
	pl_ ## name ## __ ## arity(PlTermv PL_av, control_t handle);       \
	static foreign_t \
	_pl_ ## name ## __ ## arity(term_t t0, int a, control_t c) \
	{ (void)a; \
	  try \
	  { \
	    return pl_ ## name ## __ ## arity(PlTermv(arity, PlTerm_term_t(t0)), c); \
	  } catch ( std::bad_alloc& ) \
	  { return PlResourceError("memory").plThrow(); \
	  } catch ( PlFail& ) \
	  { return false; \
	  } catch ( PlException& ex ) \
	  { return ex.plThrow(); \
	  } \
	} \
	static PlRegister _x ## name ## __ ## arity(PROLOG_MODULE, plname, arity, \
						    _pl_ ## name ## __ ## arity, \
						    PL_FA_NONDETERMINISTIC | PL_FA_VARARGS); \
	static foreign_t pl_ ## name ## __ ## arity(PlTermv PL_av, control_t handle)

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

		 /*******************************
		 *     NONDET HELPERS		*
		 *******************************/

// For non-deterministic predicates that allocate a context, the
// PlForeignContextPtr is a RAII (Resource Acquisition Is
// Initialization) class (a kind of "smart pointer") that holds a
// pointer that is deleted at exit (whether by return or exception)
// unless deferred_free_ is false. This simplifies code that can throw
// exceptions such as the PlTerm::as_long().
//
// The pointer must have been allocated using the `new` operator.  The
// pointer can have the value nullptr.
//
// THe methods are the usual "smart pointer" ones: dereference (either
// using `->` or `*`), get(), set(ptr). In addition:
//    deferred_free() - the pointer will be deleted on return/throw
//    keep() - the pointer will not be deleted on return/throw

template <typename ContextType> class PlForeignContextPtr
{
  ContextType *ptr_;
  bool deferred_free_;

public:
  explicit PlForeignContextPtr(control_t handle)
    : ptr_(static_cast<ContextType *>(PL_foreign_context_address(handle))),
      deferred_free_(true) { }

  ContextType& operator*()  const { return *ptr_; }
  ContextType* operator->() const { return ptr_; }
  ContextType* get()        const { return ptr_; }
  void set(ContextType* ptr = nullptr) { ptr_ = ptr; }

  void deferred_free()       { deferred_free_ = true; }
  void deferred_free(bool v) { deferred_free_ = v; }
  void keep()                { deferred_free_ = false; }

  ~PlForeignContextPtr()
  { if ( deferred_free_ )
      delete ptr_; // it's safe to delete nullptr
  }
};

#endif /*_SWI_CPP_H*/
