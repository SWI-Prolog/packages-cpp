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
#include <climits>
#include <cstdint>
#include <cstring>
#include <cwchar>
#include <functional>
#include <string>

#if INT_MAX != 0x7fffffff
  #error "Unexpected value for INT_MAX"
#endif

#if LONG_MAX == 0x7fffffffffffffff
  #if SIZE_MAX != 0xffffffffffffffff
  #error "Unexpected value for SIZE_MAX"
  #endif
#elif LONG_MAX == 0x7fffffff
  #if SIZE_MAX == 0xffffffffffffffff || SIZE_MAX == 0xffffffff
  #else
    #error "Unexpected value for SIZE_MAX"
  #endif
#else
  #error "Unexpected value for LONG_MAX"
#endif

#if !(defined(__APPLE__) || defined(__FreeBSD__))
#include <malloc.h>
#endif

class PlAtom;
class PlTerm;
class PlTermv;


// A pseudo-exception for quick exist on failure, for use by the unify
// methods.  This is special-cased in the PREDICATE et al macros.
// Note that it is *not* a subclass of PlException. See the
// documentation for more details on how this works with returning
// Prolog failure and returning exceptions.
class PlFail
{
public:
  explicit PlFail() {}
};


// Throw PlFail on failure or exception.  This exception is caught by
// the PREDICATE, which simply returns false ... if the failure was
// caused by an exception, SWI-Prolog will detect that and turn the
// failure into a Prolog exception.  Therefore, there is no need for
// calling PL_exception(0) and doing something different if there is
// a pending Prolog exception (to call PL_exception(0), use
// PlException_qid()).
inline void
PlCheck(int rc)
{ if ( !rc )
    throw PlFail();
}


		 /*******************************
		 * COMMON OPERATIONS (TEMPLATE) *
		 *******************************/

template <typename C_t> class WrappedC
{
public:
  C_t C_; // The wrapped value

  static constexpr C_t null = 0;
  bool is_null()  const { return C_ == null; }
  bool not_null() const { return C_ != null; }
  void verify() const; // Throw exception if is_null()

  explicit WrappedC<C_t>(C_t v)
    : C_(v) { }
  WrappedC<C_t>(const WrappedC<C_t>&) = default;
  // WrappedC<C_t>& operator =(const WrappedC<C_t>&) = default; // deprecated/deleted in PlTerm
  operator bool() const = delete; // Use not_null() instead
  // reset() is common with "smart pointers"; wrapped wrapped atom_t,
  // term_t, etc. aren't "smart" in the same sense, but the objects
  // they refer to are garbage collected and some care is needed to
  // ensure they have appropriate reference counts (e.g.,
  // PlAtom::register_ref() and PlTerm::record()).
  void reset(C_t v = null) { C_ = v; }
};

// TODO: use PlEncoding wherever a method takes a char* or std::string.
// TODO: #define SWI_DEFAULT_TEXT_ENCODING EncUTF8
//       (set outside SWI-cpp2.h, with an appropriate default)
// For the various "get/put/unify string" methods:
typedef enum PlEncoding
{ EncLatin1 = REP_ISO_LATIN_1,
  EncUTF8   = REP_UTF8,
  EncLocale = REP_MB
} PlEncoding;
static constexpr PlEncoding ENC_INPUT = EncLatin1; // TODO: EncUTF8?
static constexpr PlEncoding ENC_OUTPUT = EncLocale;


		 /*******************************
		 *  PL_STRINGS_{MARK,RELEASE}   *
		 *******************************/

class PlStringBuffers
{
private:
  buf_mark_t __PL_mark;

public:
  explicit PlStringBuffers()
  { PL_mark_string_buffers(&__PL_mark); // TODO: modify PL_STRINGS_MARK() to be used here
  }

  ~PlStringBuffers()
  { PL_release_string_buffers_from_mark(__PL_mark); // TODO: modify PL_STRINGS_RELEASE() to be used here
  }
};


		 /*******************************
		 *	 PROLOG CONSTANTS	*
		 *******************************/

class PlFunctor : public WrappedC<functor_t>
{
public:
  PlFunctor(functor_t v)
    : WrappedC<functor_t>(v) { }
  // PlFunctor(const char*) is handled by std::string constructor
  explicit PlFunctor(const std::string& name, size_t arity);
  explicit PlFunctor(const std::wstring& name, size_t arity);

  bool operator ==(functor_t to) = delete;

  [[deprecated("use PlPredicate")]] predicate_t pred(module_t m) const {
    predicate_t p = PL_pred(C_, m);
    if ( p == nullptr )
      throw PlFail();
    return p;
  }

  PlAtom name() const;

  size_t arity() const {
    return PL_functor_arity(C_);
  }
};


class PlAtom : public WrappedC<atom_t>
{
public:
  explicit PlAtom(atom_t v)
    : WrappedC<atom_t>(v) { }
  explicit PlAtom(const std::string& text) // TODO: add encoding
    : WrappedC<atom_t>(PL_new_atom_nchars(text.size(), text.data()))
  { verify();
  }
  explicit PlAtom(const std::wstring& text)
    : WrappedC<atom_t>(PL_new_atom_wchars(text.size(), text.data()))
  { verify();
  }
  explicit PlAtom(const char *text)
    : WrappedC<atom_t>(PL_new_atom_nchars(static_cast<size_t>(-1), text))
  { verify();
  }

  const std::string get_mbchars(unsigned int flags) const
  { PlStringBuffers _string_buffers;
    size_t len;
    char *s;
    PlCheck(PL_atom_mbchars(C_, &len, &s, CVT_EXCEPTION|flags));
    return std::string(s, len);
  }

  const std::string as_string(PlEncoding enc=ENC_OUTPUT) const
  { return get_mbchars(static_cast<unsigned int>(enc));
  }
  const std::wstring as_wstring() const;

  bool operator ==(const char *s) const
  { PlStringBuffers _string_buffers;
    return strcmp(s, PL_atom_nchars(C_, nullptr)) == 0; // TODO: use PL_atom_mbchars() or get_mbchars()
  }
  bool operator ==(const wchar_t *s) const
  { PlStringBuffers _string_buffers;
    return wcscmp(s, PL_atom_wchars(C_, nullptr)) == 0;
  }
  bool operator ==(const std::string& s) const
  { PlStringBuffers _string_buffers;
    size_t len;
    const char* s0 = PL_atom_nchars(C_, &len); // TODO: use PL_atom_mbchars() or get_mbchars()
    return std::string(s0, len) == s;
  }
  bool operator ==(const std::wstring& s) const
  { PlStringBuffers _string_buffers;
    size_t len;
    const wchar_t* s0 = PL_atom_wchars(C_, &len);
    return std::wstring(s0, len) == s;
  }
  bool operator ==(const PlAtom &a) const
  { return C_ == a.C_;
  }
  [[deprecated("use PlAtom instead of atomt_t")]] bool operator ==(atom_t to) const
  { return C_ == to;
  }

  bool operator !=(const char *s) const
  { return !(*this == s);
  }
  bool operator !=(const wchar_t *s) const
  { return !(*this == s);
  }
  // TODO: add std::string, std::wstring
  bool operator !=(const PlAtom &a) const
  { return !(*this == a);
  }
  [[deprecated("use PlAtom instead of atom_t")]] bool operator !=(atom_t to) const
  { return C_ != to;
  }

  void register_ref() const
  { PL_register_atom(C_);
  }

  void unregister_ref() const
  { PL_unregister_atom(C_);
  }

  // TODO: replace blob_data() with C++ interface to blobs
  void* blob_data(size_t *len, struct PL_blob_t **type) const
  { return PL_blob_data(C_, len, type);
  }
};

		 /*******************************
		 *     GENERIC PROLOG TERM	*
		 *******************************/

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
  [[nodiscard]] static bool chkex(int rc); // if failed due to exception, throw exception

public:
  PlTerm(const PlTerm&) = default;
  explicit PlTerm(const PlAtom& a)
    : WrappedC<term_t>(PL_new_term_ref())
  { verify();
    PlCheck(PL_put_atom(C_, a.C_));
  }

  // PlTerm& operator =(const PlTerm&) = delete; // see below

  int type()         const { return PL_term_type(C_); } // PL_VARIABLE, PL_ATOM, etc.
  bool is_variable() const { return PL_is_variable(C_); }
  bool is_ground()   const { return PL_is_ground(C_); }
  bool is_atom()     const { return PL_is_atom(C_); }
  bool is_integer()  const { return PL_is_integer(C_); }
  bool is_string()   const { return PL_is_string(C_); }
  bool is_float()    const { return PL_is_float(C_); }
  bool is_rational() const { return PL_is_rational(C_); }
  bool is_compound() const { return PL_is_compound(C_); }
  bool is_callable() const { return PL_is_callable(C_); }
  bool is_list()     const { return PL_is_list(C_); }
  bool is_dict()     const { return PL_is_dict(C_); }
  bool is_pair()     const { return PL_is_pair(C_); }
  bool is_atomic()   const { return PL_is_atomic(C_); }
  bool is_number()   const { return PL_is_number(C_); }
  bool is_acyclic()  const { return PL_is_acyclic(C_); }
  bool is_functor(const PlFunctor& f) const { return PL_is_functor(C_, f.C_); }

  record_t record() const;

					/* PlTerm --> C */
  [[deprecated("use as_long()")]]     explicit operator long()     const { return as_long(); }
  [[deprecated("use as_int()")]]      explicit operator int()      const { return as_int(); }
  [[deprecated("use as_uint32_t()")]] explicit operator uint32_t() const { return as_uint32_t(); }
  [[deprecated("use as_uint64_t()")]] explicit operator uint64_t() const { return as_uint64_t(); }
  [[deprecated("use as_float()")]]    explicit operator double()   const { return as_float(); }
  [[deprecated("use as_pointer()")]]  explicit operator void *()   const { return as_pointer(); }
  [[deprecated("use as_atom()")]]     explicit operator PlAtom()   const { return as_atom(); }

  // No need for overloading int64_t, size_t, etc.; these are defined
  // by the compiler in terms of one of the types below.
  // TODO: add wchar_t, char16_t, char32_t
  void integer(bool               *v) const { int v0; PlCheck(PL_cvt_i_bool(C_, &v0)); *v = v0; }
  void integer(char               *v) const { PlCheck(PL_cvt_i_char(  C_, v)); }
  void integer(int                *v) const { PlCheck(PL_cvt_i_int(   C_, v)); }
  void integer(long               *v) const { PlCheck(PL_cvt_i_long(  C_, v)); }
  void integer(long long          *v) const { PlCheck(PL_cvt_i_llong( C_, v)); }
  void integer(short              *v) const { PlCheck(PL_cvt_i_short( C_, v)); }
  void integer(signed char        *v) const { PlCheck(PL_cvt_i_schar( C_, v)); }
  void integer(unsigned char      *v) const { PlCheck(PL_cvt_i_uchar( C_, v)); }
  void integer(unsigned int       *v) const { PlCheck(PL_cvt_i_uint(  C_, v)); }
  void integer(unsigned long      *v) const { PlCheck(PL_cvt_i_ulong( C_, v)); }
  void integer(unsigned long long *v) const { PlCheck(PL_cvt_i_ullong(C_, v)); }
  void integer(unsigned short     *v) const { PlCheck(PL_cvt_i_ushort(C_, v)); }

  // All the conversion functions throw a PlTypeError exception if
  // they fail (because of the wrong Prolog type). If you want to be
  // safe, use is_XXX() first to verify the type.

  std::string   as_string(PlEncoding enc=ENC_OUTPUT) const;
  std::wstring  as_wstring()  const;
  long          as_long()     const { long          v; integer(&v); return v; }
  int32_t       as_int32_t()  const { int32_t       v; integer(&v); return v; }
  uint32_t      as_uint32_t() const { uint32_t      v; integer(&v); return v; }
  uint64_t      as_uint64_t() const { uint64_t      v; integer(&v); return v; }
  int64_t       as_int64_t()  const { int64_t       v; integer(&v); return v; }
  size_t        as_size_t()   const { size_t        v; integer(&v); return v; }
  int           as_int()      const { int           v; integer(&v); return v; }
  unsigned      as_uint()     const { unsigned      v; integer(&v); return v; }
  unsigned long as_ulong()    const { unsigned long v; integer(&v); return v; }
  bool          as_bool()     const { bool          v; integer(&v); return v; }
  void          as_nil()      const;
  double        as_float()    const;
  double        as_double()   const { return as_float(); }
  void *        as_pointer()  const; // TODO: replace with C++ interface to blobs

  // TODO: PL_get_mpz(), PL_getr_mpq()

  std::string get_nchars(unsigned int flags) const
  { PlStringBuffers _string_buffers;
    char  *s;
    size_t len;
    PlCheck(PL_get_nchars(C_, &len, &s, flags|CVT_EXCEPTION));
    return std::string(s, len);
  }


  PlAtom as_atom() const;
  [[nodiscard]] bool get_if_atom(PlAtom *a) const
  { atom_t v;
    if (PL_get_atom(C_, &v) )
    { *a = PlAtom(v);
      return true;
    }
    return false;
  }

					/* Compounds */
  PlTerm operator [](size_t index) const;
  size_t arity() const; // throws PlTypeError if not a "compound" or atom
  PlAtom name() const; // throws PlTypeError if not a "compound" or atom
  [[nodiscard]] bool name_arity(PlAtom *name, size_t *arity) const; // name and/or arity can be nullptr
  PlTerm copy_term_ref() const
  { PlTerm t(PL_copy_term_ref(C_));
    t.verify();
    return t;
  }

  // The assignment operators from version 1 have been removed because
  // of possible confusion with the standard assignment and copy
  // operators. Also, they have unusual semantics; normally an
  // assignment operator would have the form
  //   PlTerm& PlTerm::operator =(const PlTerm&)
  // with implicit or explicit cast from, e.g. PlAtom to PlTerm

						/* UNIFY */
  // TODO: handle encoding for char*, std::string
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(const PlTerm& t2)   const { return unify_term(t2); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(const PlAtom& a)    const { return unify_atom(a); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(const char *v)      const { return unify_atom(v); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(const wchar_t *v)   const { return unify_atom(v); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(intptr_t v)         const { return unify_integer(v); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(double v)           const { return unify_float(v); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(const PlFunctor& f) const { return unify_functor(f); }

  // All the unify_*() methods check for an exception (and throw), so
  // the return code is whether the unification succeeded or not.
  // TODO: replace PL_unify_*() with PL_unify_chars() and flags, where appropriate
  // TODO: handle encodings for char*, std::string
  [[nodiscard]] bool unify_term(const PlTerm& t2)        const { return chkex(PL_unify(C_, t2.C_)); }
  [[nodiscard]] bool unify_atom(const PlAtom& a)         const { return chkex(PL_unify_atom(C_, a.C_)); }
  [[nodiscard]] bool unify_chars(int flags, size_t len, const char *s) const { return chkex(PL_unify_chars(C_, flags, len, s)); }
  [[nodiscard]] bool unify_chars(int flags, const std::string& s) const { return chkex(PL_unify_chars(C_, flags, s.size(), s.data())); }
  [[nodiscard]] bool unify_atom(const char*           v) const { return chkex(PL_unify_atom_chars(C_, v)); }
  [[nodiscard]] bool unify_atom(const wchar_t*        v) const { return chkex(PL_unify_wchars(C_, PL_ATOM, static_cast<size_t>(-1), v)); }
  [[nodiscard]] bool unify_atom(const std::string&    v) const { return chkex(PL_unify_atom_nchars(C_, v.size(), v.data())); }
  [[nodiscard]] bool unify_atom(const std::wstring&   v) const { return chkex(PL_unify_wchars(C_, PL_ATOM, v.size(), v.data())); }
  [[nodiscard]] bool unify_list_codes(const char*     v) const { return chkex(PL_unify_list_codes(C_, v)); } // TODO: [[deprecated]]
  [[nodiscard]] bool unify_list_chars(const char*     v) const { return chkex(PL_unify_list_chars(C_, v)); } // TODO: [[deprecated]]
  // See comment with PlTerm::integer() about the overloading.
  [[nodiscard]] bool unify_integer(bool               v) const { return chkex(PL_unify_int64(C_, v)); }
  [[nodiscard]] bool unify_integer(char               v) const { return chkex(PL_unify_int64(C_, v)); }
  [[nodiscard]] bool unify_integer(int                v) const { return chkex(PL_unify_int64(C_, v)); }
  [[nodiscard]] bool unify_integer(long               v) const { return chkex(PL_unify_int64(C_, v)); }
  [[nodiscard]] bool unify_integer(long long          v) const { return chkex(PL_unify_int64(C_, v)); }
  [[nodiscard]] bool unify_integer(short              v) const { return chkex(PL_unify_int64(C_, v)); }
  [[nodiscard]] bool unify_integer(signed char        v) const { return chkex(PL_unify_int64(C_, v)); }
  [[nodiscard]] bool unify_integer(unsigned char      v) const { return chkex(PL_unify_uint64(C_, v)); }
  [[nodiscard]] bool unify_integer(unsigned int       v) const { return chkex(PL_unify_uint64(C_, v)); }
  [[nodiscard]] bool unify_integer(unsigned long      v) const { return chkex(PL_unify_uint64(C_, v)); }
  [[nodiscard]] bool unify_integer(unsigned long long v) const { return chkex(PL_unify_uint64(C_, v)); }
  [[nodiscard]] bool unify_integer(unsigned short     v) const { return chkex(PL_unify_uint64(C_, v)); }
  [[nodiscard]] bool unify_float(double               v) const { return chkex(PL_unify_float(C_, v)); }
  [[nodiscard]] bool unify_string(const std::string&  v) const { return chkex(PL_unify_string_nchars(C_, v.size(), v.data())); }
  [[nodiscard]] bool unify_string(const std::wstring& v) const { return chkex(PL_unify_wchars(C_, PL_STRING, v.size(), v.data())); }
  [[nodiscard]] bool unify_functor(const PlFunctor&   f) const { return chkex(PL_unify_functor(C_, f.C_)); }
  [[nodiscard]] bool unify_pointer(void *ptr)            const { return chkex(PL_unify_pointer(C_, ptr)); } // TODO: replace with C++ interface to blobs
  [[nodiscard]] bool unify_nil()                         const { return chkex(PL_unify_nil(C_)); }
  [[nodiscard]] bool unify_nil_ex()                      const { return chkex(PL_unify_nil_ex(C_)); }
  [[nodiscard]] bool unify_list(PlTerm h, PlTerm t)      const { return chkex(PL_unify_list(C_, h.C_, t.C_)); }
  [[nodiscard]] bool unify_list_ex(PlTerm h, PlTerm t)   const { return chkex(PL_unify_list_ex(C_, h.C_, t.C_)); }
  [[nodiscard]] bool unify_bool(bool val)                const { return chkex(PL_unify_bool(C_, val)); }
  [[nodiscard]] bool unify_bool_ex(bool val)             const { return chkex(PL_unify_bool_ex(C_, val)); }
  [[nodiscard]] bool unify_blob(void *blob, size_t len, PL_blob_t *type) const { return chkex(PL_unify_blob(C_, blob, len, type)); }

  // TODO: handle PL_unify_mpz(), PL_unify_mpq()

					/* Comparison standard order terms */
  [[nodiscard]] int compare(const PlTerm& t2)       const { return PL_compare(C_, t2.C_); }
  bool operator == (const PlTerm& t2) const { return compare(t2) == 0; }
  bool operator != (const PlTerm& t2) const { return compare(t2) != 0; }
  bool operator <  (const PlTerm& t2) const { return compare(t2) <  0; }
  bool operator >  (const PlTerm& t2) const { return compare(t2) >  0; }
  bool operator <= (const PlTerm& t2) const { return compare(t2) <= 0; }
  bool operator >= (const PlTerm& t2) const { return compare(t2) >= 0; }
					/* comparison (long) */
  /* TODO: uint64_t; but that requires adding a lot of overloaded methods */
  bool operator == (int64_t v) const;
  bool operator != (int64_t v) const;
  bool operator <  (int64_t v) const;
  bool operator >  (int64_t v) const;
  bool operator <= (int64_t v) const;
  bool operator >= (int64_t v) const;

					/* comparison (atom, string) */
  // TODO: deprecate comparison with char*s, std::string, etc. and instead
  //       make a new method that includes the encoding. For now, these
  //       are safe only with ASCII (EncLatin1):
  bool operator ==(const char *s) const;
  bool operator ==(const wchar_t *s) const;
  bool operator ==(const std::string& s) const;
  bool operator ==(const std::wstring& s) const;
  bool operator ==(const PlAtom& a) const;

  bool operator !=(const char *s)         const { return !(*this == s); }
  bool operator !=(const wchar_t *s)      const { return !(*this == s); }
  bool operator !=(const std::string& s)  const { return !(*this == s); }
  bool operator !=(const std::wstring& s) const { return !(*this == s); }
  bool operator !=(const PlAtom& a)       const { return !(*this == a); }

  // E.g.: t.write(Serror, 1200, PL_WRT_NEWLINE|PL_WRT_QUOTED);
  void write(IOSTREAM *s, int precedence, int flags) const { PlCheck(PL_write_term(s, C_, precedence, flags)); }
};


class PlTerm_atom : public PlTerm
{
public:
  // TODO: Use the fact that PL_put_atom() always returns true
  // TODO: Add encoding for char*, std::string.
  //       For now, these are safe only with ASCII (EncLatin1):
  explicit PlTerm_atom(atom_t a)                 { PlCheck(PL_put_atom(C_, a)); }
  explicit PlTerm_atom(const PlAtom& a)          { PlCheck(PL_put_atom(C_, a.C_)); }
  explicit PlTerm_atom(const char *text)         { PlCheck(PL_put_atom_chars(C_, text)); } // TODO: add encoding
  explicit PlTerm_atom(const wchar_t *text)      { PlCheck(PL_unify_wchars(C_, PL_ATOM, static_cast<size_t>(-1), text)); }
  explicit PlTerm_atom(const std::string& text)  { PlCheck(PL_put_atom_nchars(C_, text.size(), text.data())); } // TODO: add encoding
  explicit PlTerm_atom(const std::wstring& text) { PlCheck(PL_unify_wchars(C_, PL_ATOM, text.size(), text.data())); }
};

class PlTerm_var : public PlTerm
{
public:
  explicit PlTerm_var() { } // PlTerm() calls Pl_new_term_ref()
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
  // See comment with PlTerm::integer() about the overloading.
  explicit PlTerm_integer(char               v) { PlCheck(PL_put_int64(C_, v)); }
  explicit PlTerm_integer(int                v) { PlCheck(PL_put_int64(C_, v)); }
  explicit PlTerm_integer(long               v) { PlCheck(PL_put_int64(C_, v)); }
  explicit PlTerm_integer(long long          v) { PlCheck(PL_put_int64(C_, v)); }
  explicit PlTerm_integer(short              v) { PlCheck(PL_put_int64(C_, v)); }
  explicit PlTerm_integer(signed char        v) { PlCheck(PL_put_int64(C_, v)); }
  explicit PlTerm_integer(unsigned char      v) { PlCheck(PL_put_uint64(C_, v)); }
  explicit PlTerm_integer(unsigned int       v) { PlCheck(PL_put_uint64(C_, v)); }
  explicit PlTerm_integer(unsigned long      v) { PlCheck(PL_put_uint64(C_, v)); }
  explicit PlTerm_integer(unsigned long long v) { PlCheck(PL_put_uint64(C_, v)); }
  explicit PlTerm_integer(unsigned short     v) { PlCheck(PL_put_uint64(C_, v)); }
};

class PlTerm_float : public PlTerm
{
public:
  explicit PlTerm_float(double v) { PlCheck(PL_put_float(C_, v));  }
};

// TODO: deprecate PlTerm_pointer and replace by C++ interface to blobs
//       (see also PlAtom::blob_data(), PlTerm::as_pointer())
class PlTerm_pointer : public PlTerm
{
public:
  explicit PlTerm_pointer(void * ptr) { PlCheck(PL_put_pointer(C_, ptr)); }
};

class PlTerm_recorded : public PlTerm
{
public:
  explicit PlTerm_recorded(record_t r) { PlCheck(PL_recorded(r, C_)); }
};

class PlModule : public WrappedC<module_t>
{
public:
  explicit PlModule(module_t m)
    : WrappedC<module_t>(m) { }
  explicit PlModule(const std::string& name)
    : WrappedC<module_t>(PL_new_module(PlAtom(name).C_))
  { verify();
  }
  explicit PlModule(PlAtom name)
    : WrappedC<module_t>(PL_new_module(name.C_))
  { verify();
  }
};

class PlPredicate : public WrappedC<predicate_t>
{
public:
  explicit PlPredicate(predicate_t p)
    : WrappedC<predicate_t>(p) { }
  explicit PlPredicate(PlFunctor f)
    : WrappedC<predicate_t>(PL_pred(f.C_, static_cast<module_t>(PlModule::null)))
  { verify();
  }
  explicit PlPredicate(PlFunctor f, PlModule m)
    : WrappedC<predicate_t>(PL_pred(f.C_, m.C_))
  { verify();
  }
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
  explicit PlTermv(size_t n = 0)
    : size_(n),
      a0_(n ? PL_new_term_refs(static_cast<int>(n)) : PlTerm::null)
  { if ( size_ && a0_ == PlTerm::null )
      throw PlFail();
  }
  explicit PlTermv(size_t n, const PlTerm& t0)
    : size_(n),
      a0_(t0.C_)
  { if ( size_ && a0_ == PlTerm::null )
      throw PlFail();
  }

  term_t termv() const
  { // Note that a0_ can be PlTerm::null if size_ == 0
    return a0_;
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
  explicit PlCompound(const wchar_t *text);
  explicit PlCompound(const std::string& text, PlEncoding enc=ENC_INPUT);
  // TODO: add PlCompound(const char*), which is slightly more efficient than implicitly converting to std::string first
  explicit PlCompound(const std::wstring& text);
  PlCompound(const char *functor, const PlTermv& args);  // TODO: PlEncoding
  PlCompound(const wchar_t *functor, const PlTermv& args);
  PlCompound(const std::string& functor, const PlTermv& args); // TODO: PlEncoding
  PlCompound(const std::wstring& functor, const PlTermv& args);
};


class PlTerm_string : public PlTerm
{
public:
  // TODO: PlEncoding
  PlTerm_string(const char *text) { PlCheck(PL_put_string_chars(C_, text)); }
  PlTerm_string(const char *text, size_t len) { PlCheck(PL_put_string_nchars(C_, len, text)); }
  PlTerm_string(const wchar_t *text) { PlCheck(PL_unify_wchars(C_, PL_STRING, static_cast<size_t>(-1), text)); }
  PlTerm_string(const wchar_t *text, size_t len) { PlCheck(PL_unify_wchars(C_, PL_STRING, len, text));}
  PlTerm_string(const std::string& text) { PlCheck(PL_put_string_nchars(C_, text.size(), text.data())); }
  PlTerm_string(const std::wstring& text) { PlCheck(PL_unify_wchars(C_, PL_STRING, text.size(), text.data())); }
};


class PlTerm_list_codes : public PlTerm
{
public:
  // TODO: PlEncoding + deprecate this interface
  PlTerm_list_codes(const char *text) { PlCheck(PL_put_list_codes(C_, text)); }
  PlTerm_list_codes(const wchar_t *text) { PlCheck(PL_unify_wchars(C_, PL_CODE_LIST, static_cast<size_t>(-1), text)); }
};


class PlTerm_list_chars : public PlTerm
{
public:
  // TODO: PlEncoding + deprecate this interface
  PlTerm_list_chars(const char *text) { PlCheck(PL_put_list_chars(C_, text)); }
  PlTerm_list_chars(const wchar_t *text) { PlCheck(PL_unify_wchars(C_, PL_CHAR_LIST, static_cast<size_t>(-1), text)); }
};


		 /*******************************
		 *	      EXCEPTIONS	*
		 *******************************/

class PlException : public PlTerm
{
public:
  explicit PlException(const PlAtom& a)
    : PlTerm(PlTerm_atom(a).C_)
  { verify();
  }
  explicit PlException(const PlTerm& t)
    : PlTerm(t)
  { verify();
  }

  // The following methods override PlTerm, but do not use the
  // "override" keyword because the method isn't virtual. Because
  // the API has PlTerm as a thin wrapper on term_t, with minimal
  // overhead, there are no virtual methods.
  const std::string  as_string(PlEncoding enc=ENC_OUTPUT) const { return string_term().as_string(enc); }
  const std::wstring as_wstring() const { return string_term().as_wstring(); }

  // plThrow() is for the try-catch in PREDICATE - returns the result
  // of PL_raise_exception(), which is always `false`, as a foreign_t.
  foreign_t plThrow()
  { return static_cast<foreign_t>(PL_raise_exception(C_));
  }

protected:
  explicit PlException(term_t ex)
    : PlTerm(ex) {}

private:
  PlTerm string_term() const;
};

class PlException_qid : public PlException
{
public:
  explicit PlException_qid(qid_t qid = 0)
    : PlException(PL_exception(qid)) { }
};


class PlTypeError : public PlException
{
public:
  explicit PlTypeError(const char *expected, const PlTerm& actual) :
    // TODO: use PL_type_error() or lazy PlTerm_atom()
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
  explicit PlDomainError(const char *expected, const PlTerm& actual) :
    // TODO: Use PL_domain_error() or lazy PlTerm_atom()
    PlException(PlCompound("error",
			   PlTermv(PlCompound("domain_error",
					      PlTermv(PlTerm_atom(expected), actual)),
				   PlTerm_var())))
  {
  }
};


class PlTermvDomainError : public PlException
{
public:
  explicit PlTermvDomainError(size_t size, size_t n) :
    // TODO: Use PL_domain_error() or lazy PlTerm_atom()
    PlException(PlCompound("error",
			   PlTermv(PlCompound("domain_error",
					      PlTermv(PlCompound("argv",
								 PlTermv(PlTerm_integer(size))),
						      PlTerm_integer(n))),
				   PlTerm_var())))
  {
  }
};


class PlInstantiationError : public PlException
{
public:
  explicit PlInstantiationError(const PlTerm& t) :
    // TODO: Use PL_instantiation_error() or lazy PlTerm_atom()
    PlException(t.is_variable() ?
	      PlCompound("error",
			 PlTermv(PlTerm_atom("instantiation_error"), t))
		: t) {}
};


class PlExistenceError : public PlException
{
public:
  explicit PlExistenceError(const PlTerm& t)
    : PlException(t) {}

  explicit PlExistenceError(const char *type, PlTerm actual) :
    // TODO: Use PL_existence_error() or lazy PlTerm_atom()
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
    // TODO: Use PL_permission_error() or lazy PlTerm_atom()
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
  explicit PlResourceError(const char *resource) :
    // TODO: Use PL_resource_error() or lazy PlTerm_atom()
    PlException(PlCompound("error",
			   PlTermv(PlCompound("resource_error",
					      PlTermv(PlTerm_atom(resource))),
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
    throw PlFail(); // Gets fatal error or PlResourceError("memory")
}

inline
PlFunctor::PlFunctor(const std::string& name, size_t arity)
  : WrappedC<functor_t>(null)
{ PlAtom a(name);
  C_ = PL_new_functor(a.C_, arity);
  PL_unregister_atom(a.C_);
  verify();
}

inline
PlFunctor::PlFunctor(const std::wstring& name, size_t arity)
  : WrappedC<functor_t>(null)
{ PlAtom a(name);
  C_ = PL_new_functor(a.C_, arity);
  PL_unregister_atom(a.C_);
  verify();
}

inline PlAtom
PlFunctor::name() const
{ return PlAtom(PL_functor_name(C_));
}


		 /*******************************
		 *	ATOM IMPLEMENTATION	*
		 *******************************/

inline const std::wstring
PlAtom::as_wstring() const
{ PlStringBuffers _string_buffers;
  size_t len;
  const wchar_t *s = PL_atom_wchars(C_, &len);
  return std::wstring(s, len);
  }


		 /*******************************
		 *	    TERM (BODY)		*
		 *******************************/

					/* PlTerm --> C */

inline std::string
PlTerm::as_string(PlEncoding enc) const
{ return get_nchars(CVT_ALL|CVT_WRITEQ|BUF_STACK|static_cast<unsigned int>(enc));
}

inline std::wstring
PlTerm::as_wstring() const
{ wchar_t *s;
  size_t len;
  PlStringBuffers _string_buffers;
  if ( PL_get_wchars(C_, &len, &s, CVT_ALL|CVT_WRITEQ|BUF_STACK|CVT_EXCEPTION) )
    return std::wstring(s, len);
  throw PlFail();
}

inline void
PlTerm::as_nil() const
{ PlCheck(PL_get_nil_ex(C_));
}

inline double
PlTerm::as_float() const
{ double v;
  PlCheck(PL_get_float_ex(C_, &v));
  return v;
}

inline PlAtom
PlTerm::as_atom() const
{ atom_t v;
  PlCheck(PL_get_atom_ex(C_, &v));
  return PlAtom(v);
}

inline void *
PlTerm::as_pointer() const
{ void *ptr;
  PlCheck(PL_get_pointer_ex(C_, &ptr));
  return ptr;
}

inline record_t
PlTerm::record() const
{ record_t rec = PL_record(C_);
  if ( rec )
    return rec;
  throw PlFail();
}

		 /*******************************
		 *             LISTS		*
		 *******************************/

class PlTerm_tail : public PlTerm
{
public:
  explicit PlTerm_tail(const PlTerm& l)
  { if ( l.is_variable() || l.is_list() )
    { C_ = l.copy_term_ref().C_;
      if ( is_null() )
	throw PlFail(); // Raises resource exception
    } else
      throw PlTypeError("list", l);
  }

					/* building */
  [[nodiscard]] bool append(const PlTerm& e)
  { // TODO: PlTerm_var tmp, ex; replace PL_unify_*() with unify_*() methods
    term_t tmp;
    if ( (tmp = PL_new_term_ref()) &&
	 PL_unify_list(C_, tmp, C_) &&
	 PL_unify(tmp, e.C_) )
    { PL_reset_term_refs(tmp);
      return true;
    }

    return false;
  }

  [[nodiscard]] bool close()
  { return unify_nil();
  }

					/* enumerating */
  [[nodiscard]] bool next(PlTerm& t)
  { if ( PL_get_list(C_, t.C_, C_) )
      return true;

    if ( PL_get_nil(C_) )
      return false;

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
  { PlCheck(PL_register_foreign_in_module(module, name, arity, reinterpret_cast<pl_function_t>(f), PL_FA_VARARGS));
  }

  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0))
  { PlCheck(PL_register_foreign_in_module(module, name, 1, reinterpret_cast<pl_function_t>(f), 0));
  }
  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1))
  { PlCheck(PL_register_foreign_in_module(module, name, 2, reinterpret_cast<pl_function_t>(f), 0));
  }
  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1, PlTerm a2))
  { PlCheck(PL_register_foreign_in_module(module, name, 3, reinterpret_cast<pl_function_t>(f), 0));
  }

  // For C-style calls - needed to support a test case
  PlRegister(const char *module, const char *name, foreign_t (*f)(term_t a0))
  { PlCheck(PL_register_foreign_in_module(module, name, 1, reinterpret_cast<pl_function_t>(f), 0));
  }

  // for non-deterministic calls
  PlRegister(const char *module, const char *name, int arity,
	     foreign_t (f)(term_t t0, int a, control_t ctx), short flags)
  { PlCheck(PL_register_foreign_in_module(module, name, arity, reinterpret_cast<pl_function_t>(f), flags));
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
    : fid_(PL_open_foreign_frame())
  { verify();
  }

  ~PlFrame()
  { PL_close_foreign_frame(fid_);
  }

  void rewind()
  { PL_rewind_foreign_frame(fid_);
  }

private:
  void verify()
  { if ( fid_ == static_cast<fid_t>(0) )
      throw PlFail();
  }
};

[[nodiscard]] inline bool PlRewindOnFail(std::function<bool()> f)
{ PlFrame frame;
  bool rc = f();
  if ( !rc )
    frame.rewind();
  return rc;
}

class PlQuery
{
private:
  qid_t qid_;

public:
  PlQuery(PlPredicate pred, const PlTermv& av, int flags = PL_Q_PASS_EXCEPTION)
    : qid_(PL_open_query(static_cast<module_t>(0), flags, pred.C_, av.termv()))
  { verify();
  }
  // TODO: PlQuery(const wstring& ...)
  // TODO: PlQuery({PlAtom,PlFunctor,PlPredicate} ...)
  PlQuery(const std::string& name, const PlTermv& av, int flags = PL_Q_PASS_EXCEPTION)
    : qid_(PL_open_query(static_cast<module_t>(0),
			 flags,
			 PlPredicate(PlFunctor(name, av.size())).C_,
			 av.termv()))
  { verify();
  }
  // TODO; Should resolve module only once.
  PlQuery(const std::string& module, const std::string& name, const PlTermv& av, int flags = PL_Q_PASS_EXCEPTION)
    : qid_(PL_open_query(PlModule(module).C_,
			 flags,
			 PlPredicate(PlFunctor(name, av.size()),
				     PlModule(module)).C_,
			 av.termv()))
  { verify();
  }

  // The return code from next_solution can be (if called with PL_Q_EXT_STATUS):
  //    TRUE
  //    FALSE
  //    PL_S_EXCEPTION
  //    PL_S_FALSE
  //    PL_S_TRUE
  //    PL_S_LAST
  // Because of this, you shouldn't use PlCheck(q.next_solution())
  [[nodiscard]] int next_solution();

  void cut()
  { qid_t qid_orig =  qid_;
    qid_ = 0;
    if ( qid_orig )
      PlCheck(PL_cut_query(qid_orig));  // rc: exception occurred in a cleanup handler
  }

  void close_destroy()
  { qid_t qid_orig =  qid_;
    qid_ = 0;
    if ( qid_orig )
      PlCheck(PL_close_query(qid_orig));  // rc: exception occurred in a cleanup handler
  }

  ~PlQuery() noexcept(false)
  { // cut() can throw a C++ exception - throwing an exception from a
    // destructor is "potentially dangerous" but it's necessary to
    // ensure proper behaviour in Prolog.
    cut(); // *not* close() - which destroys data&bindings from query
  }

private:
  void verify()
  { if ( qid_ == static_cast<qid_t>(0) )
      throw PlFail();
  }
};


// See comment about possible return values from
// PlQuery::next_solution(), which is used by PlCall().
inline int
PlCall(const std::string& predicate, const PlTermv& args, int flags = PL_Q_PASS_EXCEPTION)
{ PlQuery q(predicate, args, flags);
  return q.next_solution();
}

inline int
PlCall(const std::string& module, const std::string& predicate, const PlTermv& args, int flags = PL_Q_PASS_EXCEPTION)
{ PlQuery q(module, predicate, args, flags);
  return q.next_solution();
}

inline int
PlCall(const std::string& goal, int flags = PL_Q_PASS_EXCEPTION)
{ PlQuery q("call", PlTermv(PlCompound(goal)), flags);
  return q.next_solution();
}

inline int
PlCall(const std::wstring& goal, int flags = PL_Q_PASS_EXCEPTION)
{ PlQuery q("call", PlTermv(PlCompound(goal)), flags);
  return q.next_solution();
}

inline int
PlCall(PlTerm goal, int flags = PL_Q_PASS_EXCEPTION)
{ PlQuery q("call", PlTermv(goal), flags);
  return q.next_solution();
}



					/* compounds */

inline PlTerm
PlTerm::operator [](size_t index) const
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


inline size_t
PlTerm::arity() const
{ atom_t name;
  size_t arity;
  if ( PL_get_name_arity(C_, &name, &arity) )
    return arity;
  throw PlTypeError("compound", *this);
}


inline PlAtom
PlTerm::name() const
{ atom_t name;
  size_t arity;
  if ( PL_get_name_arity(C_, &name, &arity) )
    return PlAtom(name);
  throw PlTypeError("compound", *this);
}

inline bool
PlTerm::name_arity(PlAtom *name, size_t *arity) const
{ atom_t name_a;
  if ( PL_get_name_arity(C_, &name_a, arity) )
  { if ( name )
      *name = PlAtom(name_a);
    return true;
  }
  return false;
}

inline bool
PlTerm::chkex(int rc)
{ if ( rc )
    return rc;
  throw PlFail();
}


					/* comparison */


inline bool PlTerm::operator ==(int64_t v) const
{ int64_t v0;

  if ( PL_get_int64(C_, &v0) )
    return v0 == v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator !=(int64_t v) const
{ int64_t v0;

  if ( PL_get_int64(C_, &v0) )
    return v0 != v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator <(int64_t v) const
{ int64_t v0;

  if ( PL_get_int64(C_, &v0) )
    return v0 < v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator >(int64_t v) const
{ int64_t v0;

  if ( PL_get_int64(C_, &v0) )
    return v0 > v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator <=(int64_t v) const
{ int64_t v0;

  if ( PL_get_int64(C_, &v0) )
    return v0 <= v;

  throw PlTypeError("integer", *this);
}

inline bool PlTerm::operator >=(int64_t v) const
{ int64_t v0;

  if ( PL_get_int64(C_, &v0) )
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
PlCompound::PlCompound(const wchar_t *text)
{ term_t t = PL_new_term_ref();

  PlCheck(PL_wchars_to_term(text, t));

  PlPutTerm(C_, t);
}

inline
PlCompound::PlCompound(const std::string& text, PlEncoding enc)
{ term_t t = PL_new_term_ref();
  if ( t == PlTerm::null )
    throw PlFail();

  // TODO: PL_put_term_from_chars() should take an unsigned int flags
  PlCheck(PL_put_term_from_chars(t, enc, text.size(), text.data()));

  PlPutTerm(C_, t);
}

inline
PlCompound::PlCompound(const std::wstring& text)
{ term_t t = PL_new_term_ref();
  if ( ! t )
    throw PlFail();

  // TODO: what is wchar_t equivalent of PL_put_term_from_chars()?
  PlCheck(PL_wchars_to_term(text.c_str(), t)); // TODO: use text.size()

  PlPutTerm(C_, t);
}

inline
PlCompound::PlCompound(const char *functor, const PlTermv& args)
{ PlCheck(PL_cons_functor_v(C_,
			    // TODO: throw if PL_new_functor() returns 0
			    PL_new_functor(PL_new_atom(functor), args.size()),
			    args.termv()));
}

inline
PlCompound::PlCompound(const wchar_t *functor, const PlTermv& args)
{ PlCheck(PL_cons_functor_v(
			    C_,
			    // TODO: throw if PL_new_functor() returns 0
			    PL_new_functor(PL_new_atom_wchars(wcslen(functor), functor),
					   args.size()),
			    args.termv()));
}

inline
PlCompound::PlCompound(const std::string& functor, const PlTermv& args)
{ PlCheck(PL_cons_functor_v(C_,
			    // TODO: throw if PL_new_functor() returns 0
			    PL_new_functor(PL_new_atom_nchars(functor.size(), functor.data()), args.size()),
			    args.termv()));
}

inline
PlCompound::PlCompound(const std::wstring& functor, const PlTermv& args)
{ PlCheck(PL_cons_functor_v(C_,
			    // TODO: throw if PL_new_functor() returns 0
			    PL_new_functor(PL_new_atom_wchars(functor.size(), functor.data()), args.size()),
			    args.termv()));
}

		 /*******************************
		 *         TERMV (BODY)         *
		 *******************************/


inline PlTermv::PlTermv(const PlAtom& a)
  : size_(1),
    a0_(PlTerm_atom(a).C_)
{ if ( !a0_ )
    throw PlFail();
}

inline PlTermv::PlTermv(const PlTerm& m0)
  : size_(1),
    a0_(m0.C_)
{ if ( !a0_ )
    throw PlFail();
}

inline PlTermv::PlTermv(const PlTerm& m0, const PlTerm& m1)
  : size_(2),
    a0_(PL_new_term_refs(2))
{ if ( !a0_ )
    throw PlFail();
  PlPutTerm(a0_+0, m0.C_);
  PlPutTerm(a0_+1, m1.C_);
}

inline PlTermv::PlTermv(const PlTerm& m0, const PlTerm& m1, const PlTerm& m2)
  : size_(3),
    a0_(PL_new_term_refs(3))
{ if ( !a0_ )
    throw PlFail();
  PlPutTerm(a0_+0, m0.C_);
  PlPutTerm(a0_+1, m1.C_);
  PlPutTerm(a0_+2, m2.C_);
}

inline PlTermv::PlTermv(const PlTerm& m0, const PlTerm& m1, const PlTerm& m2, const PlTerm& m3)
  : size_(4),
    a0_(PL_new_term_refs(4))
{ if ( !a0_ )
    throw PlFail();
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
    throw PlFail();
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

inline PlTerm
PlException::string_term() const
{ PlFrame fr;
#ifdef USE_PRINT_MESSAGE
  PlTermv av(2);

  PlCheck(av[0].unify_term(PlCompound("print_message",
				      PlTermv("error", *this))));
  PlQuery q("$write_on_string", av);
  if ( q.next_solution() )
    return av[1];
#else
  PlTermv av(2);
  PlCheck(av[0].unify_term(*this));
  PlQuery q("$messages", "message_to_string", av);
  if ( q.next_solution() )
    return av[1];
#endif
  return PlTerm_string("[ERROR: Failed to generate message.  Internal error]");
}


		 /*******************************
		 *	    QUERY (BODY)	*
		 *******************************/

inline int
PlQuery::next_solution()
{ int rval = PL_next_solution(qid_);

  if ( !rval )
    (void)close_destroy(); // TODO: what if this creates an exception?
  return rval;
}


		 /*******************************
		 *	      ENGINE		*
		 *******************************/

class PlEngine
{
public:
  PlEngine(int argc, char **argv)
  { PlCheck(PL_initialise(argc, argv));
  }
  PlEngine(int argc, wchar_t **argv)
  { PlCheck(PL_winitialise(argc, argv));
  }

  PlEngine(char *av0)
  { av[0] = av0;
    av[1] = nullptr;

    PlCheck(PL_initialise(1, av));
  }

  PlEngine(wchar_t *av0)
  { w_av[0] = av0;
    w_av[1] = nullptr;

    PlCheck(PL_winitialise(1, w_av));
  }

  void cleanup(int status_and_flags = 0) {
    PlCheck(PL_cleanup(status_and_flags));
  }

  ~PlEngine() noexcept(false)
  { // cleanup() can throw a C++ exception - throwing an exception
    // from a destructor is "potentially dangerous" but it's necessary
    // to ensure proper behaviour in Prolog.
      cleanup();
  }

private:
  char *av[2];
  wchar_t *w_av[2];
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
