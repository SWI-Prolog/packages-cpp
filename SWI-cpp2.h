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

#ifndef _SWI_CPP2_H
#define _SWI_CPP2_H

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <climits>
#include <cstdint>
#include <cstring>
#include <cwchar>
#include <functional>
#include <string>
#include <cassert>
#include <memory>
#include <typeinfo>

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

#if !(defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__))
#include <malloc.h>
#endif

class PlAtom;
class PlTerm;
class PlTermv;
class PlRecord;
class PlRecordExternalCopy;
class PlBlob;


// PlExceptionBase is used for try-catch that handles the exceptions
// defined in this header file but excludes the standard C++
// exceptions.
class PlExceptionBase : public std::exception
{
};

// PlExceptionFail is for PlFail and PlExceptionFail in
// PREDICATE_CATCH but excludes PlException

class PlExceptionFailBase : PlExceptionBase
{
};

// PlFail is a pseudo-exception for quick exit on failure, for use by
// the PlTerm::unify methods and PlQuery::next_solution().  This is
// special-cased in the PREDICATE et al macros.  Note that it is *not*
// a subclass of PlException. See the documentation for more details
// on how this works with returning Prolog failure and returning
// exceptions.
class PlFail : public PlExceptionFailBase
{
public:
  explicit PlFail() {}

  virtual const char* what() const throw() override
  { return "PlFail";
  }
};

// PlExceptionFail is a variant of PlFail, for when a resource error
// happens and we can't use PlException (because we're out of
// resources and therefore can't create any more terms).
class PlExceptionFail : public PlExceptionFailBase
{
public:
  explicit PlExceptionFail() {}

  virtual const char* what() const throw() override
  { return "PlExceptionFail";
  }
};


// Check the return code; if there's a Prolog exception, throw
// PlException else return the rc. If the rc is FALSE (e.g., from
// PL_unify_*() or PL_next_solution(), that rc is returned; you might
// wish to wrap the call in PlCheckFail().
template<typename C_t> [[nodiscard]] C_t PlWrap(C_t rc, qid_t qid = 0);

// As PlWrap, but always throw an exception for zero rc.
// This is for functions that report errors but don't have an
// indication of "fail" - that is, almost everything except for
// functions like PL_unify_*() or PL_next_solution().
template<typename C_t> void PlEx(C_t rc, qid_t qid = 0);

// Check the return code: if "false", throw PlFail.
inline void PlCheckFail(bool rc);

#include "SWI-cpp2-plx.h"



		 /*******************************
		 * COMMON OPERATIONS (TEMPLATE) *
		 *******************************/

template <typename C_t> class WrappedC
{
private:
  C_t C_ = null; // The wrapped value - access by unwrap(), unwrap_as_ptr(), or PlUnwrapAsPtr()

public:
  static constexpr C_t null = C_t();
  bool is_null()  const { return unwrap() == null; }
  bool not_null() const { return unwrap() != null; }
  void set_null() { C_ = null; }
  C_t unwrap() const { return C_; }
  C_t *unwrap_as_ptr() { return &C_; }

  friend C_t *PlUnwrapAsPtr(WrappedC<C_t>* obj) { return obj ? obj->unwrap_as_ptr() : nullptr; }

  explicit WrappedC(C_t v)
    : C_(v) { }

  WrappedC(            const WrappedC<C_t>&) = default;
  WrappedC& operator =(const WrappedC<C_t>&) = default;
  // This simple wrapper class doesn't need a move constructor or
  // move operator =.

  ~WrappedC() { }

  operator bool() const = delete; // Use not_null(), is_null() instead
  bool operator ==(WrappedC<C_t> o) const { return C_ == o.C_; }
  bool operator !=(WrappedC<C_t> o) const { return C_ != o.C_; }

  // reset() is common with "smart pointers"; wrapped atom_t, term_t,
  // etc. aren't "smart" in the same sense, but the objects they refer
  // to are garbage collected by Prolog and some care is needed to
  // ensure they have appropriate reference counts (e.g.,
  // PlAtom::register_ref() and PlTerm::record()).
  void reset() { C_ = null; } // same as set_null()
  void reset(WrappedC<C_t> v) { C_ = v.unwrap(); }
  void reset_wrapped(C_t v) { C_ = v; }
};

// TODO: use PlEncoding wherever a method takes a char* or std::string.
// TODO: #define SWI_DEFAULT_TEXT_ENCODING EncUTF8
//       (set outside SWI-cpp2.h, with an appropriate default)
// For the various "get/put/unify string" methods:
typedef enum class PlEncoding
{ Latin1 = REP_ISO_LATIN_1,
  UTF8   = REP_UTF8,
  Locale = REP_MB
} PlEncoding;
static constexpr PlEncoding ENC_INPUT = PlEncoding::Latin1; // TODO: EncUTF8?
static constexpr PlEncoding ENC_OUTPUT = PlEncoding::Locale;


		 /*******************************
		 *  PL_STRINGS_{MARK,RELEASE}   *
		 *******************************/

class PlStringBuffers
{ // This class depends on the details of PL_STRINGS_MARK() and PL_STRINGS_RELEASE().
private:
  buf_mark_t __PL_mark;

public:
  explicit PlStringBuffers()
  { Plx_mark_string_buffers(&__PL_mark);
  }

  ~PlStringBuffers()
  { Plx_release_string_buffers_from_mark(__PL_mark);
  }
};

		 /*******************************
		 *  PL_{acquire,release}_stream *
		 *******************************/

// TODO: document this.
// In brief, this is RAII for PL_{acquire,release_stream}.
// To use:
//    PlAcquireStream strm(other_stream);
//    Sfprintf(strm, ...);

class PlAcquireStream
{
public:
  explicit PlAcquireStream(IOSTREAM *s)
    : s_(Plx_acquire_stream(s))
  { PlEx<bool>(s_ != nullptr);
  }

  operator IOSTREAM *()
  { return s_;
  }

  // The following has an implicit throw of PlFail if
  // PL_release_stream() detects an IO error had happened
  ~PlAcquireStream()
  { if ( s_ )
    { Plx_release_stream(s_);
   }
  }

private:
  IOSTREAM *s_ = nullptr;
};

		 /*******************************
		 *	 PROLOG CONSTANTS	*
		 *******************************/

class PlAtom : public WrappedC<atom_t>
{
public:
  explicit PlAtom(atom_t v)
    : WrappedC<atom_t>(v) { }
  explicit PlAtom(const std::string& text)
    : WrappedC<atom_t>(Plx_new_atom_nchars(text.size(), text.data()))
  { }
  explicit PlAtom(const std::wstring& text)
    : WrappedC<atom_t>(Plx_new_atom_wchars(text.size(), text.data()))
  { }
  explicit PlAtom(const pl_wchar_t *text)
    : WrappedC<atom_t>(Plx_new_atom_wchars(static_cast<size_t>(-1), text))
  { }
  explicit PlAtom(const char *text)
    : WrappedC<atom_t>(Plx_new_atom_nchars(static_cast<size_t>(-1), text))
  { }
  explicit PlAtom(PlEncoding rep, size_t len, const char *s)
    : WrappedC<atom_t>(Plx_new_atom_mbchars(static_cast<int>(rep), len, s))
  { }
  explicit PlAtom(PlEncoding rep, std::string& text) // TODO: rep as optional with default ENC_INPUT
    : WrappedC<atom_t>(Plx_new_atom_mbchars(static_cast<int>(rep), text.size(), text.data()))
  { }

  const std::string mbchars(unsigned int flags) const;

  const std::wstring wchars() const;

  const std::string as_string(PlEncoding enc=ENC_OUTPUT) const
  { return mbchars(static_cast<unsigned int>(enc));
  }

  const std::wstring as_wstring() const
  { return wchars();
  }

  [[nodiscard]] int write(IOSTREAM *s, int flags) const;

  // TODO: operator ==(PlAtom) is defined by WrappedC<C_t> but
  //       the following is needed to avoid amiguous overload:
  bool operator ==(PlAtom to) const { return unwrap() == to.unwrap(); }
  bool operator !=(PlAtom to) const { return unwrap() != to.unwrap(); }

  [[deprecated("use as_string() and std::string::operator==() or ==PlAtom")]]   bool operator ==(const char *s)         const { return eq(s); }
  [[deprecated("use as_string() and std::string::operator==() or ==PlAtom")]]   bool operator ==(const wchar_t *s)      const { return eq(s); }
  [[deprecated("use as_string() and std::string::operator==() or ==PlAtom")]]   bool operator ==(const std::string& s)  const { return eq(s); }
  [[deprecated("use as_swtring() and std::wstring::operator==() or ==PlAtom")]] bool operator ==(const std::wstring& s) const { return eq(s); }
  [[deprecated("use PlAtom instead of atom_t")]]                                bool operator ==(atom_t to)             const { return unwrap() == to; }

  [[deprecated("use as_string() and std::string::operator!=() or !=PlAtom")]]   bool operator !=(const char *s)         const { return !eq(s); }
  [[deprecated("use as_string() and std::string::operator!=() or !=PlAtom")]]   bool operator !=(const wchar_t *s)      const { return !eq(s); }
  [[deprecated("use PlAtom instead of atom_t")]]                                bool operator !=(atom_t to)             const { return unwrap() != to; }

  // TODO: when C++17 becomes standard, rename register_ref() to register().

  void register_ref() const
  { Plx_register_atom(unwrap());
  }

  void unregister_ref() const
  { Plx_unregister_atom(unwrap());
  }

  // TODO: replace blob_data() with C++ interface to blobs
  void* blob_data(size_t *len, struct PL_blob_t **type) const
  { return Plx_blob_data(unwrap(), len, type);
  }

private:
  bool eq(const char *s) const // used by deprecated operator ==
  { PlStringBuffers _string_buffers;
    return strcmp(s, Plx_atom_nchars(unwrap(), nullptr)) == 0;
  }
  bool eq(const wchar_t *s) const // used by deprecated operator ==
  { PlStringBuffers _string_buffers;
    return wcscmp(s, Plx_atom_wchars(unwrap(), nullptr)) == 0;
  }
  bool eq(const std::string& s) const // used by deprecated operator ==
  { PlStringBuffers _string_buffers;
    size_t len;
    const char* s0 = Plx_atom_nchars(unwrap(), &len);
    return std::string(s0, len) == s;
  }
  bool eq(const std::wstring& s) const // used by deprecated operator ==
  { PlStringBuffers _string_buffers;
    size_t len;
    const wchar_t* s0 = Plx_atom_wchars(unwrap(), &len);
    return std::wstring(s0, len) == s;
  }
};

class PlFunctor : public WrappedC<functor_t>
{
public:
  explicit PlFunctor(functor_t v)
    : WrappedC<functor_t>(v) { }

  // PlFunctor(const char*) is handled by std::string constructor

  // TODO: add encoding to string
  explicit PlFunctor(const std::string& name, size_t arity)
    : WrappedC<functor_t>(null)
  { PlAtom a(name);
    reset_wrapped(Plx_new_functor(a.unwrap(), arity));
    Plx_unregister_atom(a.unwrap());
  }

  explicit PlFunctor(const std::wstring& name, size_t arity)
  : WrappedC<functor_t>(null)
  { PlAtom a(name);
    reset_wrapped(Plx_new_functor(a.unwrap(), arity));
    Plx_unregister_atom(a.unwrap());
  }

  explicit PlFunctor(PlAtom name, size_t arity)
    : WrappedC<functor_t>(Plx_new_functor(name.unwrap(), arity)) { }

  [[deprecated("use PlPredicate")]] predicate_t pred(module_t m) const {
    predicate_t p = Plx_pred(unwrap(), m);
    return p;
  }

  PlAtom name() const { return PlAtom(Plx_functor_name(unwrap())); }
  size_t arity() const { return Plx_functor_arity(unwrap()); }
};


class PlModule : public WrappedC<module_t>
{
public:
  explicit PlModule(module_t m = 0)
    : WrappedC<module_t>(m) { }
  explicit PlModule(const std::string& name)
    : WrappedC<module_t>(Plx_new_module(PlAtom(name).unwrap()))
  { }
  explicit PlModule(PlAtom name)
    : WrappedC<module_t>(Plx_new_module(name.unwrap()))
  { }

  PlAtom module_name() const
  { return PlAtom(Plx_module_name(unwrap()));
  }
  // TODO: strip_module
};


		 /*******************************
		 *     GENERIC PROLOG TERM	*
		 *******************************/

class PlTerm : public WrappedC<term_t>
{
protected:
  explicit PlTerm()
    : WrappedC<term_t>(Plx_new_term_ref())
  { }

public:
  explicit PlTerm(PlAtom a)
    : WrappedC<term_t>(Plx_new_term_ref())
  { Plx_put_atom(unwrap(), a.unwrap());
  }

  // The following constructor is the same as to PlTerm_term_t(); the
  // latter is for consistency with other constructors
  // (PlTerm_integer(), etc.)  and the former is to make some template
  // programming eaiser.
  explicit PlTerm(term_t t)
    : WrappedC<term_t>(t)
  { }

  explicit PlTerm(const PlRecord& r);

  PlTerm(const PlTerm&) = default;

  // TODO: PlTerm& operator =(const PlTerm&) = delete; // TODO: when the deprecated items below are removed

  [[nodiscard]] bool get_atom(PlAtom *a) const { return Plx_get_atom(unwrap(), PlUnwrapAsPtr(a)); }
  [[nodiscard]] bool get_bool(int *value) const { return Plx_get_bool(unwrap(), value); }
  [[deprecated("use get_chars(flags) returning std::string")]]
  [[nodiscard]] bool get_chars(char **s, unsigned int flags) const { return Plx_get_chars(unwrap(), s, flags); }
  [[nodiscard]] bool get_list_chars(char **s, unsigned int flags) const { return Plx_get_list_chars(unwrap(), s, flags); }
  [[nodiscard]] bool get_atom_nchars(size_t *len, char **a) const { return Plx_get_atom_nchars(unwrap(), len, a); }
  [[nodiscard]] bool get_list_nchars(size_t *len, char **s, unsigned int flags) const { return Plx_get_list_nchars(unwrap(), len, s, flags); }
  [[deprecated("use get_nchars(flags) returning std::string")]]
  [[nodiscard]] bool get_nchars(size_t *len, char **s, unsigned int flags) const { return _get_nchars(len, s, flags); }
  const std::string get_nchars(unsigned int flags) const;
  [[deprecated("use get_wchars(flags) returning std::wstring")]]
  [[nodiscard]] bool get_wchars(size_t *length, pl_wchar_t **s, unsigned flags) const { return _get_wchars(length, s, flags); }
  const std::wstring get_wchars(unsigned int flags) const;
  [[nodiscard]] bool get_integer(int *i) const { return Plx_get_integer(unwrap(), i); }
  [[nodiscard]] bool get_long(long *i) const { return Plx_get_long(unwrap(), i); }
  [[nodiscard]] bool get_intptr(intptr_t *i) const { return Plx_get_intptr(unwrap(), i); }
  [[nodiscard]] bool get_pointer(void **ptr) const { return Plx_get_pointer(unwrap(), ptr); }
  [[nodiscard]] bool get_float(double *f) const { return  Plx_get_float(unwrap(), f); }
  [[nodiscard]] bool get_functor(PlFunctor *f) const { return Plx_get_functor(unwrap(), PlUnwrapAsPtr(f)); }
  [[nodiscard]] bool get_name_arity(PlAtom *name, size_t *arity) const { return Plx_get_name_arity(unwrap(), PlUnwrapAsPtr(name), arity);  }
  [[nodiscard]] bool get_compound_name_arity(PlAtom *name, size_t *arity) const { return Plx_get_compound_name_arity(unwrap(), PlUnwrapAsPtr(name), arity); }
  [[nodiscard]] bool get_module(PlModule *module) const { return Plx_get_module(unwrap(), PlUnwrapAsPtr(module)); }
  [[nodiscard]] bool get_arg(size_t index, PlTerm a) const { return Plx_get_arg(index, unwrap(), a.unwrap()); }
  [[nodiscard]] bool get_dict_key(PlAtom key, PlTerm dict, PlTerm value) const { return Plx_get_dict_key(key.unwrap(), dict.unwrap(), value.unwrap()); }
  [[nodiscard]] bool get_list(PlTerm h, PlTerm t) const { return Plx_get_list(unwrap(), h.unwrap(), t.unwrap()); }
  [[nodiscard]] bool get_head(PlTerm h) const { return Plx_get_head(unwrap(), h.unwrap()); }
  [[nodiscard]] bool get_tail(PlTerm t) const { return Plx_get_tail(unwrap(), t.unwrap()); }
  // TODO: get_mpz
  // TODO: get_mpq
  [[nodiscard]] bool get_nil() const { return Plx_get_nil(unwrap()); }
  [[nodiscard]] bool get_blob(void **blob, size_t *len, PL_blob_t **type) const { return Plx_get_blob(unwrap(), blob, len, type); }

  [[nodiscard]] bool get_file_name(char **name, int flags) const { return Plx_get_file_name(unwrap(), name, flags); }
  [[nodiscard]] bool get_file_nameW(wchar_t **name, int flags) const { return Plx_get_file_nameW(unwrap(), name, flags); }
  [[nodiscard]] const std::string get_file_name(int flags) const;
  [[nodiscard]] const std::wstring get_file_nameW(int flags) const;

  [[nodiscard]] bool get_attr(term_t a) const { return Plx_get_attr(unwrap(), a); }

  void get_atom_ex(PlAtom *a)       const { Plx_get_atom_ex(unwrap(), PlUnwrapAsPtr(a)); }
  void get_integer_ex(int *i)       const { Plx_get_integer_ex(unwrap(),i); }
  void get_long_ex(long *i)         const { Plx_get_long_ex(unwrap(), i); }
  void get_int64_ex(int64_t *i)     const { Plx_get_int64_ex(unwrap(), i); }
  void get_uint64_ex(uint64_t *i)   const { Plx_get_uint64_ex(unwrap(), i); }
  void get_intptr_ex(intptr_t *i)   const { Plx_get_intptr_ex(unwrap(), i); }
  void get_size_ex(size_t *i)       const { Plx_get_size_ex(unwrap(), i); }
  void get_bool_ex(int *i)          const { Plx_get_bool_ex(unwrap(), i); }
  void get_float_ex(double *f)      const { Plx_get_float_ex(unwrap(), f); }
  void get_char_ex(int *p, int eof) const { Plx_get_char_ex(unwrap(), p, eof); }
  void unify_bool_ex(int val)       const { Plx_unify_bool_ex(unwrap(), val); }
  void get_pointer_ex(void **addrp) const { Plx_get_pointer_ex(unwrap(), addrp); }
  bool unify_list_ex(PlTerm h, PlTerm t) const { return Plx_unify_list_ex(unwrap(), h.unwrap(), t.unwrap()); }
  void unify_nil_ex()               const { Plx_unify_nil_ex(unwrap()); }
  bool get_list_ex(PlTerm h, PlTerm t) const { return Plx_get_list_ex(unwrap(), h.unwrap(), t.unwrap()); }
  void get_nil_ex()                 const {  Plx_get_nil_ex(unwrap()); }

  int type()         const { return Plx_term_type(unwrap()); } // PL_VARIABLE, PL_ATOM, etc.
  bool is_attvar()   const { return Plx_is_attvar(unwrap()); }
  bool is_variable() const { return Plx_is_variable(unwrap()); }
  bool is_ground()   const { return Plx_is_ground(unwrap()); }
  bool is_atom()     const { return Plx_is_atom(unwrap()); }
  bool is_integer()  const { return Plx_is_integer(unwrap()); }
  bool is_string()   const { return Plx_is_string(unwrap()); }
  bool is_atom_or_string() const
  { int t = type();
    return t == PL_ATOM || t == PL_STRING;
  }
  bool is_float()    const { return Plx_is_float(unwrap()); }
  bool is_rational() const { return Plx_is_rational(unwrap()); }
  bool is_compound() const { return Plx_is_compound(unwrap()); }
  bool is_callable() const { return Plx_is_callable(unwrap()); }
  bool is_list()     const { return Plx_is_list(unwrap()); }
  bool is_dict()     const { return Plx_is_dict(unwrap()); }
  bool is_pair()     const { return Plx_is_pair(unwrap()); }
  bool is_atomic()   const { return Plx_is_atomic(unwrap()); }
  bool is_number()   const { return Plx_is_number(unwrap()); }
  bool is_acyclic()  const { return Plx_is_acyclic(unwrap()); }
  bool is_functor(PlFunctor f) const { return Plx_is_functor(unwrap(), f.unwrap()); }
  bool is_blob(PL_blob_t **type) const { return Plx_is_blob(unwrap(), type); }

  void must_be_attvar()   const;
  void must_be_variable() const;
  void must_be_ground()   const;
  void must_be_atom()     const;
  void must_be_integer()  const;
  void must_be_string()   const;
  void must_be_atom_or_string() const;
  void must_be_float()    const;
  void must_be_rational() const;
  void must_be_compound() const;
  void must_be_callable() const;
  void must_be_list()     const;
  void must_be_dict()     const;
  void must_be_pair()     const;
  void must_be_atomic()   const;
  void must_be_number()   const;
  void must_be_acyclic()  const;
  // TODO: if needed
  // void must_be_functor(PlFunctor f) const;
  // void must_be_blob(PL_blob_t **type) const;

  void put_variable()                                      { Plx_put_variable(unwrap()); }
  void put_atom(PlAtom a)                                  { Plx_put_atom(unwrap(), a.unwrap()); }
  void put_bool(int val)                                   { Plx_put_bool(unwrap(), val); }
  void put_atom_chars(const char *chars)                   { Plx_put_atom_chars(unwrap(), chars); }
  void put_string_chars(const char *chars)                 { Plx_put_string_chars(unwrap(), chars); }
  void put_chars(int flags, size_t len, const char *chars) { Plx_put_chars(unwrap(), flags, len, chars); }
  void put_list_chars(const char *chars)                   { Plx_put_list_chars(unwrap(), chars); }
  void put_list_codes(const char *chars)                   { Plx_put_list_codes(unwrap(), chars); }
  // TODO: add std::string versions of the following
  void put_atom_nchars(size_t l, const char *chars)        { Plx_put_atom_nchars(unwrap(), l, chars); }
  void put_string_nchars(size_t len, const char *chars)    { Plx_put_string_nchars(unwrap(), len, chars); }
  void put_list_nchars(size_t l, const char *chars)        { Plx_put_list_nchars(unwrap(), l, chars); }
  void put_list_ncodes(size_t l, const char *chars)        { Plx_put_list_ncodes(unwrap(), l, chars); }
  void put_integer(long i)                                 { Plx_put_integer(unwrap(), i); }
  void put_int64(int64_t i)                                { Plx_put_int64(unwrap(), i); }
  void put_uint64(uint64_t i)                              { Plx_put_uint64(unwrap(), i); }
  void put_pointer(void *ptr)                              { Plx_put_pointer(unwrap(), ptr); }
  void put_float(double f)                                 { Plx_put_float(unwrap(), f); }
  void put_functor(PlFunctor functor)                      { Plx_put_functor(unwrap(), functor.unwrap()); }
  void put_list()                                          { Plx_put_list(unwrap()); }
  void put_nil()                                           { Plx_put_nil(unwrap()); }
  void put_term(PlTerm t2)                                 { Plx_put_term(unwrap(), t2.unwrap()); }
  void put_blob( void *blob, size_t len, PL_blob_t *type)  { Plx_put_blob(unwrap(), blob, len, type); }
  PlRecord record() const;

  // TODO: When the following are implemented, add them
  //       to the deleted methods in PlTermScoped:
  // TODO: PL_put_dict(term_t t, atom_t tag, size_t len, const atom_t *keys, term_t values)
  // TODO: PL_cons_functor(term_t h, functor_t f, ...)
  // TODO: PL_cons_functor_v(term_t h, functor_t fd, term_t a0)
  // TODO: PL_cons_list(term_t l, term_t h, term_t t)

  // TODO: PL_unify_*()?
  // TODO: PL_skip_list()

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
  void integer(bool               *v) const { int v0; Plx_cvt_i_bool(unwrap(), &v0); *v = v0; }
  void integer(char               *v) const { Plx_cvt_i_char(  unwrap(), v); }
  void integer(int                *v) const { Plx_cvt_i_int(   unwrap(), v); }
  void integer(long               *v) const { Plx_cvt_i_long(  unwrap(), v); }
  void integer(long long          *v) const { Plx_cvt_i_llong( unwrap(), v); }
  void integer(short              *v) const { Plx_cvt_i_short( unwrap(), v); }
  void integer(signed char        *v) const { Plx_cvt_i_schar( unwrap(), v); }
  void integer(unsigned char      *v) const { Plx_cvt_i_uchar( unwrap(), v); }
  void integer(unsigned int       *v) const { Plx_cvt_i_uint(  unwrap(), v); }
  void integer(unsigned long      *v) const { Plx_cvt_i_ulong( unwrap(), v); }
  void integer(unsigned long long *v) const { Plx_cvt_i_ullong(unwrap(), v); }
  void integer(unsigned short     *v) const { Plx_cvt_i_ushort(unwrap(), v); }

  // All the conversion functions throw a PlTypeError exception if
  // they fail (because of the wrong Prolog type). If you want to be
  // safe, use is_XXX() first to verify the type.

  const std::string as_string(PlEncoding enc=ENC_OUTPUT) const
  { return get_nchars(CVT_ALL|CVT_WRITEQ|static_cast<unsigned int>(enc));
  }

  const std::wstring as_wstring() const
  { return get_wchars(CVT_ALL|CVT_WRITEQ);
  }

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

  PlAtom as_atom() const;
  [[nodiscard]] bool eq_if_atom(PlAtom a) const;

					/* Compounds */
  PlTerm operator [](size_t index) const;
  size_t arity() const; // throws PlTypeError if not a "compound" or atom
  PlAtom name() const; // throws PlTypeError if not a "compound" or atom
  [[nodiscard]] bool name_arity(PlAtom *name, size_t *arity) const; // name and/or arity can be nullptr
  [[nodiscard]] PlTerm copy_term_ref() const;
  void free_term_ref();
  void free_term_ref_reset();

  // The assignment operators from version 1 have been removed because
  // of possible confusion with the standard assignment and copy
  // operators. Also, they have unusual semantics; normally an
  // assignment operator would have the form
  //   PlTerm& PlTerm::operator =(const PlTerm&)
  // with implicit or explicit cast from, e.g. PlAtom to PlTerm

						/* UNIFY */
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(PlTerm t2)        const { return unify_term(t2); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(PlAtom a)         const { return unify_atom(a); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(const char *v)    const { return unify_atom(v); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(const wchar_t *v) const { return unify_atom(v); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(intptr_t v)       const { return unify_integer(v); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(double v)         const { return unify_float(v); }
  [[deprecated("use unify_*()")]] [[nodiscard]] bool operator =(PlFunctor f)      const { return unify_functor(f); }

  // All the unify_*() methods check for an exception (and throw), so
  // the return code is whether the unification succeeded or not.
  // TODO: replace PL_unify_*() with PL_unify_string() and flags, where appropriate
  // TODO: encodings for char*, std::string
  [[nodiscard]] bool unify_term(PlTerm t2)               const { return Plx_unify(unwrap(), t2.unwrap()); }
  [[nodiscard]] bool unify_atom(PlAtom a)                const { return Plx_unify_atom(unwrap(), a.unwrap()); }
  [[nodiscard]] bool unify_chars(int flags, size_t len, const char *s) const { return Plx_unify_chars(unwrap(), flags, len, s); }
  [[nodiscard]] bool unify_chars(int flags, const std::string& s) const { return Plx_unify_chars(unwrap(), flags, s.size(), s.data()); }
  [[nodiscard]] bool unify_atom(const char*           v) const { return Plx_unify_atom_chars(unwrap(), v); }
  [[nodiscard]] bool unify_atom(const wchar_t*        v) const { return Plx_unify_wchars(unwrap(), PL_ATOM, static_cast<size_t>(-1), v); }
  [[nodiscard]] bool unify_atom(const std::string&    v) const { return Plx_unify_atom_nchars(unwrap(), v.size(), v.data()); }
  [[nodiscard]] bool unify_atom(const std::wstring&   v) const { return Plx_unify_wchars(unwrap(), PL_ATOM, v.size(), v.data()); }
  [[nodiscard]] bool unify_list_codes(const char*     v) const { return Plx_unify_list_codes(unwrap(), v); } // TODO: [[deprecated]]
  [[nodiscard]] bool unify_list_chars(const char*     v) const { return Plx_unify_list_chars(unwrap(), v); } // TODO: [[deprecated]]
  // See comment with PlTerm::integer() about the overloading.
  [[nodiscard]] bool unify_integer(bool               v) const { return Plx_unify_int64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(char               v) const { return Plx_unify_int64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(int                v) const { return Plx_unify_int64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(long               v) const { return Plx_unify_int64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(long long          v) const { return Plx_unify_int64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(short              v) const { return Plx_unify_int64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(signed char        v) const { return Plx_unify_int64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(unsigned char      v) const { return Plx_unify_uint64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(unsigned int       v) const { return Plx_unify_uint64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(unsigned long      v) const { return Plx_unify_uint64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(unsigned long long v) const { return Plx_unify_uint64(unwrap(), v); }
  [[nodiscard]] bool unify_integer(unsigned short     v) const { return Plx_unify_uint64(unwrap(), v); }
  [[nodiscard]] bool unify_float(double               v) const { return Plx_unify_float(unwrap(), v); }
  [[nodiscard]] bool unify_string(const std::string&  v) const { return Plx_unify_string_nchars(unwrap(), v.size(), v.data()); }
  [[nodiscard]] bool unify_wstring(const std::wstring& v) const { return Plx_unify_wchars(unwrap(), PL_STRING, v.size(), v.data()); }
  [[nodiscard]] bool unify_functor(PlFunctor          f) const { return Plx_unify_functor(unwrap(), f.unwrap()); }
  [[nodiscard]] bool unify_pointer(void *ptr)            const { return Plx_unify_pointer(unwrap(), ptr); } // TODO: replace with C++ interface to blobs
  [[nodiscard]] bool unify_nil()                         const { return Plx_unify_nil(unwrap()); }
  [[nodiscard]] bool unify_list(PlTerm h, PlTerm t)      const { return Plx_unify_list(unwrap(), h.unwrap(), t.unwrap()); }
  [[nodiscard]] bool unify_bool(bool val)                const { return Plx_unify_bool(unwrap(), val); }


  [[nodiscard]] bool unify_blob(const PlBlob* blob) const;
  [[nodiscard]] bool unify_blob(std::unique_ptr<PlBlob>* blob) const;
  [[nodiscard]] bool unify_blob(const void *blob, size_t len, const PL_blob_t *type) const
  { return Plx_unify_blob(unwrap(), const_cast<void*>(blob), len, const_cast<PL_blob_t*>(type)); }

  // TODO: PL_unify_mpz(), PL_unify_mpq()

					/* Comparison standard order terms */
  [[nodiscard]] int compare(PlTerm t2) const { return Plx_compare(unwrap(), t2.unwrap()); }
  bool operator ==(PlTerm t2) const { return compare(t2) == 0; }
  bool operator !=(PlTerm t2) const { return compare(t2) != 0; }
  bool operator < (PlTerm t2) const { return compare(t2) <  0; }
  bool operator > (PlTerm t2) const { return compare(t2) >  0; }
  bool operator <=(PlTerm t2) const { return compare(t2) <= 0; }
  bool operator >=(PlTerm t2) const { return compare(t2) >= 0; }
					/* comparison (long) */
  [[deprecated("use as_int64_t() and int64_t::operator==()")]] bool operator ==(int64_t v) const;
  [[deprecated("use as_int64_t() and int64_t::operator!=()")]] bool operator !=(int64_t v) const;
  [[deprecated("use as_int64_t() and int64_t::operator<()")]]  bool operator < (int64_t v) const;
  [[deprecated("use as_int64_t() and int64_t::operator>()")]]  bool operator > (int64_t v) const;
  [[deprecated("use as_int64_t() and int64_t::operator<=()")]] bool operator <=(int64_t v) const;
  [[deprecated("use as_int64_t() and int64_t::operator>=()")]] bool operator >=(int64_t v) const;

					/* comparison (atom, string) */
  [[deprecated("use as_string() and std::string::operator==()")]]    bool operator ==(const char *s)         const { return eq(s); }
  [[deprecated("use as_string() and std::string::operator==()")]]    bool operator ==(const wchar_t *s)      const { return eq(s); }
  [[deprecated("use as_wstring() and std::string::operator==()")]]   bool operator ==(const std::string& s)  const { return eq(s); }
  [[deprecated("use as_wstring() and std::wstring::operator==()")]]  bool operator ==(const std::wstring& s) const { return eq(s); }
  [[deprecated("use as_atom()")]]                                    bool operator ==(PlAtom a)              const { return eq(a); }

  [[deprecated("use as_string() and std::string::operator==()")]]    bool operator !=(const char *s)         const { return !eq(s); }
  [[deprecated("use as_wstring() and std::wstring::operator==()")]]  bool operator !=(const wchar_t *s)      const { return !eq(s); }
  [[deprecated("use as_string() and std::string::operator==()")]]    bool operator !=(const std::string& s)  const { return !eq(s); }
  [[deprecated("use as_wstring() and std::wstring::operator==()")]]  bool operator !=(const std::wstring& s) const { return !eq(s); }
  [[deprecated("use as_atom()")]]                                    bool operator !=(PlAtom a)              const { return !eq(a); }

  // E.g.: t.write(Serror, 1200, PL_WRT_QUOTED);
  [[nodiscard]] int write(IOSTREAM *s, int precedence, int flags) const
  { // TODO: move "&~PL_WRITE_NEWLINE" to PL_write_term() and update foreign.doc
    return Plx_write_term(s, unwrap(), precedence, flags & ~PL_WRT_NEWLINE);
  }

  void reset_term_refs() { Plx_reset_term_refs(unwrap()); }

  bool call(PlModule module = PlModule()) const { return Plx_call(unwrap(), module.unwrap()); }

private:
  bool eq(const char *s) const;
  bool eq(const wchar_t *s) const;
  bool eq(const std::string& s) const;
  bool eq(const std::wstring& s) const;
  bool eq(PlAtom a) const;

  // deprecated get_chars get_nchars(), get_wchars() are used internally (and safely):
  [[nodiscard]] bool _get_chars(char **s, unsigned int flags) const { return Plx_get_chars(unwrap(), s, flags); }
  [[nodiscard]] bool _get_nchars(size_t *len, char **s, unsigned int flags) const { return Plx_get_nchars(unwrap(), len, s, flags); }
  [[nodiscard]] bool _get_wchars(size_t *length, pl_wchar_t **s, unsigned flags) const { return Plx_get_wchars(unwrap(), length, s, flags); }
};


// PlTermScoped is an *experimental* inteface, which may change
// in the future. It implements a PlTerm that is automatically
// freed when it goes out of scope. The API is similar to
// std::unique_ptr.

class PlTermScoped : public PlTerm
{
public:
  explicit PlTermScoped()
  : PlTerm()
  { }

  explicit PlTermScoped(PlTerm t)
    : PlTerm(t.copy_term_ref())
  { }

  explicit PlTermScoped(term_t t)
    : PlTerm(Plx_copy_term_ref(t))
  { }

  explicit PlTermScoped(PlTermScoped&& moving) noexcept
    : PlTerm(moving)
  { moving.reset();
  }

  PlTermScoped& operator=(PlTermScoped&& moving) noexcept
  { if ( this != &moving )
    { reset(moving);
      moving.reset();
    }
    return *this;
  }

  PlTermScoped(const PlTermScoped&) = delete;
  PlTermScoped& operator=(PlTermScoped const&) = delete;

  ~PlTermScoped()
  { free_term_ref_reset(); // TODO: reset() isn't needed?
  }

  void reset()
  { free_term_ref();
    PlTerm::reset();
  }

  void reset(PlTerm src)
  { free_term_ref();
    PlTerm::reset(src);
  }

  PlTerm get() const noexcept { return PlTerm(unwrap()); }

  PlTermScoped release() noexcept
  { term_t t = unwrap();
    reset();
    return PlTermScoped(t);
  }

  void swap(PlTermScoped& src) noexcept
  { // std::swap(*this, src);
    term_t this_unwrap = unwrap();
    // Must use PlTerm::reset() to avoid call to PL_free_term_ref():
    static_cast<PlTerm*>(this)->reset(PlTerm(src.unwrap()));
    static_cast<PlTerm>(src).reset(PlTerm(this_unwrap));
  }

  // The put_*() and cons_*() methods are incompatible with the
  // call to PL_free_term_ref() in the destructor.
  void put_variable() = delete;
  void put_atom(PlAtom a) = delete;
  void put_bool(int val) = delete;
  void put_atom_chars(const char *chars) = delete;
  void put_string_chars(const char *chars) = delete;
  void put_chars(int flags, size_t len, const char *chars) = delete;
  void put_list_chars(const char *chars) = delete;
  void put_list_codes(const char *chars) = delete;
  void put_atom_nchars(size_t l, const char *chars) = delete;
  void put_string_nchars(size_t len, const char *chars) = delete;
  void put_list_nchars(size_t l, const char *chars) = delete;
  void put_list_ncodes(size_t l, const char *chars) = delete;
  void put_integer(long i) = delete;
  void put_pointer(void *ptr) = delete;
  void put_float(double f) = delete;
  void put_functor(PlFunctor functor) = delete;
  void put_list() = delete;
  void put_nil() = delete;
  void put_term(PlTerm t2) = delete;
  void put_blob( void *blob, size_t len, PL_blob_t *type) = delete;
};

// TODO: verify that this is the right way to specialize
//       std::swap()'s use with std containers:
namespace std
{
  inline
  void swap(PlTermScoped& lhs, PlTermScoped& rhs) noexcept
  { lhs.swap(rhs);
  }
}


class PlTerm_atom : public PlTerm
{
public:
  // TODO: Add encoding for char*, std::string.
  //       For now, these are safe only with ASCII (PlEncoding::Latin1):
  explicit PlTerm_atom(atom_t a)                 { Plx_put_atom(unwrap(), a); }
  explicit PlTerm_atom(PlAtom a)                 { Plx_put_atom(unwrap(), a.unwrap()); }
  explicit PlTerm_atom(const char *text)         { Plx_put_atom_chars(unwrap(), text); } // TODO: add encoding
  explicit PlTerm_atom(const wchar_t *text)      { PlEx<bool>(Plx_unify_wchars(unwrap(), PL_ATOM, static_cast<size_t>(-1), text)); }
  explicit PlTerm_atom(const std::string& text)  { Plx_put_atom_nchars(unwrap(), text.size(), text.data()); } // TODO: add encoding
  explicit PlTerm_atom(const std::wstring& text) { PlEx<int>(Plx_unify_wchars(unwrap(), PL_ATOM, text.size(), text.data())); }
};

[[nodiscard]]
inline int
PlAtom::write(IOSTREAM *s, int flags) const {
  // TODO: move "&~PL_WRITE_NEWLINE" to PL_write_term() and update foreign.doc
  return PlTerm_atom(*this).write(s, 1200, flags & ~PL_WRT_NEWLINE);
}

class PlTerm_var : public PlTerm
{
public:
  explicit PlTerm_var() { } // PlTerm() calls Pl_new_term_ref()
};

class PlTerm_term_t : public PlTerm
{
public:
  // TODO: [[deprecated("use PlTerm(Plterm::null)")]]
  explicit PlTerm_term_t(term_t t)
    : PlTerm(t) {}
};

class PlTerm_integer : public PlTerm
{
public:
  // See comment with PlTerm::integer() about the overloading.
  explicit PlTerm_integer(char               v) { Plx_put_int64(unwrap(), v); }
  explicit PlTerm_integer(int                v) { Plx_put_int64(unwrap(), v); }
  explicit PlTerm_integer(long               v) { Plx_put_int64(unwrap(), v); }
  explicit PlTerm_integer(long long          v) { Plx_put_int64(unwrap(), v); }
  explicit PlTerm_integer(short              v) { Plx_put_int64(unwrap(), v); }
  explicit PlTerm_integer(signed char        v) { Plx_put_int64(unwrap(), v); }
  explicit PlTerm_integer(unsigned char      v) { Plx_put_uint64(unwrap(), v); }
  explicit PlTerm_integer(unsigned int       v) { Plx_put_uint64(unwrap(), v); }
  explicit PlTerm_integer(unsigned long      v) { Plx_put_uint64(unwrap(), v); }
  explicit PlTerm_integer(unsigned long long v) { Plx_put_uint64(unwrap(), v); }
  explicit PlTerm_integer(unsigned short     v) { Plx_put_uint64(unwrap(), v); }
};

class PlTerm_float : public PlTerm
{
public:
  explicit PlTerm_float(double v) { Plx_put_float(unwrap(), v);  }
};

// TODO: deprecate PlTerm_pointer and replace by C++ interface to blobs
//       (see also PlAtom::blob_data(), PlTerm::as_pointer())
class PlTerm_pointer : public PlTerm
{
public:
  explicit PlTerm_pointer(void * ptr) { Plx_put_pointer(unwrap(), ptr); }
};

inline PlModule PlContext();


class PlPredicate : public WrappedC<predicate_t>
{
public:
  explicit PlPredicate(predicate_t p)
    : WrappedC<predicate_t>(p) { }
  explicit PlPredicate(PlFunctor f)
    : WrappedC<predicate_t>(Plx_pred(f.unwrap(), static_cast<module_t>(PlModule::null)))
  { }
  explicit PlPredicate(PlFunctor f, PlModule m)
    : WrappedC<predicate_t>(Plx_pred(f.unwrap(), m.unwrap()))
  { }
  explicit PlPredicate(const char *name, int arity, const char *module)
    : WrappedC<predicate_t>(Plx_predicate(name, arity,  module))
  { }
  explicit PlPredicate(const std::string& name, int arity, const std::string& module)
    : WrappedC<predicate_t>(Plx_predicate(name.c_str(), arity,  module.c_str()))
  { }
  void predicate_info(PlAtom *name, size_t *arity, PlModule *module)
  { atom_t n;
    module_t m;
    Plx_predicate_info(unwrap(), &n, arity, &m);
    *name = PlAtom(n);
    *module = PlModule(m);
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
      a0_(n ? Plx_new_term_refs(n) : PlTerm::null)
  { if ( size_ )
      PlEx<bool>(a0_ != (term_t)0);
  }
  explicit PlTermv(size_t n, PlTerm t0)
    : size_(n),
      a0_(t0.unwrap())
  { // Assume that t0 is valid - it can be if 0 if PREDICATE_NONDET is
    // called for PL_PRUNED
  }

  PlTermv(const PlTermv&) = default;
  PlTermv& operator =(const PlTermv&) = default;
  ~PlTermv() = default;

  term_t termv() const
  { // Note that a0_ can be PlTerm::null if size_ == 0
    return a0_;
  }

  size_t size() const
  { return size_;
  }

  // TODO: PlTermv copy_term_ref() const

					/* create from args */
  explicit PlTermv(PlAtom a);
  explicit PlTermv(PlTerm m0);
  explicit PlTermv(PlTerm m0, PlTerm m1);
  explicit PlTermv(PlTerm m0, PlTerm m1, PlTerm m2);
  explicit PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3);
  explicit PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3, PlTerm m4);

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
  PlTerm_string(const char *text) { Plx_put_string_chars(unwrap(), text); }
  PlTerm_string(const char *text, size_t len) { Plx_put_string_nchars(unwrap(), len, text); }
  PlTerm_string(const wchar_t *text) { PlEx<int>(Plx_unify_wchars(unwrap(), PL_STRING, static_cast<size_t>(-1), text)); }
  PlTerm_string(const wchar_t *text, size_t len) { PlEx<int>(Plx_unify_wchars(unwrap(), PL_STRING, len, text));}
  PlTerm_string(const std::string& text) { Plx_put_string_nchars(unwrap(), text.size(), text.data()); }
  PlTerm_string(const std::wstring& text) { PlEx<int>(Plx_unify_wchars(unwrap(), PL_STRING, text.size(), text.data())); }
};


class PlTerm_list_codes : public PlTerm
{
public:
  // TODO: PlEncoding + deprecate this interface
  PlTerm_list_codes(const char *text) { Plx_put_list_codes(unwrap(), text); }
  PlTerm_list_codes(const wchar_t *text) { PlEx<int>(Plx_unify_wchars(unwrap(), PL_CODE_LIST, static_cast<size_t>(-1), text)); }
};


class PlTerm_list_chars : public PlTerm
{
public:
  // TODO: PlEncoding + deprecate this interface
  PlTerm_list_chars(const char *text) { Plx_put_list_chars(unwrap(), text); }
  PlTerm_list_chars(const wchar_t *text) { PlEx<int>(Plx_unify_wchars(unwrap(), PL_CHAR_LIST, static_cast<size_t>(-1), text)); }
};


class PlRecord : public WrappedC<record_t>
{
public:
  explicit PlRecord(PlTerm t)
    : WrappedC<record_t>(Plx_record(t.unwrap()))
  { }

  explicit PlRecord(record_t r)
    : WrappedC<record_t>(r)
  { }

  PlRecord(const PlRecord& r)
    : WrappedC<record_t>(r)
  { }

  PlRecord(PlRecord&& r) noexcept
    : WrappedC<record_t>(r)
  { r.reset();
  }

  PlRecord& operator=(const PlRecord& r)
  { if ( this != &r )
      reset(r);
    return *this;
  }

  PlRecord& operator=(PlRecord&& r) noexcept
  { if ( this != &r )
    { reset(r);
      r.reset();
    }
    return *this;
  }

  PlTerm term() const
  { PlTerm_var t;
    Plx_recorded(unwrap(), t.unwrap());
    return t;
  }

  void erase()
  { if ( not_null() )
      Plx_erase(unwrap());
    reset();
  }

  PlRecord duplicate() const
  { return is_null() ? PlRecord(null) : PlRecord(Plx_duplicate_record(unwrap()));
  }

  ~PlRecord()
  { // TODO: erase(); -- but probably doesn't work with move constructor
  }
};


class PlRecordDeleter
{
public:
  void operator ()(PlRecord *r) const
  { r->erase();
    delete r;
  }
};


class PlRecordExternalCopy
{
private:
  std::string C_ = "";

public:
  // TODO: C++20 has better constexpr for strings
  // static constexpr std::string_view null = "";
  bool is_null()  const { return C_.empty(); }
  bool not_null() const { return !C_.empty(); }
  void set_null() { C_ = ""; }
  // TODO: unwrap() can create a temporary because std:: string is more
  //       complex than an int or pointer, so don't implement them:
  // std::string unwrap() const { return C_; }
  // std::string *unwrap_as_ptr() { return &C_; }

  PlRecordExternalCopy(PlTerm t)
    : C_(init(t))
  { }

  PlRecordExternalCopy(const std::string& external)
    : C_(external)
  { }

  PlRecordExternalCopy(const char*external, size_t len)
    : C_(std::string(external, len))
  { }

  PlRecordExternalCopy(const PlRecordExternalCopy& r) = default;
  PlRecordExternalCopy& operator =(const PlRecordExternalCopy&) = delete;
  ~PlRecordExternalCopy() = default;

  PlTerm term() const
  { PlTerm_var t;
    Plx_recorded_external(C_.data(), t.unwrap());
    return t;
  }

  static PlTerm
  term(const char* data)
  { PlTerm_var t;
    Plx_recorded_external(data, t.unwrap());
    return t;
  }

  static PlTerm
  term(const std::string& data)
  { return term(data.data());
  }

  const std::string& data() const { return C_; }

private:
  std::string init(PlTerm t)
  { size_t len;
    char *external =  Plx_record_external(t.unwrap(), &len);
    std::string result(external, len);
    Plx_erase_external(external);
    return result;
  }

  void verify() const
  { PlEx<bool>(!C_.empty());
  }
};


		 /*******************************
		 *	      EXCEPTIONS	*
		 *******************************/

// Note: PlException, because it implements std::exception::what(),
//       isn't just a simple wrapper; it has the full virutal methods
//       overhead and also contains a std::string for the message that
//       what() generates. If you want something lightweight, you
//       should create the PlException object only if you need to do a
//       "throw".

class PlException : public PlExceptionBase
{
public:
  explicit PlException(PlTerm t)
    : term_rec_(t) { }

  explicit PlException(PlAtom a)
    : term_rec_(PlTerm_atom(a)) { }

  PlException(const PlException& e)
    : term_rec_(e.term_rec_.duplicate()),
      what_str_(e.what_str_)
  { }
  PlException(PlException&& e)
    : term_rec_(e.term_rec_.duplicate()),
      what_str_(e.what_str_)
  { e.term_rec_.erase();
    // Don't need to do anything with e.what_str_
  }
  PlException& operator =(const PlException&) = delete; // TODO: implement
  PlException& operator =(PlException&&) = delete;      // TODO: implement

  virtual bool is_null()
  { return term_rec_.is_null() || term().is_null();
  }
  virtual bool not_null()
  { return term_rec_.not_null() && term().not_null();
  }

  virtual ~PlException()
  { erase();
  }

  virtual const char* what() const throw() override;

  virtual PlTerm term() const
  { return term_rec_.term();
  }

  virtual const std::string as_string(PlEncoding enc=ENC_OUTPUT) const;

  // plThrow() is for the try-catch in PREDICATE - returns the result
  // of Plx_raise_exception(), which is always `false`, as a foreign_t.
  virtual foreign_t plThrow() const
  { return static_cast<foreign_t>(Plx_raise_exception(term().unwrap()));
  }

  // The following method needs to be used with care (e.g., not when
  // there's a resource error), which is why it isn't currently used
  // to implement what() -- it uses a call to
  // '$messages':message_to_string/2.
  PlTerm string_term() const;

protected:
  explicit PlException(term_t ex)
    : term_rec_(PlTerm(ex)) { }

  void set_what_str();

  void erase();

  PlRecord term_rec_;
  std::string what_str_; // keeps copy of what() so that c_str() works

  // PlTerm string_term() const; // TODO: revive this
};


PlException PlGeneralError(PlTerm inside);

PlException PlTypeError(const std::string& expected, PlTerm culprit);

PlException PlDomainError(const std::string& expected, PlTerm culprit);

PlException PlDomainError(PlTerm expected, PlTerm culprit);

PlException PlInstantiationError(PlTerm culprit);

PlException PlUninstantiationError(PlTerm culprit);

PlException PlRepresentationError(const std::string& resource);

PlException PlExistenceError(const std::string& type, PlTerm culprit);

PlException PlPermissionError(const std::string& op, const std::string& type, PlTerm obj);

PlException PlResourceError(const std::string& resource);

PlException PlUnknownError(const std::string& description);

void PlWrap_fail(qid_t qid);

template<typename C_t> C_t
PlWrap(C_t rc, qid_t qid)
{ if ( rc == static_cast<C_t>(0) )
    PlWrap_fail(qid);
  return rc;
}

void PlEx_fail(qid_t qid = 0);

template<typename C_t> void
PlEx(C_t rc, qid_t qid)
{ if ( rc == static_cast<C_t>(0) )
    PlEx_fail(qid);
}


inline void
PlCheckFail(bool rc)
{ if ( !rc )
    throw PlFail();
}

		 /*******************************
		 *             LISTS		*
		 *******************************/

class PlTerm_tail : public PlTerm
{
public:
  explicit PlTerm_tail(PlTerm l);

					/* building */
  [[nodiscard]] bool append(PlTerm e);

  [[nodiscard]] bool close()
  { return unify_nil();
  }

					/* enumerating */
  [[nodiscard]] bool next(PlTerm& t);
};


		 /*******************************
		 *	     REGISTER		*
		 *******************************/


class PlControl : public WrappedC<control_t>
{
public:
  explicit PlControl(control_t c)
    : WrappedC<control_t>(c) { }

  [[nodiscard]] int foreign_control() const { return Plx_foreign_control(unwrap()); }

  [[nodiscard]] intptr_t foreign_context() const { return Plx_foreign_context(unwrap()); }

  [[nodiscard]] void *foreign_context_address() const { return Plx_foreign_context_address(unwrap()); }

  [[nodiscard]] PlPredicate foreign_context_predicate() const { return PlPredicate(Plx_foreign_context_predicate(unwrap())); }

  template <typename ContextType>
  [[nodiscard]]
  std::unique_ptr<ContextType>
  context_unique_ptr()
  { return std::unique_ptr<ContextType>(static_cast<ContextType *>(foreign_context_address()));
  }

};


class PlRegister
{
public:
  PlRegister(const char *module, const char *name, int arity,
	    foreign_t (f)(term_t t0, int a, control_t ctx))
  { PlEx<bool>(PL_register_foreign_in_module(module, name, arity, reinterpret_cast<pl_function_t>(f), PL_FA_VARARGS));
  }

  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0))
  { PlEx<bool>(PL_register_foreign_in_module(module, name, 1, reinterpret_cast<pl_function_t>(f), 0));
  }
  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1))
  { PlEx<bool>(PL_register_foreign_in_module(module, name, 2, reinterpret_cast<pl_function_t>(f), 0));
  }
  PlRegister(const char *module, const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1, PlTerm a2))
  { PlEx<bool>(PL_register_foreign_in_module(module, name, 3, reinterpret_cast<pl_function_t>(f), 0));
  }

  // For C-style calls - needed to support a test case
  PlRegister(const char *module, const char *name, foreign_t (*f)(term_t a0))
  { PlEx<bool>(PL_register_foreign_in_module(module, name, 1, reinterpret_cast<pl_function_t>(f), 0));
  }

  // for non-deterministic calls
  PlRegister(const char *module, const char *name, int arity,
	     foreign_t (f)(term_t t0, int a, control_t ctx), short flags)
  { PlEx<bool>(PL_register_foreign_in_module(module, name, arity, reinterpret_cast<pl_function_t>(f), flags));
  }

  PlRegister(const PlRegister&) = delete;
  PlRegister(PlRegister&&) = delete;
  PlRegister& operator =(const PlRegister&) = delete;
  PlRegister& operator =(PlRegister&&) = delete;
  ~PlRegister() = default;
};


		 /*******************************
		 *	 CALLING PROLOG		*
		 *******************************/

class PlFrame : public WrappedC<fid_t>
{
public:
  PlFrame()
    : WrappedC<fid_t>(Plx_open_foreign_frame())
  { }

  void discard()
  { if ( not_null() )
    { Plx_discard_foreign_frame(unwrap());
      set_null();
    }
  }

  void close()
  { if ( not_null() )
    { Plx_close_foreign_frame(unwrap());
      set_null();
    }
  }

  ~PlFrame() noexcept(false)
  { // See comment about exception in PlQuery::~PlQuery()
    close();
  }

  void rewind()
  { Plx_rewind_foreign_frame(unwrap());
  }
};

[[nodiscard]] bool PlRewindOnFail(std::function<bool()> f);


class PlQuery : public WrappedC<qid_t>
{
public:
  PlQuery(PlPredicate pred, const PlTermv& av, int flags = PL_Q_PASS_EXCEPTION)
    : WrappedC<qid_t>(Plx_open_query(static_cast<module_t>(0), flags, pred.unwrap(), av.termv())),
      flags_(flags)
  { }
  // TODO: PlQuery(const wstring& ...)
  // TODO: PlQuery({PlAtom,PlFunctor,PlPredicate} ...)
  PlQuery(const std::string& name, const PlTermv& av, int flags = PL_Q_PASS_EXCEPTION)
    : WrappedC<qid_t>(Plx_open_query(static_cast<module_t>(0),
                                     flags,
                                     PlPredicate(PlFunctor(name, av.size())).unwrap(),
                                     av.termv())),
      flags_(flags)
  { }
  // TODO; Should resolve module only once.
  PlQuery(const std::string& module, const std::string& name, const PlTermv& av, int flags = PL_Q_PASS_EXCEPTION)
    : WrappedC<qid_t>(Plx_open_query(PlModule(module).unwrap(),
                                     flags,
                                     PlPredicate(PlFunctor(name, av.size()),
                                                 PlModule(module)).unwrap(),
                                     av.termv())),
      flags_(flags)
  { }
  PlQuery(qid_t qid)
    : WrappedC<qid_t>(qid)
  { }

  // The return code from next_solution can be (if called with PL_Q_EXT_STATUS):
  //    TRUE
  //    FALSE
  //    PL_S_NOT_INNER
  //    PL_S_EXCEPTION
  //    PL_S_FALSE
  //    PL_S_TRUE
  //    PL_S_LAST
  // Because of this, you shouldn't use PlCheckFail(q.next_solution())
  [[nodiscard]] int next_solution();

  PlTerm exception() const
  { verify(); // Not meaningful if cut() or close_destroy() has been done
    return PlTerm(Plx_exception(exception_qid()));
  }

  qid_t exception_qid() const
  { return flags_&PL_Q_CATCH_EXCEPTION ? unwrap() : 0;
  }

  PlTerm yielded() const
  { return PlTerm(Plx_yielded(unwrap()));
  }

  void cut()
  { if ( not_null() )
      Plx_cut_query(unwrap());
    set_null();
  }

  void close_destroy()
  { if ( not_null() )
      Plx_close_query(unwrap());
    set_null();
  }

  ~PlQuery() noexcept(false)
  { // cut() can throw a C++ exception - throwing an exception from a
    // destructor is "potentially dangerous" but it's necessary to
    // ensure proper behaviour in Prolog.
    cut(); // *not* close_destroy() - which destroys data&bindings from query
  }

private:
  int flags_;

  void verify() const
  { // PL_open_query() can return 0 if there isn't enough space on the
    // environment stack - the error is in PL_exception(0).
    PlEx<bool>(unwrap() != static_cast<qid_t>(0));
    int ex_flags = flags_ & (PL_Q_NORMAL | PL_Q_CATCH_EXCEPTION | PL_Q_PASS_EXCEPTION);
    // Ensure that only one of the exception-handling flags is set:
    if ( ex_flags != 0 &&
         ex_flags != PL_Q_NORMAL &&
         ex_flags != PL_Q_CATCH_EXCEPTION &&
         ex_flags != PL_Q_PASS_EXCEPTION )
      throw PlDomainError("PlQuery_flags", PlTerm_integer(flags_));
  }
};

PlQuery PlCurrentQuery();


// TODO: PlQueryEngine() from Plx_query_engine9)

// TODO: PlAssert(PlTerm, PlModule, flags) from Plx_assert()


// See comment about possible return values from
// PlQuery::next_solution(), which is used by PlCall().
int PlCall(const std::string& predicate, const PlTermv& args, int flags = PL_Q_PASS_EXCEPTION);
int PlCall(const std::string& module, const std::string& predicate, const PlTermv& args, int flags = PL_Q_PASS_EXCEPTION);
int PlCall(const std::string& goal, int flags = PL_Q_PASS_EXCEPTION);
int PlCall(const std::wstring& goal, int flags = PL_Q_PASS_EXCEPTION);
int PlCall(PlTerm goal, int flags = PL_Q_PASS_EXCEPTION);

		 /*******************************
		 *	      ENGINE		*
		 *******************************/

class PlEngine
{
public:
  PlEngine(int argc, char **argv)
  { Plx_initialise(argc, argv);
  }
  PlEngine(int argc, wchar_t **argv)
  { Plx_winitialise(argc, argv);
  }

  PlEngine(char *av0)
  { av[0] = av0;
    av[1] = nullptr;

    Plx_initialise(1, av);
  }

  PlEngine(wchar_t *av0)
  { w_av[0] = av0;
    w_av[1] = nullptr;

    Plx_winitialise(1, w_av);
  }

  // TODO: figure out copy/move semantics and implement
  PlEngine(const PlEngine&) = delete;
  PlEngine(PlEngine&&) = delete;
  PlEngine& operator =(const PlEngine&) = delete;
  PlEngine& operator =(PlEngine&&) = delete;

  void cleanup(int status_and_flags = 0) {
    Plx_cleanup(status_and_flags);
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


		 /***********************************
		 *  PL_{get,acquire,release}_stream *
		 ***********************************/


class PlStream
{
public:
  explicit PlStream(PlTerm stream, int flags);

  explicit PlStream(IOSTREAM *s);

  PlStream(const PlStream&) = default;
  PlStream& operator =(const PlStream&) = default;

  operator IOSTREAM *()
  { return s_;
  }

  // The destructor has an implicit throw of PlExceptionFail if
  // PL_release_stream() detects that an IO error had happened.  This
  // violates the C++ standard -- if ~PlStream() is called during a
  // stack unwind due to another exception being thrown, behavior is
  // "undefined". However, this is very unlikely; and the alternative
  // - not throwing an exception - negates the purpose of PlStream.
  // See https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Rc-dtor-fail
  ~PlStream() noexcept; // "noexcept" means it can throw

  // Implicit throw of PlExceptionFail if release fails:
  void release();

  void check_stream() const; // verify that stream is set

  // Following are simple wrappers around the S*() functions, which
  // check for error and call release(), which should throw an
  // exception.

  int set_timeout(int tmo);
  int unit_size();
  // TODO putc(), getc(), ungetc() - they can be a bit tricky
  //   int putc(int c);
  //   int getc();
  //   int ungetc(int c);
  bool canrepresent(int c);
  int putcode(int c);
  int getcode();
  int peekcode();
  int putw(int w);
  int getw();
  size_t fread(void *data, size_t size, size_t elems); // TODO: does not check for error
  size_t fwrite(const void *data, size_t size, size_t elems); // TODO: does not check for error
  int feof();
  int fpasteof();
  int ferror();
  void clearerr();
  int seterr(int which, const char *message);
  int set_exception(term_t ex);
  int setenc(IOENC new_enc, IOENC *old_enc);
  int setlocale(struct PL_locale *new_loc, struct PL_locale **old_loc);
  int flush();
  int64_t size();
  [[deprecated("Use seek64()")]] int seek(int64_t pos, int whence);
  [[deprecated("Use tell64()")]] int64_t tell();
  int close();
  int gcclose(int flags);
  char *gets(char *buf, int n);
  ssize_t read_pending(char *buf, size_t limit, int flags);
  size_t pending(); // TODO: does not check for error
  int fputs(const char *q);
  int printf(const char *fm, ...) WPRINTF23;
  int printfX(const char *fm, ...);
  int vprintf(const char *fm, va_list args);
  int lock();
  int tryLock();
  int unlock();
  int fileno();
  // int	closehook(void (*hook)(IOSTREAM *s));
  void	setbuffer(char *buf, size_t size);

  int64_t tell64();
  int	seek64(int64_t pos, int whence);

  int	checkBOM();
  int	writeBOM();

  bool qlf_get_int64(int64_t *ip);
  bool qlf_get_int32(int32_t *ip);
  bool qlf_get_uint32(uint32_t *ip);
  bool qlf_get_double(double *fp);
  bool qlf_get_atom(atom_t *a);
  bool qlf_put_int64(int64_t i);
  bool qlf_put_int32(int32_t i);
  bool qlf_put_uint32(uint32_t i);
  bool qlf_put_double(double f);
  bool qlf_put_atom(atom_t a);

private:
  IOSTREAM* s_ = nullptr;
};


		 /*******************************
		 *     REGISTER PREDICATES	*
		 *******************************/

#ifndef PROLOG_MODULE
#define PROLOG_MODULE static_cast<const char*>(nullptr)
#endif

// This macro is used by both the PREDICATE macros and the blob callbacks

#define PREDICATE_CATCH(error_action) \
	    catch ( const std::bad_alloc& ) \
	  { (void)Plx_resource_error("memory"); \
            error_action; \
	  } catch ( const PlExceptionFailBase& ) \
          { error_action; \
	  } catch ( const PlException& ex ) \
	  { ex.plThrow(); \
            error_action; \
	  }

#define NAMED_PREDICATE(plname, name, arity) \
	static foreign_t \
	pl_ ## name ## __ ## arity(PlTermv PL_av); \
	static foreign_t \
	_pl_ ## name ## __ ## arity(term_t t0, int a, control_t c) \
	{ (void)a; (void)c; \
          foreign_t rc; \
	  try \
	  { \
	    rc = pl_ ## name ## __ ## arity(PlTermv(arity, PlTerm(t0))); \
	  } \
          PREDICATE_CATCH(rc = false) \
          return rc; \
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
          foreign_t rc; \
	  try \
	  { \
	    rc = pl_ ## name ## __0(); \
	  } \
          PREDICATE_CATCH(rc = false) \
          return rc; \
	} \
	static PlRegister _x ## name ## __0(PROLOG_MODULE, plname, 0, \
					    _pl_ ## name ## __0); \
	static foreign_t pl_ ## name ## __0(void)

#define NAMED_PREDICATE_NONDET(plname, name, arity) \
	static foreign_t \
	pl_ ## name ## __ ## arity(PlTermv PL_av, PlControl handle); \
	static foreign_t \
	_pl_ ## name ## __ ## arity(term_t t0, int a, control_t c) \
	{ (void)a; \
          foreign_t rc; \
	  try \
	  { \
	    /* t0.unwrap() is 0 if handle.foreign_control()==PL_PRUNED */ \
	    rc = pl_ ## name ## __ ## arity(PlTermv(arity, PlTerm(t0)), PlControl(c)); \
	  } \
          PREDICATE_CATCH(rc = false) \
          return rc; \
	} \
	static PlRegister _x ## name ## __ ## arity(PROLOG_MODULE, plname, arity, \
						    _pl_ ## name ## __ ## arity, \
						    PL_FA_NONDETERMINISTIC | PL_FA_VARARGS); \
	static foreign_t pl_ ## name ## __ ## arity(PlTermv PL_av, PlControl handle)

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

// TODO: delete this section
		 /*******************************
		 *     NONDET HELPERS		*
		 *******************************/

template <typename ContextType>
// TODO: [[deprecated("Use PlControl::context_unique_ptr")]]
class PlForeignContextPtr
{
public:
  [[deprecated("Use PlControl::context_unique_ptr")]]
  explicit PlForeignContextPtr(PlControl handle)
  { ptr_.reset(static_cast<ContextType *>(handle.foreign_context_address()));
  }

  PlForeignContextPtr(const PlForeignContextPtr<ContextType>&) = delete;
  PlForeignContextPtr(PlForeignContextPtr<ContextType>&&) = delete;
  PlForeignContextPtr<ContextType>& operator =(const PlForeignContextPtr<ContextType>&) = delete;
  PlForeignContextPtr<ContextType>& operator =(PlForeignContextPtr<ContextType>&&) = delete;

  [[deprecated("Use PlControl::context_unique_ptr")]] ContextType& operator*()  const { return *ptr_; }
  [[deprecated("Use PlControl::context_unique_ptr")]] ContextType* operator->() const { return ptr_.get(); }
  [[deprecated("Use PlControl::context_unique_ptr")]] ContextType* get()        const { return ptr_.get(); }
  [[deprecated("Use PlControl::context_unique_ptr")]] void set(ContextType* ptr = nullptr)   { reset(ptr); } // TODO: delete
  [[deprecated("Use PlControl::context_unique_ptr")]] void reset(ContextType* ptr = nullptr) { ptr_.reset(ptr); }
  [[deprecated("Use PlControl::context_unique_ptr")]] ContextType* keep()        { return release(); } // TODO: delete
  [[deprecated("Use PlControl::context_unique_ptr")]] ContextType* release()     { return ptr_.release(); }

private:
  std::unique_ptr<ContextType> ptr_;
};


		 /*******************************
		 *          BLOBS		*
		 *******************************/

// PlBlobV is used by PL_BLOB_DEFINITION to create a vector of
// C-callable functions that in turn call the methods inside PlBlob.
// Although PlBlobV is defined as a class, it is really a namespace,
// with a template variable C_t for convenience.
//
// TODO: This code assumes that the only difference between C and C++
//       functions is the name mangling. If there are additional
//       differences, this template class will need to be replaced by
//       some macros that define a set of `extern "C"` functions.

template<typename C_t>
class PlBlobV
{
public:
  [[nodiscard]]
  static C_t *
  cast(PlAtom aref)
  { size_t len;
    PL_blob_t *type;
    if ( aref.is_null() )
      return nullptr;
    auto ref = static_cast<C_t *>(aref.blob_data(&len, &type));
    // Can't throw PlException here because might be in a context
    // outside of a PREDICATE.
    if ( ref && type == ref->blob_t_ )
    { if ( len != sizeof *ref )
        PL_api_error("Invalid size %zd (should be %zd) for %s", len, sizeof *ref, typeid(C_t).name());
      return ref;
    }
    return nullptr;
  }

  [[nodiscard]]
  static C_t*
  cast_check(PlAtom aref)
  { auto ref = cast(aref);
    // Can't throw PlException here because might be in a context
    // outside of a PREDICATE.
    if ( !ref )
      PL_api_error("Failed cast to %s", typeid(C_t).name());
    return ref;
  }

  [[nodiscard]]
  static C_t*
  cast_ex(PlTerm t, const PL_blob_t& b)
  { auto ref = cast(t.as_atom());
    if ( !ref )
      throw PlTypeError(b.name, t);
    return ref;
  }

  static void acquire(atom_t a)
  { PlAtom a_(a);
    auto data = cast_check(a_);
    bool rc; // Uninitialized variable warning (some compilers)
    try
    { data->acquire(a_);
      rc = true;
    }
    PREDICATE_CATCH(rc = false)
    if ( !rc )
      PL_api_error("Failed acquire() for %s", typeid(C_t).name());
    // TODO: if ( ! rc ) Plx_clear_exception() ?
  }

  [[nodiscard]]
  static int release(atom_t a) noexcept
  { auto data = cast(PlAtom(a));
    if ( !data ) // Shouldn't happen, even if PL_free_blob() has been used.
      return true;
    try
    { if ( !data->pre_delete() )
        return false;
    }
    PREDICATE_CATCH(return false)
    delete data;
    return true;
  }

  [[nodiscard]]
  static int compare(atom_t a, atom_t b)
  { if (a == b) // Just in case Prolog didn't check this.
      return 0;
    // Prolog should never give us two atoms (blobs) with different
    // types - they should have been already compared by standard
    // order of types; but use cast_check() anyway (which will be
    // optimised away if NDEBUG).
    bool rc_try = false; // Uninitialized variable warning (some compilers)
    int rc = 0;          // Uninitialized variable warning (some compilers)
    try
    { const auto a_data = cast(PlAtom(a));
      const auto b_data = cast(PlAtom(b));
      rc = ( a_data && b_data ) ? a_data->compare_fields(b_data) : 0;
      if ( rc == 0 )
        rc = (a_data < b_data) ? -1 : (a_data > b_data) ? 1 : 0;
      rc_try = true;
    }
    PREDICATE_CATCH(rc_try = false; rc = 0;)
    if ( !rc_try )
      PL_api_error("Failed compare() for %s", typeid(C_t).name());
    return rc;
  }

  [[nodiscard]]
  static int write(IOSTREAM *s, atom_t a, int flags)
  { const auto data = cast(PlAtom(a));
    if ( !data )
      // TODO: demangle typeid::name()
      return Sfprintf(s, "<%s>(%p)", typeid(C_t).name(), data) >= 0;
    int rc = -1; // Uninitialized variable warning (some compilers)
    try
    { rc = data->write(s, flags);
    }
    PREDICATE_CATCH(rc = -1)
      // TODO: if ( rc < 0 ) Plx_clear_exception() ?
    return rc;
  }

  [[nodiscard]]
  static int save(atom_t a, IOSTREAM *fd)
  { const auto data = cast(PlAtom(a));
    if ( !data )
      return false;
    bool rc = false; // Uninitialized variable warning (some compilers)
    try
    { data->save(fd);
      rc = true;
    }
    PREDICATE_CATCH(rc = false)
      // TODO: if ( ! rc ) Plx_clear_exception() ?
    return rc;
  }

  [[nodiscard]]
  static atom_t load(IOSTREAM *fd)
  { C_t ref;
    atom_t atom;
    int rc_try = false; // Uninitialized variable warning (some compilers)
    try
    { atom = ref.load(fd).unwrap();
      rc_try = true;
    }
    PREDICATE_CATCH(rc_try = false);
    if ( !rc_try )
    { // TODO: Plx_clear_exception() ?
      return PlAtom::null;
    }
    return atom;
  }
};

#if __cplusplus >= 202002L
#define DESIG_INIT(name) .name =
#else
#define DESIG_INIT(name)
#endif

#define PL_BLOB_DEFINITION(blob_class, blob_name) \
{ DESIG_INIT(magic)   PL_BLOB_MAGIC,			\
  DESIG_INIT(flags)   PL_BLOB_NOCOPY,			\
  DESIG_INIT(name)    blob_name,			\
  DESIG_INIT(release) PlBlobV<blob_class>::release,	\
  DESIG_INIT(compare) PlBlobV<blob_class>::compare,	\
  DESIG_INIT(write)   PlBlobV<blob_class>::write,	\
  DESIG_INIT(acquire) PlBlobV<blob_class>::acquire,	\
  DESIG_INIT(save)    PlBlobV<blob_class>::save,	\
  DESIG_INIT(load)    PlBlobV<blob_class>::load		\
}

#define PL_BLOB_SIZE \
  virtual size_t blob_size_() const override { return sizeof *this; }

class PlBlob
{
public:
  explicit PlBlob(const PL_blob_t* _blob_t)
    : blob_t_(_blob_t), symbol_(PlAtom(PlAtom::null)) { }
  explicit PlBlob() = delete;
  explicit PlBlob(const PlBlob&) = delete;
  explicit PlBlob(PlBlob&&) = delete;
  PlBlob& operator =(const PlBlob&) = delete;
  virtual ~PlBlob() = default;

  virtual bool pre_delete() { return true; }

  virtual size_t blob_size_() const = 0; // See PL_BLOB_SIZE

  // acquire() is not virtual and subclass must not override it.
  void acquire(PlAtom _symbol)
  { symbol_ = _symbol;
    // Don't: symbol_.register_ref() because it's already got a
    // reference from when the blob was allocated, (by
    // PL_unify_blob(), PL_put_blob(), or PL_new_blob()); extra
    // reference count would prevent its GC.
  }

  PlTerm symbol_term() const;

  virtual int compare_fields(const PlBlob *_b) const
  { return 0; // compare() will do bitwise comparison
  }

  // write() is not virtual and subclass must not override it. write_fields() is virtual.
  bool write(IOSTREAM *s, int flags) const;

  bool virtual write_fields(IOSTREAM *s, int flags) const
  { return true;
  }

  virtual void save(IOSTREAM *fd) const;

  virtual PlAtom load(IOSTREAM *fd);

  void register_ref()
  { symbol_.register_ref();
  }

  void unregister_ref()
  { symbol_.unregister_ref();
  }

  const PL_blob_t* blob_t_;

  // Associated symbol (used for error terms), filled in by acquire()
  // and usually accessed by the symbol_term() method
  PlAtom symbol_;
};


// For backwards compatibility (before the unwrap() method was added):
#define C_ unwrap()


#ifndef _SWI_CPP2_CPP_SEPARATE

#ifndef _SWI_CPP2_CPP_inline
#define _SWI_CPP2_CPP_inline inline
#endif
#include "SWI-cpp2.cpp"

#endif

#endif /*_SWI_CPP2_H*/
