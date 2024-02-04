/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2023, University of Amsterdam
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

// This file was derived from <SWI-Prolog.h> WUNUSED is changed to
// [[nodiscard]], and added to some functions that don't have WUNUSEd
// (possibly WUNUSED should be added for them in SWI-Prolog.h).

// Commented out lines are because:
// - no return value
// - return value 0 doesn't mean "fail"
// - private
// - has "..." or similar that requires a bit more work.
// In addition, some functions that return a boolean as an "int" have
// been changed to use a C++ "bool" (the template functions PlWrap()
// and PlEx() have been written to handle this situations).

// This file is included by SWI-cpp2.h -- it is kept separate because
// it is derived from SWI-Prolog.h

#ifndef _SWI_CPP2_PLX_H
#define _SWI_CPP2_PLX_H


/* Wrapper macros - each PL_*() function has a corresponding Plx_*() wrapper:
     PLX_EXCE is for functions whose return code only indicates an error
     PLX_WRAP is for functions whose return code could mean either an error or failure
     PLX_ASIS and PLX_VOID are for functions that are used as-is
*/

// TODO: remove PlWrapDebug() when global ordering bug is fixed
//       https://github.com/SWI-Prolog/swipl-devel/issues/1155

#ifdef O_DEBUG
void PlWrapDebug(const char*);
#else
#define PlWrapDebug(m)
#endif

#define PLX_EXCE(type, name, params, args) inline void Plx_ ## name params { PlWrapDebug("EXCE-" #name); PlEx<type>(PL_ ## name args); }

#define PLX_WRAP(type, name, params, args) [[nodiscard]] inline type Plx_ ## name params { PlWrapDebug("WRAP-" #name); return PlWrap<type>(PL_ ## name args); }

#define PLX_ASIS(type, name, params, args) [[nodiscard]] inline type Plx_ ## name params { PlWrapDebug("ASIS-"#name); return PL_ ## name args; }
#define PLX_VOID(type, name, params, args) inline void Plx_ ## name params { PlWrapDebug("VOID-" #name); PL_ ## name args; }

PLX_ASIS(int                    , foreign_control                 , (control_t c), (c))
PLX_ASIS(intptr_t               , foreign_context                 , (control_t c), (c))
PLX_ASIS(void *                 , foreign_context_address         , (control_t c), (c))
PLX_ASIS(predicate_t            , foreign_context_predicate       , (control_t c), (c))

PLX_VOID(void                   , register_extensions             , (const PL_extension *e), (e))
PLX_VOID(void                   , register_extensions_in_module   , (const char *module, const PL_extension *e), (module, e))

// (skipped):: int PL_register_foreign(const char *name, int arity, pl_function_t func, int flags, ...);
// (skipped):: int PL_register_foreign_in_module( const char *module , const char *name, int arity, pl_function_t func, int flags, ...);

// Deprecated: PL_load_extensions(const PL_extension *e);

// TODO: document PL_license()
PLX_VOID(void                    , license                         , (const char *license, const char *module), (license, module))

PLX_ASIS(module_t                , context                         , (), ())
PLX_ASIS(atom_t                  , module_name                     , (module_t module), (module))
PLX_WRAP(module_t                , new_module                      , (atom_t name), (name))
PLX_EXCE(int                     , strip_module                    , (term_t in, module_t *m, term_t out), (in, m, out))
PLX_WRAP(fid_t                   , open_foreign_frame              , (), ())

PLX_VOID(void                    , rewind_foreign_frame            , (fid_t cid), (cid))
PLX_VOID(void                    , close_foreign_frame             , (fid_t cid), (cid))
PLX_VOID(void                    , discard_foreign_frame           , (fid_t cid), (cid))

PLX_WRAP(predicate_t             , pred                            , (functor_t f, module_t m), (f, m))
PLX_WRAP(predicate_t             , predicate                       , (const char *name, int arity, const char* module), (name, arity, module))
PLX_EXCE(int                     , predicate_info                  , (predicate_t pred, atom_t *name, size_t *arity, module_t *module), (pred, name, arity, module))
PLX_WRAP(qid_t                   , open_query                      , (module_t m, int flags, predicate_t pred, term_t t0), (m, flags, pred, t0))
// TODO: PL_next_solution() needs special handling:
//       [[nodiscard]] int PL_next_solution(qid_t qid);
PLX_EXCE(int                     , close_query                     , (qid_t qid), (qid))
PLX_EXCE(int                     , cut_query                       , (qid_t qid), (qid))
PLX_ASIS(qid_t                   , current_query                   , (), ())
PLX_ASIS(PL_engine_t             , query_engine                    , (qid_t qid), (qid))
PLX_ASIS(bool                    , can_yield                       , (), ())
// [[nodiscard]]
PLX_WRAP(int                     , call                            , (term_t t, module_t m), (t, m))
// TODO: Needs special case - see PL_next_solution():
//       [[nodiscard]] int PL_call_predicate(module_t m, int debug, predicate_t pred, term_t t0);
PLX_ASIS(term_t                  , exception                       , (qid_t qid), (qid))
PLX_ASIS(int                     , raise_exception                 , (term_t exception), (exception))
// Deprecated: int PL_throw(term_t exception);
PLX_VOID(void                    , clear_exception                 , (), ())
// TODO: document PL_yielded()
PLX_ASIS(term_t                  , yielded                         , (qid_t qid), (qid))
PLX_EXCE(int                     , assert                          , (term_t term, module_t m, int flags), (term, m, flags))
PLX_WRAP(term_t                  , new_term_refs                   , (size_t n), (n))
PLX_WRAP(term_t                  , new_term_ref                    , (), ())
PLX_WRAP(term_t                  , copy_term_ref                   , (term_t from), (from))
PLX_VOID(void                    , reset_term_refs                 , (term_t r), (r))
/* [[deprecated]]  */
PLX_WRAP(atom_t                  , new_atom                        , (const char *s), (s))

PLX_WRAP(atom_t                  , new_atom_nchars                 , (size_t len, const char *s), (len, s))
PLX_WRAP(atom_t                  , new_atom_wchars                 , (size_t len, const pl_wchar_t *s), (len, s))
PLX_WRAP(atom_t                  , new_atom_mbchars                , (int rep, size_t len, const char *s), (rep, len, s))
// Deprecated: const char *PL_atom_chars(atom_t a);
PLX_WRAP(const char *            , atom_nchars                     , (atom_t a, size_t *len), (a, len))
PLX_EXCE(int                     , atom_mbchars                    , (atom_t a, size_t *len, char **s, unsigned int flags), (a, len, s, flags))
PLX_WRAP(const wchar_t *         , atom_wchars                     , (atom_t a, size_t *len), (a, len))
PLX_VOID(void                    , register_atom                   , (atom_t a), (a))
PLX_VOID(void                    , unregister_atom                 , (atom_t a), (a))
// (skipped):: void _PL_debug_register_atom(atom_t a, const char *file, int line, const char *func);
// (skipped):: void _PL_debug_unregister_atom(atom_t a, const char *file, int line, const char *func);

PLX_WRAP(functor_t               , new_functor                     , (atom_t f, size_t a), (f, a))
PLX_ASIS(atom_t                  , functor_name                    , (functor_t f), (f))
PLX_ASIS(size_t                  , functor_arity                   , (functor_t f), (f))
[[nodiscard]]
PLX_ASIS(bool                    , get_atom                        , (term_t t, atom_t *a), (t, a))
[[nodiscard]]
PLX_ASIS(bool                    , get_bool                        , (term_t t, int *value), (t, value))
[[nodiscard]]
PLX_ASIS(bool                    , get_atom_chars                  , (term_t t, char **a), (t, a))
[[nodiscard]]
// Deprecated: int PL_get_string(term_t t, char **s, size_t *len);
[[nodiscard]]
PLX_ASIS(bool                    , get_chars                       , (term_t t, char **s, unsigned int flags), (t, s, flags))
[[nodiscard]]
PLX_ASIS(bool                    , get_list_chars                  , (term_t l, char **s, unsigned int flags), (l, s, flags))
[[nodiscard]]
PLX_ASIS(bool                    , get_atom_nchars                 , (term_t t, size_t *len, char **a), (t, len, a))
[[nodiscard]]
PLX_ASIS(bool                    , get_list_nchars                 , (term_t l, size_t *len, char **s, unsigned int flags), (l, len, s, flags))
[[nodiscard]]
PLX_ASIS(bool                    , get_nchars                      , (term_t t, size_t *len, char **s, unsigned int flags), (t, len, s, flags))
[[nodiscard]]
PLX_ASIS(bool                    , get_integer                     , (term_t t, int *i), (t, i))
[[nodiscard]]
PLX_ASIS(bool                    , get_long                        , (term_t t, long *i), (t, i))
[[nodiscard]]
PLX_ASIS(bool                    , get_intptr                      , (term_t t, intptr_t *i), (t, i))
[[nodiscard]]
PLX_ASIS(bool                    , get_pointer                     , (term_t t, void **ptr), (t, ptr))
[[nodiscard]]
PLX_ASIS(bool                    , get_float                       , (term_t t, double *f), (t, f))
[[nodiscard]]
PLX_ASIS(bool                    , get_functor                     , (term_t t, functor_t *f), (t, f))
[[nodiscard]]
PLX_ASIS(bool                    , get_name_arity                  , (term_t t, atom_t *name, size_t *arity), (t, name, arity))
[[nodiscard]]
PLX_ASIS(bool                    , get_compound_name_arity         , (term_t t, atom_t *name, size_t *arity), (t, name, arity))
[[nodiscard]]
PLX_ASIS(bool                    , get_module                      , (term_t t, module_t *module), (t, module))
[[nodiscard]]
PLX_ASIS(bool                    , get_arg                         , (size_t index, term_t t, term_t a), (index, t, a))
[[nodiscard]]
PLX_ASIS(bool                    , get_dict_key                    , (atom_t key, term_t dict, term_t value), (key, dict, value))
[[nodiscard]]
PLX_ASIS(bool                    , get_list                        , (term_t l, term_t h, term_t t), (l, h, t))
[[nodiscard]]
PLX_ASIS(bool                    , get_head                        , (term_t l, term_t h), (l, h))
[[nodiscard]]
PLX_ASIS(bool                    , get_tail                        , (term_t l, term_t t), (l, t))
[[nodiscard]]
PLX_ASIS(bool                    , get_nil                         , (term_t l), (l))
[[nodiscard]]
[[deprecated]]
PLX_ASIS(int                     , get_term_value                  , (term_t t, term_value_t *v), (t, v))
PLX_ASIS(char *                  , quote                           , (int chr, const char *data), (chr, data))
// See the definition of PL_for_dict - return code determined by func:
PLX_ASIS(int                     , for_dict                        , (term_t dict,
                                                                      int (*func)(term_t key, term_t value, void *closure),
                                                                      void *closure, int flags),
                                                                     (dict, func, closure, flags))
PLX_ASIS(int                     , term_type                       , (term_t t), (t))
PLX_ASIS(bool                    , is_variable                     , (term_t t), (t))
PLX_ASIS(bool                    , is_ground                       , (term_t t), (t))
PLX_ASIS(bool                    , is_atom                         , (term_t t), (t))
PLX_ASIS(bool                    , is_integer                      , (term_t t), (t))
PLX_ASIS(bool                    , is_string                       , (term_t t), (t))
PLX_ASIS(bool                    , is_float                        , (term_t t), (t))
PLX_ASIS(bool                    , is_rational                     , (term_t t), (t))
PLX_ASIS(bool                    , is_compound                     , (term_t t), (t))
PLX_ASIS(bool                    , is_callable                     , (term_t t), (t))
PLX_ASIS(bool                    , is_functor                      , (term_t t, functor_t f), (t, f))
PLX_ASIS(bool                    , is_list                         , (term_t t), (t))
PLX_ASIS(bool                    , is_dict                         , (term_t t), (t))
PLX_ASIS(bool                    , is_pair                         , (term_t t), (t))
PLX_ASIS(bool                    , is_atomic                       , (term_t t), (t))
PLX_ASIS(bool                    , is_number                       , (term_t t), (t))
PLX_ASIS(bool                    , is_acyclic                      , (term_t t), (t))
PLX_EXCE(int                     , put_variable                    , (term_t t), (t))
PLX_EXCE(int                     , put_atom                        , (term_t t, atom_t a), (t, a))
PLX_EXCE(int                     , put_bool                        , (term_t t, int val), (t, val))
PLX_EXCE(int                     , put_atom_chars                  , (term_t t, const char *chars), (t, chars))
PLX_EXCE(int                     , put_string_chars                , (term_t t, const char *chars), (t, chars))
PLX_EXCE(int                     , put_chars                       , (term_t t, int flags, size_t len, const char *chars), (t, flags, len, chars))
PLX_EXCE(int                     , put_list_chars                  , (term_t t, const char *chars), (t, chars))
PLX_EXCE(int                     , put_list_codes                  , (term_t t, const char *chars), (t, chars))
PLX_EXCE(int                     , put_atom_nchars                 , (term_t t, size_t l, const char *chars), (t, l, chars))
PLX_EXCE(int                     , put_string_nchars               , (term_t t, size_t len, const char *chars), (t, len, chars))
PLX_EXCE(int                     , put_list_nchars                 , (term_t t, size_t l, const char *chars), (t, l, chars))
PLX_EXCE(int                     , put_list_ncodes                 , (term_t t, size_t l, const char *chars), (t, l, chars))
PLX_EXCE(int                     , put_integer                     , (term_t t, long i), (t, i))
PLX_EXCE(int                     , put_pointer                     , (term_t t, void *ptr), (t, ptr))
PLX_EXCE(int                     , put_float                       , (term_t t, double f), (t, f))
PLX_EXCE(int                     , put_functor                     , (term_t t, functor_t functor), (t, functor))
PLX_EXCE(int                     , put_list                        , (term_t l), (l))
PLX_EXCE(int                     , put_nil                         , (term_t l), (l))
PLX_EXCE(int                     , put_term                        , (term_t t1, term_t t2), (t1, t2))
PLX_EXCE(int                     , put_dict                        , (term_t t, atom_t tag, size_t len, const atom_t *keys, term_t values), (t, tag, len, keys, values))
// TODO:
//    PL_EXPORT(atom_t)	_PL_cons_small_int(int64_t v); // 0 return code means not a small int
//    PL_EXPORT(void)		_PL_unregister_keys(size_t len, atom_t *keys);
// (skipped):: int PL_cons_functor(term_t h, functor_t f, ...) WUNUSED;
PLX_EXCE(int                     , cons_functor_v                  , (term_t h, functor_t fd, term_t a0), (h, fd, a0))
PLX_EXCE(int                     , cons_list                       , (term_t l, term_t h, term_t t), (l, h, t))

// [[nodiscard]]
PLX_WRAP(bool                    , unify                           , (term_t t1, term_t t2), (t1, t2))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_atom                      , (term_t t, atom_t a), (t, a))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_atom_chars                , (term_t t, const char *chars), (t, chars))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_list_chars                , (term_t t, const char *chars), (t, chars))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_list_codes                , (term_t t, const char *chars), (t, chars))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_string_chars              , (term_t t, const char *chars), (t, chars))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_atom_nchars               , (term_t t, size_t l, const char *s), (t, l, s))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_list_ncodes               , (term_t t, size_t l, const char *s), (t, l, s))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_list_nchars               , (term_t t, size_t l, const char *s), (t, l, s))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_string_nchars             , (term_t t, size_t len, const char *chars), (t, len, chars))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_bool                      , (term_t t, int n), (t, n))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_integer                   , (term_t t, intptr_t n), (t, n))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_float                     , (term_t t, double f), (t, f))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_pointer                   , (term_t t, void *ptr), (t, ptr))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_functor                   , (term_t t, functor_t f), (t, f))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_compound                  , (term_t t, functor_t f), (t, f))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_list                      , (term_t l, term_t h, term_t t), (l, h, t))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_nil                       , (term_t l), (l))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_arg                       , (size_t index, term_t t, term_t a), (index, t, a))
// (skipped):: // [[nodiscard]] bool PL_unify_term(term_t t, ...)
// [[nodiscard]]
PLX_WRAP(bool                    , unify_chars                     , (term_t t, int flags, size_t len, const char *s), (t, flags, len, s))

// [[nodiscard]]
PLX_ASIS(bool                    , skip_list                       , (term_t list, term_t tail, size_t *len), (list, tail, len))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_wchars                    , (term_t t, int type, size_t len, const pl_wchar_t *s), (t, type, len, s))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_wchars_diff               , (term_t t, term_t tail, int type, size_t len, const pl_wchar_t *s), (t, tail, type, len, s))

// [[nodiscard]]
PLX_ASIS(bool                    , get_wchars                      , (term_t l, size_t *length, pl_wchar_t **s, unsigned flags), (l, length, s, flags))
// TODO: document PL_utf8_strlen
// [[nodiscard]]
PLX_ASIS(size_t                  , utf8_strlen                     , (const char *s, size_t len), (s, len))
// [[nodiscard]]
PLX_ASIS(bool                    , get_int64                       , (term_t t, int64_t *i), (t, i))
// [[nodiscard]]
PLX_ASIS(bool                    , get_uint64                      , (term_t t, uint64_t *i), (t, i))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_int64                     , (term_t t, int64_t value), (t, value))
// [[nodiscard]]
PLX_WRAP(bool                    , unify_uint64                    , (term_t t, uint64_t value), (t, value))
// [[nodiscard]]
PLX_EXCE(int                     , put_int64                       , (term_t t, int64_t i), (t, i))
// [[nodiscard]]
PLX_EXCE(int                     , put_uint64                      , (term_t t, uint64_t i), (t, i))
PLX_ASIS(bool                    , is_attvar                       , (term_t t), (t))
PLX_WRAP(int                     , get_attr                        , (term_t v, term_t a), (v, a))
PLX_EXCE(int                     , get_atom_ex                     , (term_t t, atom_t *a), (t, a))
PLX_EXCE(int                     , get_integer_ex                  , (term_t t, int *i), (t, i))
PLX_EXCE(int                     , get_long_ex                     , (term_t t, long *i), (t, i))
PLX_EXCE(int                     , get_int64_ex                    , (term_t t, int64_t *i), (t, i))
PLX_EXCE(int                     , get_uint64_ex                   , (term_t t, uint64_t *i), (t, i))
PLX_EXCE(int                     , get_intptr_ex                   , (term_t t, intptr_t *i), (t, i))
PLX_EXCE(int                     , get_size_ex                     , (term_t t, size_t *i), (t, i))
PLX_EXCE(int                     , get_bool_ex                     , (term_t t, int *i), (t, i))
PLX_EXCE(int                     , get_float_ex                    , (term_t t, double *f), (t, f))
PLX_EXCE(int                     , get_char_ex                     , (term_t t, int *p, int eof), (t, p, eof))
PLX_EXCE(int                     , unify_bool_ex                   , (term_t t, int val), (t, val))
PLX_EXCE(int                     , get_pointer_ex                  , (term_t t, void **addrp), (t, addrp))
PLX_WRAP(int                     , unify_list_ex                   , (term_t l, term_t h, term_t t), (l, h, t))
PLX_EXCE(int                     , unify_nil_ex                    , (term_t l), (l))
PLX_WRAP(int                     , get_list_ex                     , (term_t l, term_t h, term_t t), (l, h, t))
PLX_EXCE(int                     , get_nil_ex                      , (term_t l), (l))

PLX_ASIS(int                     , instantiation_error             , (term_t culprit), (culprit))
PLX_ASIS(int                     , uninstantiation_error           , (term_t culprit), (culprit))
PLX_ASIS(int                     , representation_error            , (const char *resource), (resource))
PLX_ASIS(int                     , type_error                      , (const char *expected, term_t culprit), (expected, culprit))
PLX_ASIS(int                     , domain_error                    , (const char *expected, term_t culprit), (expected, culprit))
PLX_ASIS(int                     , existence_error                 , (const char *type, term_t culprit), (type, culprit))
PLX_ASIS(int                     , permission_error                , (const char *operation, const char *type, term_t culprit), (operation, type, culprit))
PLX_ASIS(int                     , resource_error                  , (const char *resource), (resource))
PLX_ASIS(int                     , syntax_error                    , (const char *msg, IOSTREAM *in), (msg, in))

PLX_ASIS(bool                    , is_blob                         , (term_t t, PL_blob_t **type), (t, type))
PLX_WRAP(bool                    , unify_blob                      , (term_t t, void *blob, size_t len, PL_blob_t *type), (t, blob, len, type))
PLX_WRAP(atom_t                  , new_blob                        , (void *blob, size_t len, PL_blob_t *type), (blob, len, type))
PLX_EXCE(int                     , put_blob                        , (term_t t, void *blob, size_t len, PL_blob_t *type), (t, blob, len, type))
PLX_WRAP(int                     , get_blob                        , (term_t t, void **blob, size_t *len, PL_blob_t **type), (t, blob, len, type))
PLX_ASIS(void*                   , blob_data                       , (atom_t a, size_t *len, struct PL_blob_t **type), (a, len, type))
PLX_ASIS(int                     , free_blob                       , (atom_t blob), (blob))
// Should not call PL_register_blob_type, so it's not defined:
// PLX_VOID(void                 , register_blob_type              , (PL_blob_t *type), (type))
PLX_ASIS(PL_blob_t*              , find_blob_type                  , (const char* name), (name))
PLX_ASIS(bool                    , unregister_blob_type            , (PL_blob_t *type), (type))

#ifdef __GNU_MP__
[[nodiscard]]
PLX_WRAP(int                     , get_mpz                         , (term_t t, mpz_t mpz), (t, mpz))
[[nodiscard]]
PLX_WRAP(bool                    , get_mpq                         , (term_t t, mpq_t mpq), (t, mpq))
[[nodiscard]]
PLX_WRAP(bool                    , unify_mpz                       , (term_t t, mpz_t mpz), (t, mpz))
[[nodiscard]]
PLX_WRAP(bool                    , unify_mpq                       , (term_t t, mpq_t mpq), (t, mpq))
#endif

PLX_ASIS(bool                    , get_file_name                   , (term_t n, char **name, int flags), (n, name, flags))
PLX_ASIS(bool                    , get_file_nameW                  , (term_t n, wchar_t **name, int flags), (n, name, flags))
// TODO: document PL_changed_cwd()
PLX_VOID(void                    , changed_cwd                     , (), ())
// TODO: document PL_cwd()
PLX_ASIS(char *                  , cwd                             , (char *buf, size_t buflen), (buf, buflen))

PLX_EXCE(int                     , cvt_i_bool                      , (term_t p, int *c), (p, c))
PLX_EXCE(int                     , cvt_i_char                      , (term_t p, char *c), (p, c))
PLX_EXCE(int                     , cvt_i_schar                     , (term_t p, signed char *c), (p, c))
PLX_EXCE(int                     , cvt_i_uchar                     , (term_t p, unsigned char *c), (p, c))
PLX_EXCE(int                     , cvt_i_short                     , (term_t p, short *s), (p, s))
PLX_EXCE(int                     , cvt_i_ushort                    , (term_t p, unsigned short *s), (p, s))
PLX_EXCE(int                     , cvt_i_int                       , (term_t p, int *c), (p, c))
PLX_EXCE(int                     , cvt_i_uint                      , (term_t p, unsigned int *c), (p, c))
PLX_EXCE(int                     , cvt_i_long                      , (term_t p, long *c), (p, c))
PLX_EXCE(int                     , cvt_i_ulong                     , (term_t p, unsigned long *c), (p, c))
PLX_EXCE(int                     , cvt_i_llong                     , (term_t p, long long *c), (p, c))
PLX_EXCE(int                     , cvt_i_ullong                    , (term_t p, unsigned long long *c), (p, c))
PLX_EXCE(int                     , cvt_i_int32                     , (term_t p, int32_t *c), (p, c))
PLX_EXCE(int                     , cvt_i_uint32                    , (term_t p, uint32_t *c), (p, c))
PLX_EXCE(int                     , cvt_i_int64                     , (term_t p, int64_t *c), (p, c))
PLX_EXCE(int                     , cvt_i_uint64                    , (term_t p, uint64_t *c), (p, c))
PLX_EXCE(int                     , cvt_i_size_t                    , (term_t p, size_t *c), (p, c))
PLX_EXCE(int                     , cvt_i_float                     , (term_t p, double *c), (p, c))
PLX_EXCE(int                     , cvt_i_single                    , (term_t p, float *c), (p, c))
PLX_EXCE(int                     , cvt_i_string                    , (term_t p, char **c), (p, c))
PLX_EXCE(int                     , cvt_i_codes                     , (term_t p, char **c), (p, c))
PLX_EXCE(int                     , cvt_i_atom                      , (term_t p, atom_t *c), (p, c))
PLX_EXCE(int                     , cvt_i_address                   , (term_t p, void *c), (p, c))
PLX_EXCE(int                     , cvt_o_int64                     , (int64_t c, term_t p), (c, p))
PLX_EXCE(int                     , cvt_o_float                     , (double c, term_t p), (c, p))
PLX_EXCE(int                     , cvt_o_single                    , (float c, term_t p), (c, p))
PLX_EXCE(int                     , cvt_o_string                    , (const char *c, term_t p), (c, p))
PLX_EXCE(int                     , cvt_o_codes                     , (const char *c, term_t p), (c, p))
PLX_EXCE(int                     , cvt_o_atom                      , (atom_t c, term_t p), (c, p))
PLX_EXCE(int                     , cvt_o_address                   , (void *address, term_t p), (address, p))

PLX_WRAP(term_t                  , new_nil_ref                     , (), ())
PLX_ASIS(int                     , cvt_encoding                    , (), ())
PLX_ASIS(int                     , cvt_set_encoding                , (int enc), (enc))
// (skipped):: void SP_set_state(int state);
// (skipped):: int SP_get_state();
PLX_ASIS(int                     , compare                         , (term_t t1, term_t t2), (t1, t2))
PLX_ASIS(int                     , same_compound                   , (term_t t1, term_t t2), (t1, t2))
// (skipped):: int PL_warning(const char *fmt   , ...) WPRINTF12;
// (skipped):: int PL_warningX(const char *fmt  , ...);
// (skipped):: void PL_fatal_error(const char *fmt  , ...) WPRINTF12;
PLX_WRAP(record_t                , record                          , (term_t term), (term))
PLX_EXCE(int                     , recorded                        , (record_t record, term_t term), (record, term))
PLX_VOID(void                    , erase                           , (record_t record), (record))
PLX_WRAP(record_t                , duplicate_record                , (record_t r), (r))
PLX_WRAP(char *                  , record_external                 , (term_t t, size_t *size), (t, size))
PLX_EXCE(int                     , recorded_external               , (const char *rec, term_t term), (rec, term))
PLX_EXCE(int                     , erase_external                  , (char *rec), (rec))
// (skipped):: int PL_set_prolog_flag(const char *name, int type, ...);
// (skipped):: PL_atomic_t _PL_get_atomic(term_t t);
// (skipped):: void _PL_put_atomic(term_t t, PL_atomic_t a);
// (skipped):: int _PL_unify_atomic(term_t t, PL_atomic_t a);
// (skipped):: int _PL_get_arg_sz(size_t index, term_t t, term_t a);
// (skipped):: int _PL_get_arg(int index, term_t t, term_t a);
PLX_VOID(void                    , mark_string_buffers             , (buf_mark_t *mark), (mark))
PLX_VOID(void                    , release_string_buffers_from_mark, (buf_mark_t mark), (mark))
PLX_WRAP(bool                    , unify_stream                    , (term_t t, IOSTREAM *s), (t, s))
// TODO: document PL_get_stream_handle
PLX_EXCE(int                     , get_stream_handle               , (term_t t, IOSTREAM **s), (t, s))
PLX_EXCE(int                     , get_stream                      , (term_t t, IOSTREAM **s, int flags), (t, s, flags))
PLX_EXCE(int                     , get_stream_from_blob            , (atom_t a, IOSTREAM**s, int flags), (a, s, flags))
PLX_WRAP(IOSTREAM*               , acquire_stream                  , (IOSTREAM *s), (s))
PLX_EXCE(int                     , release_stream                  , (IOSTREAM *s), (s))
// TODO: document PL_release_stream_noerror()
PLX_WRAP(int                     , release_stream_noerror          , (IOSTREAM *s), (s))
// TODO: document PL_open_resource()
PLX_WRAP(IOSTREAM *              , open_resource                   , (module_t m, const char *name, const char *rc_class, const char *mode), (m, name, rc_class, mode))

// (skipped):: IOSTREAM **_PL_streams(void);	/* base of streams */
PLX_ASIS(int                     , write_term                      , (IOSTREAM *s, term_t term, int precedence, int flags), (s, term, precedence, flags))
PLX_ASIS(bool                    , ttymode                         , (IOSTREAM *s), (s))

// TODO: PL_put_term_from_chars depends on CVT_EXCEPTION - ? make version that checks this and throws an exception?
PLX_ASIS(int                     , put_term_from_chars             , (term_t t, int flags, size_t len, const char *s), (t, flags, len, s))

// PL_chars_to_term(), PL_wchars_to_term() put error into term for syntax errors
[[nodiscard]]
PLX_ASIS(int                     , chars_to_term                   , (const char *chars, term_t term), (chars, term))
[[nodiscard]]
PLX_ASIS(int                     , wchars_to_term                  , (const pl_wchar_t *chars, term_t term), (chars, term))

// In the following, some of the functions can return `false` without
// a Prolog error; in these cases, a PlUnknownError is thrown.
// If you wish finer control, use the PL_*() version of the call.
PLX_EXCE(int                     , initialise                      , (int argc, char **argv), (argc, argv))
PLX_EXCE(int                     , winitialise                     , (int argc, wchar_t **argv), (argc, argv))
PLX_ASIS(bool                    , is_initialised                  , (int *argc, char ***argv), (argc, argv))
PLX_EXCE(int                     , set_resource_db_mem             , (const unsigned char *data, size_t size), (data, size))
PLX_ASIS(bool                    , toplevel                        , (), ())
PLX_EXCE(int                     , cleanup                         , (int status), (status))
PLX_VOID(void                    , cleanup_fork                    , (), ())
PLX_ASIS(int                     , halt                            , (int status), (status))

PLX_ASIS(void *                  , dlopen                          , (const char *file, int flags), (file, flags))
PLX_ASIS(const char *            , dlerror                         , (), ())
PLX_ASIS(void *                  , dlsym                           , (void *handle, char *symbol), (handle, symbol))
PLX_ASIS(int                     , dlclose                         , (void *handle), (handle))

// TODO: document PL_dispatch(), PL_add_to_protocol, etc.
PLX_ASIS(int                     , dispatch                        , (int fd, int wait), (fd, wait))
PLX_VOID(void                    , add_to_protocol                 , (const char *buf, size_t count), (buf, count))
PLX_ASIS(char *                  , prompt_string                   , (int fd), (fd))
PLX_VOID(void                    , write_prompt                    , (int dowrite), (dowrite))
PLX_VOID(void                    , prompt_next                     , (int fd), (fd))
PLX_ASIS(char *                  , atom_generator                  , (const char *prefix, int state), (prefix, state))
PLX_ASIS(pl_wchar_t*             , atom_generator_w                , (const pl_wchar_t *pref, pl_wchar_t *buffer, size_t buflen, int state), (pref, buffer, buflen, state))

PLX_ASIS(void *                  , malloc                          , (size_t size), (size))
PLX_ASIS(void *                  , malloc_atomic                   , (size_t size), (size))
PLX_ASIS(void *                  , malloc_uncollectable            , (size_t size), (size))
PLX_ASIS(void *                  , malloc_atomic_uncollectable     , (size_t size), (size))
PLX_ASIS(void *                  , realloc                         , (void *mem, size_t size), (mem, size))
PLX_ASIS(void *                  , malloc_unmanaged                , (size_t size), (size))
PLX_ASIS(void *                  , malloc_atomic_unmanaged         , (size_t size), (size))
PLX_VOID(void                    , free                            , (void *mem), (mem))
PLX_ASIS(int                     , linger                          , (void *mem), (mem))

PLX_ASIS(PL_dispatch_hook_t      , dispatch_hook                   , (PL_dispatch_hook_t h), (h))
PLX_VOID(void                    , abort_hook                      , (PL_abort_hook_t h), (h))
PLX_VOID(void                    , initialise_hook                 , (PL_initialise_hook_t h), (h))
PLX_ASIS(int                     , abort_unhook                    , (PL_abort_hook_t h), (h))
PLX_ASIS(PL_agc_hook_t           , agc_hook                        , (PL_agc_hook_t h), (h))

// TODO: int PL_scan_options(term_t options, int flags, const char *opttype, PL_option_t specs[], ...);
// Deprecated: void (*PL_signal(int sig, void (*func)(int)))(int);
PLX_ASIS(int                     , sigaction                       , (int sig, pl_sigaction_t *act, pl_sigaction_t *old), (sig, act, old))
PLX_VOID(void                    , interrupt                       , (int sig), (sig))
PLX_ASIS(int                     , raise                           , (int sig), (sig))
PLX_ASIS(int                     , handle_signals                  , (), ())
PLX_ASIS(int                     , get_signum_ex                   , (term_t sig, int *n), (sig, n))
// (skipped):: int PL_action(int, ...);
PLX_VOID(void                    , on_halt                         , (int (*f)(int, void *), void *closure), (f, closure))
PLX_VOID(void                    , exit_hook                       , (int (*f)(int, void *), void *closure), (f, closure))
PLX_VOID(void                    , backtrace                       , (int depth, int flags), (depth, flags))
PLX_ASIS(char *                  , backtrace_string                , (int depth, int flags), (depth, flags))
PLX_ASIS(int                     , check_data                      , (term_t data), (data))
PLX_ASIS(int                     , check_stacks                    , (), ())
PLX_ASIS(int                     , current_prolog_flag             , (atom_t name, int type, void *ptr), (name, type, ptr))
PLX_ASIS(unsigned int            , version_info                    , (int which), (which))
PLX_ASIS(intptr_t                , query                           , (int i), (i))
PLX_ASIS(int                     , thread_self                     , (), ())
PLX_WRAP(int                     , unify_thread_id                 , (term_t t, int i), (t, i))
PLX_WRAP(int                     , get_thread_id_ex                , (term_t t, int *idp), (t, idp))
PLX_ASIS(int                     , get_thread_alias                , (int tid, atom_t *alias), (tid, alias))
// TODO: document thread_attach_engine; make PLX_WRAP version (tid < 0)
PLX_ASIS(int                     , thread_attach_engine            , (PL_thread_attr_t *attr), (attr))
PLX_EXCE(int                     , thread_destroy_engine           , (), ())
PLX_ASIS(int                     , thread_at_exit                  , (void (*function)(void *), void *closure, int global), (function, closure, global))
PLX_ASIS(int                     , thread_raise                    , (int tid, int sig), (tid, sig))

// JW: disabled.  Claims these functions are not present in Windows, blocking the build.
#if 0 && defined(_WINDOWS_) || defined(_WINDOWS_H) /* <windows.h> is included */
PLX_ASIS(int                     , w32thread_raise                 , (DWORD dwTid, int sig), (dwTid, sig))
PLX_ASIS(int                     , wait_for_console_input          , (void *handle), (handle))
PLX_ASIS(int                     , w32_wrap_ansi_console           , (), ())
PLX_ASIS(const char*             , w32_running_under_wine          , (), ())
PLX_EXCE(LRESULT                 , win_message_proc                , (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam), (hwnd, message, wParam, lParam))
#endif

PLX_ASIS(PL_engine_t             , create_engine                   , (PL_thread_attr_t *attributes), (attributes))
PLX_ASIS(int                     , set_engine                      , (PL_engine_t engine, PL_engine_t *old), (engine, old))
PLX_ASIS(int                     , destroy_engine                  , (PL_engine_t engine), (engine))
PLX_ASIS(hash_table_t            , new_hash_table                  , (int size, void (*free_symbol)(table_key_t n, table_value_t v)), (size, free_symbol))
PLX_ASIS(int                     , register_profile_type           , (PL_prof_type_t *type), (type))
PLX_ASIS(void*                   , prof_call                       , (void *handle, PL_prof_type_t *type), (handle, type))
PLX_VOID(void                    , prof_exit                       , (void *node), (node))
// (skipped):: PL_EXPORT_DATA(int) plugin_is_GPL_compatible;
// (skipped):: int emacs_module_init(void*);
PLX_ASIS(int                     , prolog_debug                    , (const char *topic), (topic))
PLX_ASIS(int                     , prolog_nodebug                  , (const char *topic), (topic))

// (skipped):: int _PL_get_xpce_reference(term_t t, xpceref_t *ref);
// (skipped):: int _PL_unify_xpce_reference(term_t t, xpceref_t *ref);
// (skipped):: int _PL_put_xpce_reference_i(term_t t, uintptr_t r);
// (skipped):: int _PL_put_xpce_reference_a(term_t t, atom_t name);

PLX_ASIS(int                     , get_context                     , (struct pl_context_t *c, int thead_id), (c, thead_id))
PLX_ASIS(int                     , step_context                    , (struct pl_context_t *c), (c))
PLX_ASIS(int                     , describe_context                , (struct pl_context_t *c, char *buf, size_t len), (c, buf, len))


#undef PLX_EXCE
#undef PLX_WRAP
#undef PLX_ASIS
#undef PLX_VOID

#endif /* _SWI_CPP2_PLX_H */
