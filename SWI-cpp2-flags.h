/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, SWI-Prolog Solutions b.v.
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

#ifndef _SWI_CPP2_FLAGS_H
#define _SWI_CPP2_FLAGS_H

#include <map>
#include <vector>
#include <string>

#include "SWI-cpp2.h"

// TODO: add documentation to pl2cpp.doc

/* WARNING: experimetal code, subject to change

   Class \ctype{PlOptionsFlag} contains utilities for translating between a
   list of strings and an flag of bits. The flag is designated as
   \ctopy{FlagT}, and typically is \ctype{int} or \ctype{unsigned}.

   \begin{description}
     \constructor{PlOptionsFlag}{string domain, vector<string,FlagT> str2flag} -
       \arg{domain} is used for exceptions; \arg{str2flag} maps
       strings to flag values. here may be multiple strings that map
       to the same flag; the first one is preferred by
       PlOptionsFlag::as_string().
     \cfunction{FlagT}{PlOptionsFlag::lookup_list}{PlTerm options, bool throw_not_found} -
       process \arg{options}, a list of strings or atoms, into a flag by looking
       up each itme and or-ing the flag values. If a lookup fails
       and \arg{throw_not_found} is \const{true} (the default),
       \ctype{PlDomainError} is thrown.
     \cfunction{FlagT}{PlOptionsFlag::lookup}{PlTerm e, bool throw_not_found}-
       process a single \arg{option}, returning its flag value.
       If a lookup fails
       and \arg{throw_not_found} is \const{true} (the default),
       \ctype{PlDomainError} is thrown, otherwise 0 is returned.
     \cfunction{string}PlOptionsFlag::as_string}{FlagT flags} -
       Return a string containing a comma-separated list of
       the preferred strings corresponding to the bits in \arg{flags}.
       If a bit value is found, "???" is added to the list
       TODO: with C++-20, std::format("{:x}")
   \end{description}

 */

template<typename FlagT>
class PlOptionsFlag {
public:
  explicit PlOptionsFlag() = delete;
  explicit PlOptionsFlag(const std::string& domain,
                         const std::vector<std::pair<std::string, FlagT>>& str2flag)
    : domain_(domain),
      str2flag_v_(str2flag)
  { for ( auto const& sf : str2flag )
    { str2flag_[sf.first] = sf.second;
    }
  }

  FlagT lookup_list(PlTerm options, bool throw_not_found = true) const
  { PlTerm_tail tail(options);
    PlTerm_var e;
    FlagT flags = 0;
    while (tail.next(e))
    { flags |= lookup(e, throw_not_found);
    }
    PlCheckFail(tail.unify_nil());
    return flags;
  }

  FlagT lookup(PlTerm option, bool throw_not_found = true) const
  { option.must_be_atom_or_string();
    auto const f = str2flag_.find(option.as_string());
    if ( f != str2flag_.end() )
      return f->second;
    if ( throw_not_found )
      throw PlDomainError(domain_, option);
    return 0;
  }

  std::string as_string(FlagT flags) const
  { std::string result;
    for ( auto const& f : str2flag_v_ )
    { if ( (flags & f.second) == f.second )
      { result += "," + f.first;
        flags &= ~f.second;
      }
    }
    if ( flags != 0 )
      result += ",???"; // TODO (C++-20): std::format("{:x}", flags)
    return result.empty() ? result : result.substr(1);
  }

private:
  std::string domain_;
  std::map<std::string, FlagT> str2flag_;
  std::vector<std::pair<std::string, FlagT>> str2flag_v_;
};

#endif /*_SWI_CPP2_FLAGS_H*/
