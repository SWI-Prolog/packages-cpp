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

#ifndef _SWI_CPP2_ATOMMAP_H
#define _SWI_CPP2_ATOMMAP_H

#include <map>
#include <mutex>

#include "SWI-cpp2.h"


// The AtomMap class is a wrapper around a std::map, mapping
// alias names to blobs. The blobs are of typ PlAtom, so this is
// actually an atom->atom map.
// The entries are protected by a mutex, so the operations are thread-safe.
// The operations do appropriate calls to register and unregister the atoms/blobs.
//
// The operations are:
//   PlAtom find(PlAtom name) - look up, returning PlAtom::null if not found
//   void insert(PlAtom name, PlAtom symbol) - insert, throwing a
//                              PlPermissionError if it's already there
//   void erase(PlAtom name) - remove the entry (no error if it's already been removed)


template <typename ValueType, typename StoredValueType>
class AtomMap
{
public:
  explicit AtomMap() = delete;
  // See permission_error/3 for explanation of insert_op and insert_type.
  // The result error message is something like
  //   No permission to <insert_op> <insert_type> `<key>'
  explicit AtomMap(const std::string& insert_op, const std::string& insert_type)
    : insert_op_(insert_op), insert_type_(insert_type)
  { }
  AtomMap(const AtomMap&) = delete;
  AtomMap(const AtomMap&&) = delete;
  AtomMap& operator =(const AtomMap&) = delete;
  AtomMap& operator =(const AtomMap&&) = delete;
  ~AtomMap() = default;

  void
  insert(PlAtom key, ValueType value)
  { std::lock_guard<std::mutex> lock__(lock_);
    insert_inside_lock(key, value);
  }

  [[nodiscard]]
  ValueType
  find(PlAtom key)
  { std::lock_guard<std::mutex> lock__(lock_);
    return find_inside_lock(key);
  }

  void
  erase(PlAtom key)
  { std::lock_guard<std::mutex> lock__(lock_);
    erase_inside_lock(key);
  }

  size_t
  size()
  { std::lock_guard<std::mutex> lock__(lock_);
    return size_inside_lock();
  }

private:
  [[nodiscard]]
  ValueType
  find_inside_lock(PlAtom key)
  { const auto lookup = entries_.find(key.unwrap());
    ValueType value(ValueType::null);
    if ( lookup != entries_.end() )
      value.reset(ValueType(lookup->second));
    return value;
  }

  void
  insert_inside_lock(PlAtom key, ValueType value)
  { const auto lookup = find_inside_lock(key);
    if ( lookup.is_null() )
    { StoredValueType stored_value(StoredValueType::null);
      register_value(value, &stored_value);
      key.register_ref();
      entries_.insert(std::make_pair(key.unwrap(), stored_value));
    } else if ( lookup != value )
    { throw PlPermissionError(insert_op_, insert_type_, PlTerm_atom(key));
    }
  }

  void
  erase_inside_lock(PlAtom key)
  { auto lookup = entries_.find(key.unwrap());
    if ( lookup == entries_.end() )
      return;
    // TODO: As an alternative to removing the entry, leave it in place
    //       (with db==nullptr showing that it's been closed; or with
    //       the value as PlAtom::null), so that rocks_close/1 can
    //       distinguish an alias lookup that should throw a
    //       PlExistenceError because it's never been opened.
    key.unregister_ref();
    unregister_stored_value(&lookup->second);
    entries_.erase(lookup);
  }

  size_t
  size_inside_lock() const
  { return entries_.size();
  }

  // Implementation for map<PlAtom,PlAtom>

  static void
  register_value(const PlAtom &value, PlAtom *stored_value)
  { *stored_value = value;
    stored_value->register_ref();
  }

  static void
  unregister_stored_value(PlAtom *stored_value)
  { stored_value->unregister_ref();
  }

  // Implementation for map<PlAtom,PlRecord> (external: PlAtom,PlTerm>)

  static void
  register_value(const PlTerm &value, PlRecord *stored_value)
  { *stored_value = value.record();
  }

  static void
  unregister_stored_value(PlRecord *stored_value)
  { stored_value->erase();
  }

  // Data - mutex + map

  std::mutex lock_;
  // TODO: Define the necessary operators for PlAtom, so that it can be
  //       the key instead of atom_t.
  std::map<atom_t, StoredValueType> entries_;

  std::string insert_op_;
  std::string insert_type_;
};


#endif /*_SWI_CPP2_ATOMMAP_H*/
