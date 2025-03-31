#include "SWI-cpp2.h"
#include <iostream>

int
main(int argc, char **argv) // TODO: char *argv[]
{ PlEngine e(argv[0]);
  // PlEngine can throw PlEngineInitialisationFailed or
  //                    PlOpenForeignFrameFailed

  try
  { PlTermv av(1);
    PlTerm_tail l(av[0]);

    try
    { for(int i=0; i<argc; i++)
        PlCheckFail(l.append(PlTerm_string(argv[i]))); // Can throw PlFail
      PlCheckFail(l.close());
    } catch ( const PlFail &ex )
    { std::cerr << "ERROR: append failed." << std::endl;
      return 253;
    }

    { PlQuery q1("writeln", av);
      if ( !PlWrap<int>(q1.next_solution()) )
      { std::cerr << "*** q1 failed" << std::endl;
        return 1;
      }
      if ( PlWrap<int>(q1.next_solution()) ) // There should be just 1 solution
      { std::cerr << "*** q1 2nd solution should have failed" << std::endl;
        return 2;
      }
    }
    { PlQuery q3("fail", PlTermv());
      if ( PlWrap<int>(q3.next_solution()) )
      { std::cerr << "*** q3 should have failed" << std::endl;
        return 3;
      }
    }
    { PlQuery q4("writelnx", av);
      if ( !PlWrap<int>(q4.next_solution()) )
      { std::cerr << "*** q4 failed" << std::endl;
        return 4;
      }
      return 5; // writelnx should have thrown an existence error
    }
  } catch ( const PlException &ex )
  { std::cerr << "ERROR: " << ex.what() << std::endl;
    return 254;
  }
  return 0;
}
