/*  Part of SWI-Prolog

    This example code is in the public domain
*/

#include <iostream>
#include <SWI-cpp2.h>
#include <SWI-cpp2.cpp>

using namespace std;

/* Usage:

   likes			prints who likes what
   likes x			prints what is liked by x
   likes x y			Test whether x likes y
   likes -happy			See who is happy

   Compile using:

   swipl-ld -o likes -ld g++ -goal true likes.cpp likes.pl
*/

int
body(int argc, char **argv)
{ if ( argc == 1 )
  { if ( strcmp(argv[0], "-happy") == 0 )
    { PlTermv av(1);			/* likes -happy */

      cout << "Happy people:" << endl;
      PlQuery q("happy", av);
      while( q.next_solution() )
	cout << "\t" << av[0].as_string() << endl;
    } else
    { PlTerm_var whom;
      PlQuery q("likes", PlTermv(PlTerm_atom(argv[0]), whom));
      cout << argv[0] << " likes:" << endl;
      while( q.next_solution() )
	cout << "\t" << whom.as_string() << endl;
    }
  } else if ( argc == 2 )
  { bool likes = PlCall("likes",
                        PlTermv(PlTerm_atom(argv[0]), PlTerm_atom(argv[1])));

    cout << (likes ? "yes" : "no") << endl;
  } else
    cout << "Usage: likes x [y] or likes -happy" << endl;

  return 0;
}


int
main(int argc, char **argv)
{ PlEngine e(argv[0]);

  try
  { return body(argc-1, argv+1);
  } catch ( const PlExceptionBase &ex )
  { cerr << "Exception thrown: " << ex.what() << endl;
    exit(1);
  }

  return 0;
}


