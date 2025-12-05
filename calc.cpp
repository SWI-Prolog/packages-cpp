/* Translation of calc.c example to C++
   (the "-goal true" in the following turns off the banner)

   swipl-ld -o calc -goal true calc.cpp calc.pl && ./calc 1 + 2 + 3
 */

#include <string>
#include <SWI-cpp2.h>

int main(int argc, char **argv) {

  PlEngine e(argv[0]);

  // combine all the arguments in a single string
  std::string expression;
  for (int n = 1; n < argc; n++) {
    if (n != 1) {
      expression.append(" ");
    }
    expression.append(argv[n]);
  }

  // Lookup calc/1 and make the arguments and call

  PlPredicate pred("calc", 1, "user");
  PlTerm_string h0(expression);
  PlQuery q(pred, PlTermv(h0));

  return PlWrap<int>(q.next_solution()) ? 0 : 1;
}
