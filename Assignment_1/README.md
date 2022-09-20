# Documentation
## Description
*HELPERS*
   - In this part I implement a small part of predicates that are used in my solution. All this predicates are implemented to make code easy to read.

*SUBTASK 1*:
   - In this part I implemented all functions for task 1.1.
   - Descriptions of all functions and predicates in sections below.

*SUBTASK 2*:
   - In this part I implemented all functions for task 1.2.
   - Here you can see the solution for only 2 functions (sum and product) and 2 arguments accepted by this functions.
   - The function *derivative* is represented as *derivative-old* in this subtask because I have derivative function implemented in subtask 7.
   - Descriptions of all functions in sections below.

*SUBTASK 3*:
   - In this part I implemented all functions for task 1.3.
   - Here you can see the helper functions for simplify function that represented in subtask 7.
   - Descriptions of all functions in sections below.

*SUBTASK 5*:
   - In this part I implemented all functions for task 1.5.
   - Here you can see the *to-infix* and *infix-add-symbol* functions.
   - I made this functions in such way that it works for exponentiation from SUBTASK 6 and for unlimited number of arguments.
   - Descriptions of all functions in sections below.

*SUBTASK 6*:
   - In this part I implemented all functions for task 1.6.
   - In this subtask I made helper functions for later use in subtask 7.
   - This functions are necessary to implement derivatives of exponentiation, cos, sin, tan, and log.
   - Descriptions of all functions in sections below.

*SUBTASK 7*:
   - In this part I implemented all functions for task 1.7.
   - Here you can see the implementation of derivative and simplify recursive functions.
   - *derivative* function is splitted to more functions such as *unit-derivative*, *exp-derivative* and etc. which are made to simplify the readability of the code. Each of this splitted function calls *derivative* function (except unit derivative as it returns nubmer).
   - Descriptions of all functions in sections below.

## Predicates
1. **(variable? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is variable.
1. **(sum? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is sum.
1. **(product? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is product
1. **(unit? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is variable or number.
1. **(leaf?? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is leaf and only consists of numbers or variables.
   - Here I used andmap to go through all arguments of the expression.
1. **(exp? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is exponentiation.
1. **(sin? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is sin.
1. **(cos? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is cos.
1. **(tan? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is tan.
1. **(log? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is log.
1. **(first-element? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is list and has at least one element.
1. **(second-element? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is list and has at least two elements.
1. **(math-func? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is sum, product, exponentiation, cos, sin, tan, or log.
1. **(acceptable-value? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is acceptable by task description.
1. **(acceptable-values? \<expr\>) -> boolean?**
   - A predicate that checks if an expression has values and expressions in acceptable form.

## Functions
1. **(summand-1 \<expr\>) -> any/c**
   - A function that return first summand of the expression.
1. **(summand-2 \<expr\>) -> any/c**
   - A function that return second summand of the expression.
1. **(multiplier-1 \<expr\>) -> any/c**
   - A function that return first multiplier of the expression.
1. **(multiplier-2 \<expr\>) -> any/c**
   - A function that return second multiplier of the expression.
1. **(unit-derivative \<val\> \<resp\>) -> number?**
   - A function that is used to find a derivative of a variable *var* or a number with respect to *resp*.
1. **(derivative-old \<expr\> \<resp\>) -> any/c**
   - A recursive function that computes a symbolic derivative of a given expression with respect to a given variable. 
   - This function works only with sum, product, variables and numbers. Moreover, sum and product accepts only two arguments at this step.
   - Works only with sum and product.
1. **(combiner \<args\> \<symbol\>) -> list?**
   - A function that combines symbol and variables to expression or to a single variable.
1. **(combine-nums \<expr\>) -> list?**
   - A function that combines numbers of the expression *expr* to a list with one number or to an empty list.
   - I used a filter to get list of numbers used in the expression and to apply operation of sum or product to them.
   - Works only with sum and product.
1. **(get-vars \<expr\>) -> list?**
   - A function that gets all arguments of expression except numbers.
   - I used a filter to get list of argumets that are not numbers.
   - Works only with sum and product.
1. **(simplify-at-root \<expr\>) -> list?**
   - A function that simplifies expression only with numbers and variables.
   - In this function I check conditions for simplification and call needed functions.
   - Simplifies only sum and product, other acceptable functions just returned.
1. **(infix-add-symbol \<expr\> \<symbol\>) -> list?**
   - A function that is part of *to-infix* function. This function implements the insertion of an operation symbols between the arguments of expressions.
   - I used *foldl* to iterate over all arguments of expression and add operation symbol to them. As *foldl* reversed expression and added extra operation symbol, I used *reverse* to return to the original order and *rest* to remove extra operation symbol. Moreover, *infix-add-symbol* calls *to-infix* function to infix arguments itself.
   - *to-infix* calls *infix-add-symbol*, and *infix-add-symbol* calls *to-infix*. Therefore, *to-infix* is recursive function.
   - I MADE THIS FUNCTION ONLY TO MAKE THE CODE EASIER TO READ. I MOVED THIS PART OF CODE FROM *to-infix* FUNCTION AS IT BECOME REALLY HARD TO READ.
1. **(to-infix \<expr\>) -> list?**
   - A recursive function that infix an operation symbols.
   - THIS FUNCTION IS RECURSIVE BECAUSE IT CALLS *infix-add-symbol* THAT CALLS A *to-infix* FUNCTION.
1. **(log-derivative \<expr\> \<resp\>) -> list?**
   - A function that calculates a derivative for log using derivative function. This function calculate derivative of log by parts.
1. **(exp-derivative \<expr\> \<resp\>) -> list?**
   - A function that calculates a derivative for exponentiation using derivative function. This function calculate derivative of exponentiation by parts.
1. **(sin-derivative \<expr\> \<resp\>) -> list?**
   - A function that calculates a derivative for sin using derivative function. This function calculate derivative of sin by parts.
1. **(cos-derivative \<expr\> \<resp\>) -> list?**
   - A function that calculates a derivative for cos using derivative function. This function calculate derivative of cos by parts.
1. **(tan-derivative \<expr\> \<resp\>) -> list?**
   - A function that calculates a derivative for tan using derivative function. This function calculate derivative of tan by parts.
1. **(convert-multiplication \<expr\> \<resp\>) -> list?**
   - A function that helps to differentiate product using derivative function.
   - This function has *helper* that goes through all arguments of multiplication, calculates derivative of each argument, and generates list of lists that represents sum of products (According to differentiation rules).
1. **(convert-sum \<expr\> \<resp\>) -> list?**
   - A function that helps to differentiate sum using derivative function.
   - This function goes through all arguments of sum using *map* and calculate derivative of each argument using *derivative* function.
1. **(derivative \<expr\> \<resp\>) -> list?**
   - A recursive function that differentiate given expression *expr* with respect to *resp*.
   - This function goes through all arguments of sum using *map* and calculate derivative of each argument using *derivative* function.
1. **(simplify \<expr\>) -> any/c**
   - A recursive function that simplifies given expression *expr*.
   - This function goes through expression's arguments, simplifies them, and simplifies expression itself completly.
   - I used map to go through all arguments of expression.
   - This function can return number, variable or a list.
   - Simplifies only sum and product, other acceptable functions just returned.
1. **(insert \<val\> \<ans\>) -> list?**
   - A recursive function that represents insertion sort (insert *val* to *ans*).
   - This function checks if *val* is list or variable. If *val* is variable - than it checks if *ans* contains such variable or not and adds it (if necessary). If *val* is list - than function makes recursive call for *val*.
1. **(variables-of \<expr\>) -> list?**
   - A function that returns sorted list of distinct variables used in a given expression.
   - This function uses *foldl* to go through all arguments of given exression and apply *insert* function to them.
1. **(gradient \<expr\> \<resp\>) -> list?**
   - A function that calculates a gradient of a multivariable expression.
   - This function uses *map* through *resp* - list of variables. And applies *derivative* for *expr* with respect to each element of *resp*.