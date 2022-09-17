# Documentation
## Predicates
1. **(variable? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is variable.
1. **(sum? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is sum.
1. **(product? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is product
1. **(unit? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is variable or number.
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
1. **(math-func? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is sum, product, exponentiation, cos, sin, tan, or log.
1. **(first-element? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is list and has at least one element.
1. **(second-element? \<expr\>) -> boolean?**
   - A predicate that checks if an expression is list and has at least two elements.

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