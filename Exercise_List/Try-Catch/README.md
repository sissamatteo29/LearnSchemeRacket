Define a new syntax form to simulate a try-catch block.
The try block takes a series of expressions and evaluates them. If all the expressions are executed without errors,
the value of the last expression is returned.
The catch block starts with a string describing the error that is caught and then there is a series of expressions to
evaluate in case the error is raised.
If an error which is not described in the catch is raised, a delegation mechanism has to be implemented, so that
the catch blocks of the other functions (in higher positions of the control flow graph) can be executed.