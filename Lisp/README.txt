830730 Rendina Davide

The project was done indiviudally.



Short description of the Lisp program
----------------------------------------

This program simulates the execution of a Little Man Computer (LMC).


Is possible to simulate the execution in two ways:


1. Writing directly a list of machine instruction and call the function execution-loop State => Output-list .
   
   State is an LMC State in form: (state :acc 0 :pc 0 :mem <the list of machine instructions> :in <the input list> :out (), noflag)

   Example: (execution-loop '(state :acc 0 :pc 0 :mem (100 902 0 .. 0) :in () :out () noflag))


2. Loading assembly instructions from a text file, using the function lmc-run filename input-list => output-list

   Example: (lmc-run <filename> '(1 2 3)).




Main functions
----------------

1. one-instruction  state => new-state


Carry out one instruction of State (taken from memory with pc as index) and produce the LMC state new-state.

Receive in input a list that represent the state and return a new state, result of the operation on the state.

Returns NIL if the computation fails.




2. execution-loop state => outlist


Start the execution of the LMC State.

Return the Output list of the LMC state as we reach an halted-state.

If the execution fails, returns NIL.




3. lmc-load filename => memory


Read assembly instructions from filename and convert them into a list of machine instructions.

Returns memory if the file is well-formed, otherwise returns NIL.




4. lmc-run filename input => output


Produce a memory using lmc-load, creates a start State and call execution-loop with this State as parameter.

The start state created is in form: (state :acc 0 :pc 0 :mem <memory> :in (..) :out (..) :flag noflag).

Return the output-list returned by execution-loop.

Return NIL if: 1) lmc-load fails.	

	       2) execution-loop fails.

               3) Input is an unvalid list (there are values greather than 999 or less than 0)




