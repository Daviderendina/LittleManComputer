830730 Rendina Davide

The project was done indiviually.



Short description of the Prolog program
----------------------------------------

This program simulates the execution of a Little Man Computer (LMC).


It's possible to simulate the execution in two ways:


1. Writing directly a list of machine instruction and call execution_loop(State, Outlist).
   
   State is an LMC State in form: state(0, 0, <the list of machine instructions>, <the input list>, [], noflag)

   Example: State = state(0, 0, [100, 902, 0 .. 0], [], [], noflag), execution_loop(State, Outlist).


2. Loading assembly instructions from a text file, using the predicate lmc-run/3.

   Example: lmc_run(<filename>, [1, 2, 3], Outlist).




Main predicates
----------------

1. one-instruction/2  -  one-instruction(State, NewState)


Carry out one instruction of State (taken from memory with pc as index) and produce the LMC state NewState.

Valid if the execution of the instruction of State produces NewState.

Fails if: 1) There are some invalid instructions (400-499, 903-999)

          2) Try to carry out an input instruction without empty list as Input list.




2. execution_loop/2  -  execution_loop(State, Outlist)


Start the execution of the LMC State.

Valid if the execution of State, using one_instruction/2, produce the result Outlist when the State reach an halted_state.

Fails if one_instruction/2 fails.




3. lmc_load/2  -  lmc_load (Filename, Memory)


Read assembly instructions from filename and convert them into a list of machine instructions.

Valid if the process of convertion of the assembly Filename produces Memory.

Fails if the file in not well formed.




4. lmc_run/3  -  lmc_run(Filename, Input, Output)


Produce a memory using lmc-load(Filename, Memory), creates a start State and call execution_loop(State, Output).

The start state created is in form: state(0, 0, Memory, Input, [], noflag).

Valid if the lmc_run produce Output list with given Filename and Input (the Input list of the State).

Fails if: 1) lmc_load/3 fails.	

          2) execution_loop/2 fails.

          3) Input is an unvalid list (there are values greather than 999 or less than 0)



