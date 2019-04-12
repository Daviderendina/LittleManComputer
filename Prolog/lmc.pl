%%%% 830730 Rendina Davide


%%%% -*- Mode: Prolog -*-


%%%% lmc.pl




%%%% Program that simulates a Little Man Computer.




%%% instruction/3, instruction/2
%%% instruction(Name, Type, Value)
%%% instruction(Name, Value)

%%% Standard assembly instruction set for LMC.

instruction(add, xx, 100).
instruction(sub, xx, 200).
instruction(sta, xx, 300).
instruction(lda, xx, 500).
instruction(bra, xx, 600).
instruction(brz, xx, 700).
instruction(brp, xx, 800).
instruction(dat, xx, datxx).
instruction(inp, 901).
instruction(out, 902).
instruction(hlt, 000).
instruction(dat, 0).



%%% one_instruction/2
%%% one_instruction(State, NewState)

%%% Carry out a single instruction on State.


%%% Halt instruction case, instruction in the range 0-99

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                halted_state(Acc, Pc, Mem, In, Out, Flag)):-
    nth0(Pc, Mem, Instruction),
    0 is div(Instruction, 100),
    !.


%%% Addition instruction case, instruction in the range 100-199

one_instruction(state(Acc, Pc, Mem, In, Out, _Flag),
                state(NewAcc, NewPc, Mem, In, Out, NewFlag)):-
    nth0(Pc, Mem, Instruction),
    1 is div(Instruction, 100),
    !,
    MemPosition is mod(Instruction, 100),
    nth0(MemPosition, Mem, MemValue),
    TmpAcc is Acc + MemValue,
    set_operation_flag(TmpAcc, NewFlag),
    NewAcc is mod(TmpAcc, 1000),
    increment_pc(Pc, NewPc).


%%% Subtract instruction case, instruction in the range 200-299

one_instruction(state(Acc, Pc, Mem, In, Out, _Flag),
                state(NewAcc, NewPc, Mem, In, Out, NewFlag)):-
    nth0(Pc, Mem, Instruction),
    2 is div(Instruction,100),
    !,
    MemPosition is mod(Instruction, 100),
    nth0(MemPosition, Mem, MemValue),
    TmpAcc is Acc - MemValue,
    set_operation_flag(TmpAcc, NewFlag),
    NewAcc is mod(TmpAcc, 1000),
    increment_pc(Pc, NewPc).


%%% Store instruction case, instruction in the range 300-399

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(Acc, NewPc, NewMem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction),
    3 is div(Instruction,100),
    !,
    MemPosition is mod(Instruction, 100),
    nth0(MemPosition, Mem, _X, MemTmp),
    nth0(MemPosition, NewMem, Acc, MemTmp),
    increment_pc(Pc, NewPc).


%%% Load instruction case, instruction in the range 500-599

one_instruction(state(_Acc, Pc, Mem, In, Out, Flag),
                state(NewAcc, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction),
    5 is div(Instruction, 100),
    !,
    MemPosition is mod(Instruction, 100),
    nth0(MemPosition, Mem, NewAcc, _MemTmp),
    increment_pc(Pc, NewPc).


%%% Branch instruction case, instruction in the range 600-699

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(Acc, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction),
    6 is div(Instruction, 100),
    !,
    NewPc is mod(Instruction, 100).


%%% Branch if zero instruction case, instruction in the range 700-799

one_instruction(state(0, Pc, Mem, In, Out, noflag),
                state(0, NewPc, Mem, In, Out, noflag)) :-
    nth0(Pc, Mem, Instruction),
    7 is div(Instruction ,100),
    !,
    NewPc is mod(Instruction, 100).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(Acc, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction),
    7 is div(Instruction,100),
    !,
    increment_pc(Pc, NewPc).


%%% Branch if positive instruction case, instruction in the range 800-899

%%% The first predicate is true when the branch succeed, otherwise the
%%% second is taken and the pc is incremented by 1

one_instruction(state(Acc, Pc, Mem, In, Out, noflag),
                state(Acc, NewPc, Mem, In, Out, noflag)) :-
    nth0(Pc, Mem, Instruction),
    8 is div(Instruction, 100),
    !,
    NewPc is mod(Instruction, 100).

one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(Acc, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, Instruction),
    8 is div(Instruction, 100),
    !,
    increment_pc(Pc, NewPc).


%%% Input instruction case, instruction has the value 901

%%% If the input list is empty, the predicate doesn't unify and
%%% one_instruction fails

one_instruction(state(_Acc, Pc, Mem, [I | In] , Out, Flag),
                state(I, NewPc, Mem, In, Out, Flag)) :-
    nth0(Pc, Mem, 901),
    !,
    increment_pc(Pc, NewPc).


%%% Output instruction case, instruction has the value 902

one_instruction(state(Acc, Pc, Mem, In , Out, Flag),
                state(Acc, NewPc, Mem, In, Outlist, Flag)) :-
    nth0(Pc, Mem, 902), !,
    increment_pc(Pc, NewPc),
    append(Out, [Acc], Outlist).


%%% If the predicate doesn't unify with no one predicate, is an illegal
%%% instruction

one_instruction((state(_Acc, _Pc, _Mem, _In, _Out, _Flag)), void):-
    fail.



%%% execution_loop/2
%%% execution_loop(State, Output)

%%% Simulate the execution starting from a State and put the result in Output

execution_loop(state(Acc, Pc, Mem, In, Out, Flag), Outlist) :-
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState),
    execution_loop(NewState, Outlist).

execution_loop(halted_state(_Acc, _Pc, _Mem, _In, Out, _Flag), Out).



%%% increment_pc/2

%%% Valid if the new value of Pc is NewPc.

increment_pc(Pc, NewPc) :-
    TmpPc is Pc + 1,
    NewPc is mod(TmpPc, 100).



%%% set_flag/2
%%% set_flag(Value, Flag)

%%% Valid if LMC state's flag with accumulator Value is Flag.

set_operation_flag(Value, flag) :-
    compare('>', Value, 999),
    !.

set_operation_flag(Value, flag) :-
    compare('<', Value, 0),
    !.

set_operation_flag(_Value, noflag).



%%% read_from_file/2

%%% Read instructions from Filename into Input.

read_from_file(Filename, Input) :-
    exists_file(Filename),
    open(Filename, read, Stream),
    stream_ended(Stream, Flag),
    read_from_stream(Stream, Flag, Input).



%%% read_from_stream/3
%%% read_from_stream(Stream, EofFlag, OutList)

%%% Read from Stream into OutList.

read_from_stream(_Stream, 1, []) :- !.

read_from_stream(Stream, 0, OutList) :-
    read_line_to_codes(Stream, Codes),
    stream_ended(Stream, Flag),
    read_from_stream(Stream, Flag, R),
    append([Codes], R, OutList).



%%% stream_ended/2

%%% Valid if Stream is at the end

stream_ended(Stream, 1) :-
    at_end_of_stream(Stream),
    !.

stream_ended(_Stream, 0).



%%% format_multiple_instruction/3
%%% format_multiple_instruction(InstructionList, FileLine,
%%%  InstructionFormattedList)

%%% Format InstructionList into InstructionFormattedList.

format_multiple_instruction([], _, []) :- !.

format_multiple_instruction([I | Is], FileLine, Res) :-
    format_instruction(I, FileLine, []),
    !,
    format_multiple_instruction(Is, FileLine, Res).

format_multiple_instruction([I | Is], FileLine, [FormattedI | Res]) :-
    format_instruction(I, FileLine, FormattedI),
    NewLine is FileLine + 1,
    format_multiple_instruction(Is, NewLine, Res).



%%% format_instruction/3
%%% format_instruction(Instruction, FileLine, InstructionFormatted)

%%% Format single Instruction into InstructionFormatted.

format_instruction([], _, []).

format_instruction(Instruction, _FileLine, []) :-
    ignore_comment(Instruction, []).

format_instruction(Instruction, _FileLine, []) :-
    split_string(Instruction, " ", " ", [""]).

format_instruction(Instruction, FileLine, FinalInstruction) :-
    ignore_comment(Instruction, InstrNoComm),
    split_string(InstrNoComm, " ", " ", Line),
    build_instruction(Line, FileLine, FinalInstruction).



%%% ignore_comment/2
%%% ignore_comment(Instruction, ResultInstruction)

%%% True if ResultInstruction is Intruction without comments.

ignore_comment([], []) :- !.

ignore_comment([X], [X]) :- X \= 47, !.

ignore_comment([47, 47 | _Xs], []) :- !.

ignore_comment([47, _X | _Xs], []) :-
    !,
    fail.

ignore_comment([X, Y | Xs], [X | R]) :-
    ignore_comment([Y | Xs], R).



%%% build_instruction/3
%%% build_instruction(Instruction, FileLine, FinalInstruction)

%%% Build Instruction structure.
%%% FinalInstruction is in the form: [Label, Instruction, Value]

build_instruction([Instruction],
                  Line,
                  [[], DowncaseInstr, [], Line]) :-
    downcase_atom(Instruction, DowncaseInstr),
    !.

build_instruction([Instruction, Value],
                  Line,
                  [[], DowncaseInstr, Value, Line]) :-
    downcase_atom(Instruction, DowncaseInstr),
    is_instruction(DowncaseInstr),
    !.

build_instruction([Label, Instruction],
                  Line,
                  [DowncaseLabel, DowncaseInstr, [], Line]) :-
    downcase_atom(Instruction, DowncaseInstr),
    downcase_atom(Label, DowncaseLabel),
    !.

build_instruction([Label, Instruction, Value],
                  Line,
                  [DowncaseLabel, DowncaseInstr, Value, Line]) :-
    downcase_atom(Instruction, DowncaseInstr),
    downcase_atom(Label, DowncaseLabel).



%%% is_instruction/1

%%% Valid if InstructionName is a valid assembly instruction.

is_instruction(InstructionName) :-
    instruction(InstructionName, _Value).

is_instruction(InstructionName) :-
    instruction(InstructionName, _Type, _Value).



%%% load_multiple_labels/2
%%% load_multiple_labels(InstructionFormatted, InstructionsNoLabel)

%%% Assert the label present in InstructionList.
%%% InstructionsNoLabel is the list of instruction without labels.

load_multiple_labels([], []) :- !.

load_multiple_labels([[[], Instruction, Data, _Counter] | Is],
                     [[Instruction, Data] | Result]) :-
   !,
   load_multiple_labels(Is, Result).

load_multiple_labels([I | Is],
                     [NewInstruction | Result]) :-
    load_label(I, NewInstruction),
    load_multiple_labels(Is, Result).



%%% load_label/2
%%% load_label(Instruction, InstructionNoLabel)

%%% Assert the label, if present, of Instruction.
%%% InstructionNoLabel is Instruction without label.

load_label([[], Instruction, [], _Counter],
           [Instruction, []]) :-
    !.

load_label([[], Instruction, MemPosition, _Counter],
           [Instruction, MemPosition]) :-
    !.

load_label([Label, Instruction, [], Counter],
           [Instruction, []]) :-
    !,
    downcase_atom(Label, DowncaseLabel),
    is_valid_label(DowncaseLabel),
    assertz(label(DowncaseLabel, Counter)).

load_label([Label, Instruction, MemPos, Counter],
           [Instruction, MemPos]) :-
    !,
    downcase_atom(Label, DowncaseLabel),
    is_valid_label(DowncaseLabel),
    assertz(label(DowncaseLabel, Counter)).



%%% is_valid_label/1

%%% Valid if LabelName is a valid label name.

is_valid_label(LabelName) :-
    label_not_exists(LabelName),
    atom_codes(LabelName, [First | Codes]),
    char_type(First, alpha),
    label_legal_char(Codes).



%%% label_legal_char/1
%%% label_legal_char(LabelName)

%%% Valid if LabelName has legal character

label_legal_char([]).

label_legal_char([X | Xs]) :-
    char_type(X, alnum),
    label_legal_char(Xs).



%%% label_not_exists/1

%%% Valid if Label not exists.

label_not_exists(Label) :-
    label(Label, _),
    !, fail.

label_not_exists(_Label).



%%% load_multiple_instruction/2
%%% load_multiple_instruction(InstructionList, Memory)

%%% Load machine instructions from InstructionList into Memory list.

load_multiple_instruction([], []) :- !.

load_multiple_instruction([I | Is], [InstructionValue | Result]) :-
    get_instruction(I, InstructionValue),
    load_multiple_instruction(Is, Result).



%%% get_instruction/2
%%% get_instruction(Instruction, ResultInstruction)

%%% Valid if machine instruction of Instruction is ResultInstruction.

get_instruction([Instruction, []], ResultInstruction) :-
    !,
    instruction(Instruction, ResultInstruction).

get_instruction([Instruction, MemPosition], ResultInstruction) :-
    instruction(Instruction, xx, datxx),
    !,
    atom_number(MemPosition, ResultInstruction),
    between(0, 999, ResultInstruction).

get_instruction([Instruction, Label], ResultInstruction) :-
    instruction(Instruction, xx, Value),
    downcase_atom(Label, LabelDown),
    label(LabelDown, Pos),
    !,
    ResultInstruction is Value + Pos.

get_instruction([Instruction, MemPosition], ResultInstruction) :-
    instruction(Instruction, xx, Value),
    atom_number(MemPosition, MemPositionNumber),
    between(0, 99, MemPositionNumber),
    ResultInstruction is Value + MemPositionNumber.



%%% lmc_load/2

%%% Load assembly instruction from Filename into Memory.

lmc_load(Filename, Memory) :-
    exists_file(Filename),
    init_labels(),
    read_from_file(Filename, In),
    format_multiple_instruction(In, 0, InstructionsFormatted),
    load_memory(InstructionsFormatted, Memory).



%%% load_memory/2

%%% Load assembly instructions form InstructionsFormatted into Memory.

load_memory(InstructionsFormatted, Memory) :-
    load_multiple_labels(InstructionsFormatted, InstructionList),
    load_multiple_instruction(InstructionList, TmpMem),
    adjust_memory(TmpMem, Memory).



%%% adjust_memory/2

%%% Valid if Result is Memory with 0s in Tail, if Memory length is <100.

adjust_memory(Memory, Result) :-
    length(Memory, MemLength),
    compare('<', MemLength, 101),
    MissingValues is 100 - MemLength,
    generate_list(MissingValues, MissingList),
    append(Memory, MissingList, Result).



%%% generate_list/2
%%% generate_list(Number, List)

%%% Generate a list of 0s with length equal to Number.

generate_list(0, []) :- !.

generate_list(Number, [0 | Result]) :-
    N2 is Number - 1,
    generate_list(N2, Result).



%%% init_labels/0

%%% Remove, if present, labels asserted previously.

init_labels() :-
    retractall(label(_,_)),
    %% assert one void label in order to avoid exceptions
    assertz(label("","")).



%%% lmc_run/3

%%% Simulate the execution of LMC Machine and put result into Output.

lmc_run(Filename, InputList, Output) :-
    lmc_load(Filename, Mem),
    is_valid_list(InputList),
    execution_loop(state(0, 0, Mem, InputList, [], noflag), Output).



%%% is_valid_list/1

%%% valid if a list has values between 0 and 999

is_valid_list([]) :- !.

is_valid_list([X | Xs]) :-
    between(0, 999, X),
    is_valid_list(Xs).







%%%% end of file -- lmc.pl



































