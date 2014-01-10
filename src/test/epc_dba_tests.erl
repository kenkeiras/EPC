-module(epc_dba_tests).

-include_lib("eunit/include/eunit.hrl").
-export([interpret/1]).

interpret({Operation, T}) ->
    O = interpret(T),
    case Operation of
        trunc ->
            trunc(O);
        round ->
            round(O)
    end;

interpret({Operation, T1, T2}) ->
    O1 = interpret(T1),
    O2 = interpret(T2),
    case Operation of
        'bsr' ->
            O1 bsr O2;
        'band' ->
            O1 band O2;
        '+' ->
            O1 + O2;
        '-' ->
            O1 - O2;
        '*' ->
            O1 * O2;
        'div' ->
            O1 div O2
    end;

interpret(X) ->
    X.

same_bit_to_int_test() ->
    ?assert(1 == interpret(epc_dba:same_bit_to_int(1, 1, 1))),
    ?assert(1 == interpret(epc_dba:same_bit_to_int(0, 0, 1))),
    ?assert(0 == interpret(epc_dba:same_bit_to_int(1, 0, 1))),
    ?assert(0 == interpret(epc_dba:same_bit_to_int(0, 1, 1))).
