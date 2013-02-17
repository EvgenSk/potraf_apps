
-module(utils).

-export([bin_to_num/1]).
-export([timestamp_to_list/1]).
-export([to_int_or_atom/1]).
-export([to_list/1]).
-export([to_integer/1]).

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X) -> float_to_list(X, [{decimals, 2}, compact]).

to_int_or_atom(X) when is_atom(X) -> X;
to_int_or_atom(X) when is_integer(X) -> X;
to_int_or_atom(X) when is_binary(X) -> to_int_or_atom(binary_to_list(X));
to_int_or_atom(X) when is_list(X) -> list_to_int_or_atom(X). 

to_integer(X) when is_atom(X) -> undefined;
to_integer(X) when is_integer(X) -> X;
to_integer(X) when is_list(X) -> to_int_or_atom(X);
to_integer(X) when is_binary(X) -> to_integer(binary_to_list(X)).

list_to_int_or_atom(X)->
    case catch list_to_integer(X) of
	{'EXIT', {badarg, _}} -> list_to_atom(X);
	N -> N
    end.

timestamp_to_list(Timestamp) ->
    {MegaSrc, SecondSrc} = Timestamp,
    case {to_int_or_atom(MegaSrc), to_int_or_atom(SecondSrc)} of
	{undefined, _} -> "undefined";
	{_, undefined} -> "undefined";
	{Mega, Second} -> to_list(Mega * 1000000 + Second)
    end.
