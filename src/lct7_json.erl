-module(lct7_json).

-export([encode/1, decode/1]).

-define(Is_number_start(C), (C == $-) or ((C >= $0) and (C =< $9))).

%% encoder
encode(Term) when is_list(Term) or is_map(Term) ->
    encode(Term, <<>>);
encode(Term) when is_binary(Term) ->
    <<"\"", Term/binary, "\"">>;
encode(Term) when is_integer(Term) ->
    integer_to_binary(Term);
encode(Term) when is_float(Term) ->
    float_to_binary(Term);
encode(true) ->
    <<"true">>;
encode(false) ->
    <<"false">>;
encode(null) ->
    <<"null">>;
encode({K, V}) ->
    Key = encode(K),
    Value = encode(V),
    <<"{\"key\":", Key/binary, ",\"value\":", Value/binary, "}">>;
encode(Term) when is_atom(Term) ->
    Bin = atom_to_binary(Term),
    <<"\"", Bin/binary, "\"">>.

encode([H | T], <<>>) ->
    Bin = encode(H),
    encode(T, <<"[", Bin/binary>>);
encode([H | T], Acc) ->
    Bin = encode(H),
    encode(T, <<Acc/binary, ",", Bin/binary>>);
encode([], Acc) ->
    <<Acc/binary, "]">>;

encode(Term, Acc) when is_map(Term) ->
    Bin = maps:fold(
        fun(K, V, Acc1) ->
            Kb = encode(K),
            Vb = encode(V),
            <<Acc1/binary, Kb/binary, ":", Vb/binary>>
        end,
        <<>>,
        Term
    ),
    <<Acc/binary, "{", Bin/binary, "}">>.

%% Decoder
decode(Json) ->
    try
        {Acc, <<>>} = do_decode(string:trim(Json)),
        {ok, Acc}
    catch _:_ -> 
        {error, <<"JSON processing error">>}
    end.

do_decode(<<"[", _Rest/binary>> = Bin) ->
    decode_val(Bin);
do_decode(<<"{", _Rest/binary>> = Bin) ->
    decode_val(Bin).

decode_val(<<"[", Rest/binary>>) ->
    decode_arr(string:trim(Rest, leading));
decode_val(<<"{", Rest/binary>>) ->
    decode_obj(string:trim(Rest, leading));
decode_val(<<"\"", Rest/binary>>) ->
    decode_str(Rest, <<>>);
decode_val(<<C, Rest/binary>>) when ?Is_number_start(C) ->
    decode_num(Rest, {<<C>>, int});
decode_val(<<"true", Rest/binary>>) ->
    {true, Rest};
decode_val(<<"false", Rest/binary>>) ->
    {false, Rest};
decode_val(<<"null", Rest/binary>>) ->
    {null, Rest}.

decode_arr(Bin) ->
    decode_arr(Bin, []).

decode_arr(<<"]", Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_arr(Bin, Acc) ->
    {Val, Rest} = decode_val(string:trim(Bin, leading)),
    Rest1 = string:trim(Rest, leading),
    case Rest1 of
        <<",", Rest2/binary>> -> decode_arr(string:trim(Rest2), [Val | Acc]);
        <<"]", Rest2/binary>> -> {lists:reverse([Val |Acc]), Rest2}
    end.

decode_obj(Bin) ->
    decode_obj(Bin, #{}).

decode_obj(<<"}", Rest/binary>>, Acc) ->
    {Acc, Rest};

decode_obj(Bin, Acc) ->
    {Key, Rest} = decode_key(string:trim(Bin, leading)),
    Rest1 = string:trim(Rest, leading),
    <<":", Rest2/binary>> = Rest1,
    {Val, Rest3} = decode_val(string:trim(Rest2, leading)),
    Acc1 = maps:put(Key, Val, Acc),
    Rest4 = string:trim(Rest3, leading),
    case Rest4 of
        <<",", Rest5/binary>> -> decode_obj(string:trim(Rest5, leading), Acc1);
        <<"}", _Rest5/binary>> -> decode_obj(Rest4, Acc1)
    end.

decode_key(<<"\"", Rest/binary>>) ->
    decode_str(Rest, <<>>).

decode_str(<<"\\\\", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\\\\">>);
decode_str(<<"\\\"", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\\\"">>);
decode_str(<<"\"", Rest/binary>>, Acc) ->
    {Acc, Rest};
decode_str(<<C/utf8, Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, C/utf8>>).

decode_num(<<C, Rest/binary>>, {Num, T}) when C >= $0, C=< $9 ->
    decode_num(Rest, {<<Num/binary, C>>, T});
decode_num(<<".", Rest/binary>>, {Num, int}) ->
    decode_num(Rest, {<<Num/binary, ".">>, int});
decode_num(<<"-", Rest/binary>>, {Num, float}) ->
    decode_num(Rest, {<<Num/binary, "-">>, float});
decode_num(<<"e", Rest/binary>>, {Num, float}) ->
    decode_num(Rest, {<<Num/binary, "e">>, float});
decode_num(<<"E", Rest/binary>>, {Num, float}) ->
    decode_num(Rest, {<<Num/binary, "e">>, float});
decode_num(Bin, {Num, int}) ->
    {binary_to_integer(Num), Bin};
decode_num(Bin, {Num, float}) ->
    {binary_to_float(Num), Bin}.
