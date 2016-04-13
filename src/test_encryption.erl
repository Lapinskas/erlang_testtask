%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)

-module(test_encryption).

-export([
        test/1,
	get_random_string/1,
	get_random_string/2
	]).

test(Length) ->
    Random = get_random_string(Length),
    {ok,Enc} = encryption:encrypt(Random),
    {ok,Dec} = encryption:decrypt(Enc),
    string:equal(Random,binary_to_list(Dec)).

get_random_string(Length) ->
    get_random_string(Length,
    "qwertyuiop[]asdfghjkl;zxcvbnm,./1234567890\n\r\b\t\0\1\2\3\4\5\6\7\8").

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).