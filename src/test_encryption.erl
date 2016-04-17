-module(test_encryption).

-export([
    test/1,
    test_iterations/1
    ]).

test_iterations(N) ->
    case N of
	0 -> io:fwrite("Test finished~n");
	_ ->
	    {{_,Result},_,_} = test(random_string(2000)),
	    io:fwrite("Test iteration ~p~n", [{N, Result}]),
	    test_iterations(N-1)
    end.

test(Data) ->
    {ok,Chipher} = encryption:enc(Data),
    {ok,Plain} = encryption:dec(Chipher),
    {
	{"test passed:",string:equal(Data,Plain)},
	{"original text:",Data},
	{"text after encryption/decryption operations:",Plain}
    }.

random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 !@#$%^&*()_+=-,.<>\;'|:'\"`\0\1\2\3\4\5\6\7\8\9\n\r"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).