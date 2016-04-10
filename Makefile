all: deps compile

deps:
	rebar get-deps

compile:
	rebar compile

clean:
	rebar clean

run:
	erl \
	  -name rpc_demo@127.0.0.1 \
	  -setcookie rpc_demo \
	  -pa ebin deps/*/ebin \
	  -eval "application:start(rpc_demo)"
