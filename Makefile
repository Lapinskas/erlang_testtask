all: deps compile

deps:
	rebar get-deps

compile:
	rebar compile

clean:
	rebar clean

server:
	erl \
	  -name test_task@127.0.0.1 \
	  -setcookie test_task \
	  -pa ebin deps/*/ebin \
	  -eval "application:start(test_task)"
test:
	erl \
	  -pa ebin deps/*/ebin \
	  test_client.beam
