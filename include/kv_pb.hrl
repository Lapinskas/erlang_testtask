-ifndef(DATA_PB_H).
-define(DATA_PB_H, true).
-record(data, {
    key = erlang:error({required, key}),
    value = erlang:error({required, value})
}).
-endif.

-ifndef(SET_REQUEST_PB_H).
-define(SET_REQUEST_PB_H, true).
-record(set_request, {
    req = erlang:error({required, req})
}).
-endif.

-ifndef(SET_RESPONSE_PB_H).
-define(SET_RESPONSE_PB_H, true).
-record(set_response, {
    error = erlang:error({required, error})
}).
-endif.

-ifndef(GET_REQUEST_PB_H).
-define(GET_REQUEST_PB_H, true).
-record(get_request, {
    key = erlang:error({required, key})
}).
-endif.

-ifndef(GET_RESPONSE_PB_H).
-define(GET_RESPONSE_PB_H, true).
-record(get_response, {
    error = erlang:error({required, error}),
    req
}).
-endif.

-ifndef(REQ_ENVELOPE_PB_H).
-define(REQ_ENVELOPE_PB_H, true).
-record(req_envelope, {
    type = erlang:error({required, type}),
    set_req,
    set_resp,
    get_req,
    get_resp
}).
-endif.

