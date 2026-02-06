-module(config_ffi).
-export([getenv_safe/1, putenv_safe/2]).

%% os:getenv/1 returns the value string or the atom `false`.
%% We convert this to a Gleam-compatible Result tuple.
getenv_safe(Key) ->
    case os:getenv(binary_to_list(Key)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

%% os:putenv/2 takes charlists. We convert from binaries.
putenv_safe(Key, Value) ->
    os:putenv(binary_to_list(Key), binary_to_list(Value)).
