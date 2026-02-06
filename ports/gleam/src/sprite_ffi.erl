-module(sprite_ffi).
-export([
    exec_cmd/1,
    exec_cmd_status/1,
    exec_pipe/1,
    command_exists/1,
    home_dir/0,
    is_directory/1,
    file_exists/1,
    dirname/1,
    basename/1,
    mkdir_p/1,
    clear_screen/0,
    time_now/0
]).

%% Execute a shell command and return stdout as a binary string.
exec_cmd(Command) ->
    Result = os:cmd(binary_to_list(Command)),
    unicode:characters_to_binary(Result).

%% Execute a shell command and return the exit code.
%% Passes stdout through to the terminal.
exec_cmd_status(Command) ->
    FullCmd = binary_to_list(Command),
    Port = open_port({spawn, FullCmd}, [exit_status, binary, stderr_to_stdout]),
    collect_port_status(Port).

collect_port_status(Port) ->
    receive
        {Port, {data, Data}} ->
            io:put_chars(Data),
            collect_port_status(Port);
        {Port, {exit_status, Status}} ->
            Status
    after 600000 ->
        port_close(Port),
        1
    end.

%% Execute a piped shell command via /bin/sh -c and return exit code.
exec_pipe(Command) ->
    Cmd = "sh -c " ++ escape_for_sh(binary_to_list(Command)),
    Port = open_port({spawn, Cmd}, [exit_status, binary, stderr_to_stdout]),
    collect_port_exit(Port).

escape_for_sh(Cmd) ->
    %% Wrap in double quotes, escaping inner double quotes
    "\"" ++ escape_dquotes(Cmd, []) ++ "\"".

escape_dquotes([], Acc) ->
    lists:reverse(Acc);
escape_dquotes([$" | Rest], Acc) ->
    escape_dquotes(Rest, [$", $\\ | Acc]);
escape_dquotes([C | Rest], Acc) ->
    escape_dquotes(Rest, [C | Acc]).

collect_port_exit(Port) ->
    receive
        {Port, {data, _}} ->
            collect_port_exit(Port);
        {Port, {exit_status, Status}} ->
            Status
    after 600000 ->
        port_close(Port),
        1
    end.

%% Check if a command exists on PATH.
command_exists(Name) ->
    Result = os:cmd(binary_to_list(<<"which ", Name/binary, " 2>/dev/null">>)),
    case string:trim(Result) of
        [] -> false;
        _ -> true
    end.

%% Get the user's home directory.
home_dir() ->
    case os:getenv("HOME") of
        false -> <<"/root">>;
        Home -> list_to_binary(Home)
    end.

%% Check if a local path is a directory.
is_directory(Path) ->
    filelib:is_dir(binary_to_list(Path)).

%% Check if a local path exists.
file_exists(Path) ->
    filelib:is_file(binary_to_list(Path)).

%% Get the parent directory of a path.
dirname(Path) ->
    list_to_binary(filename:dirname(binary_to_list(Path))).

%% Get the base name of a path.
basename(Path) ->
    list_to_binary(filename:basename(binary_to_list(Path))).

%% Create a directory and parents.
mkdir_p(Path) ->
    case filelib:ensure_dir(binary_to_list(<<Path/binary, "/">>) ) of
        ok -> true;
        {error, _} -> false
    end.

%% Clear the terminal screen.
clear_screen() ->
    io:put_chars(<<"\033[2J\033[H">>),
    nil.

%% Get current time as HH:MM:SS string.
time_now() ->
    {{_, _, _}, {H, M, S}} = calendar:local_time(),
    list_to_binary(io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S])).
