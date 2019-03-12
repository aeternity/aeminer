%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%    A library providing Cuckoo Cycle PoW generation and verification.
%%%    Using (as an independent OS process) the C/C++ Cuckoo Cycle implementation of
%%%    John Tromp:  https://github.com/tromp/cuckoo
%%%    White paper: https://github.com/tromp/cuckoo/blob/master/doc/cuckoo.pdf?raw=true
%%%
%%%    We use erlang:open_port/2 to start an OS process that runs this C code.
%%%    The reasons for using this over os:cmd and erlexec are:
%%%      - no additional C-based dependency which is Unix-focused and therefore hard to port to Windows
%%%      - widely tested and multiplatform-enabled solution
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aeminer_pow_cuckoo).

-export([config/7,
         addressed_instances/1,
         repeats/1,
         exec/1,
         extra_args/1,
         hex_enc_header/1,
         get_node_size/1
        ]).

-export([generate/5,
         verify/5
        ]).

-export_type([hashable/0,
              exec/0,
              exec_group/0,
              extra_args/0,
              hex_enc_header/0,
              repeats/0,
              instance/0,
              instances/0,
              edge_bits/0,
              solution/0,
              config/0
             ]).

-ifdef(TEST).
-export([verify_proof_/3,
         solution_to_binary/2
        ]).
-endif.

-include("aeminer.hrl").

-type hashable()          :: aeminer_blake2b_256:hashable().

-type nonce()             :: aeminer_pow:nonce().

-type sci_target()        :: aeminer_pow:sci_target().

-type instance()          :: aeminer_pow:instance()
                           | undefined.

-type exec()              :: string().

-type exec_group()        :: binary().

-type extra_args()        :: string().

-type hex_enc_header()    :: boolean().

-type repeats()           :: non_neg_integer().

-type edge_bits()         :: pos_integer().

-type instances()         :: [aeminer_pow:instance()]
                           | undefined.

-type solution()          :: [integer()].

-type output_parser_fun() :: fun(([string()], state()) ->
                                    {ok, term(), term()} | {error, term()}).

-record(config, {
          exec            :: exec(),
          exec_group      :: exec_group(),
          extra_args      :: extra_args(),
          hex_enc_header  :: hex_enc_header(),
          repeats         :: repeats(),
          edge_bits       :: edge_bits(),
          instances       :: instances()
         }).

-opaque config()          :: #config{}.

-record(state, {
          os_pid          :: integer() | undefined,
          port            :: port() | undefined,
          buffer = []     :: string(),
          target          :: sci_target() | undefined,
          edge_bits       :: edge_bits(),
          parser          :: output_parser_fun()
         }).

-type state()             :: #state{}.

-define(IS_CONFIG(Exec, ExecGroup, ExtraArgs, HexEncHdr, Repeats, EdgeBits, Instances),
       is_binary(Exec) and is_binary(ExecGroup) and
       is_binary(ExtraArgs) and is_boolean(HexEncHdr) and
       (is_integer(Repeats) and (Repeats > 0)) and
       (is_integer(EdgeBits) and (EdgeBits > 0)) and
       (is_list(Instances) or (Instances =:= undefined))).

-define(debug(F, A), aeminer:debug(F, A)).
-define(info(F, A),  aeminer:info(F, A)).
-define(warning(F, A), aeminer:warning(F, A)).
-define(error(F, A), aeminer:error(F, A)).

%%%=============================================================================
%%% API
%%%=============================================================================

config(Exec, ExecGroup, ExtraArgs, HexEncHdr, Repeats, EdgeBits, Instances) when
      ?IS_CONFIG(Exec, ExecGroup, ExtraArgs, HexEncHdr, Repeats, EdgeBits, Instances) ->
    #config{
       exec           = binary_to_list(Exec),
       exec_group     = ExecGroup,
       extra_args     = binary_to_list(ExtraArgs),
       hex_enc_header = HexEncHdr,
       repeats        = Repeats,
       edge_bits      = EdgeBits,
       instances      = Instances}.

-spec addressed_instances(config()) -> instances().
addressed_instances(#config{instances = Instances}) ->
    Instances.

-spec repeats(config()) -> repeats().
repeats(#config{repeats = Repeats}) ->
    Repeats.

-spec exec(config()) -> exec().
exec(#config{exec = Exec}) ->
    Exec.

-spec extra_args(config()) -> extra_args().
extra_args(#config{extra_args = ExtraArgs}) ->
    ExtraArgs.

-spec hex_enc_header(config()) -> hex_enc_header().
hex_enc_header(#config{hex_enc_header = HexEncHdr}) ->
    HexEncHdr.

%%------------------------------------------------------------------------------
%% Proof of Work generation with default settings
%%
%% According to my experiments, increasing the number of trims from the default
%% 7 in John Tromp's code does not really affect speed, reducing it causes failure.
%%
%% Measured execution times (seconds) for 7 trims for threads:
%%   1: 44.61 46.92 46.41
%%   2: 15.17 15.75 19.19
%%   3:  9.35 10.92  9.73
%%   4: 10.66  7.83 10.02
%%   5:  7.41  7.47  7.32
%%  10:  7.27  6.70  6.38
%%  20:  6.25  6.74  6.41
%%
%%  Very slow below 3 threads, not improving significantly above 5, let us take 5.
%%------------------------------------------------------------------------------
-spec generate(hashable(), sci_target(), nonce(), config(), instance()) ->
    {ok, {nonce(), solution()}} | {error, no_solution} | {error, {runtime, term()}}.
generate(Data, Target, Nonce, Config, Instance) when
      Nonce >= 0, Nonce =< ?MAX_NONCE ->
    %% Hash Data and convert the resulting binary to a base64 string for Cuckoo
    %% Since this hash is purely internal, we don't use api encoding
    Hash   = aeminer_blake2b_256:hash(Data),
    Hash64 = base64:encode_to_string(Hash),
    ?debug("Generating solution for data hash ~p and nonce ~p with target ~p.",
           [Hash, Nonce, Target]),
    case generate_int(Hash64, Nonce, Target, Config, Instance) of
        {ok, Nonce1, Soln} ->
            {ok, {Nonce1, Soln}};
        {error, no_value} ->
            ?debug("No cuckoo solution found", []),
            {error, no_solution};
        {error, Rsn} ->
            %% Exec failed (segfault, not found, etc.): let miner decide
            {error, {runtime, Rsn}}
    end.

%%------------------------------------------------------------------------------
%% Proof of Work verification (with difficulty check)
%%------------------------------------------------------------------------------
-spec verify(hashable(), nonce(), solution(), sci_target(), edge_bits()) ->
    boolean().
verify(Data, Nonce, Soln, Target, EdgeBits) when
      is_list(Soln), Nonce >= 0, Nonce =< ?MAX_NONCE ->
    Hash = aeminer_blake2b_256:hash(Data),
    case test_target(Soln, Target, EdgeBits) of
        true ->
            verify_proof(Hash, Nonce, Soln, EdgeBits);
        false ->
            false
    end.

%% Internal functions.

generate_int(Hash, Nonce, Target,
             #config{exec = Exec, extra_args = ExtraArgs0,
                     hex_enc_header = HexEncHdr} = Config, Instance) ->
    ExtraArgs   = case is_miner_instance_addressation_enabled(Config) of
                      true  -> ExtraArgs0 ++ " -d " ++ integer_to_list(Instance);
                      false -> ExtraArgs0
                  end,
    EncodedHash = case HexEncHdr of
                      true  -> hex_string(Hash);
                      false -> Hash
                  end,
    ExecBinDir  = exec_bin_dir(Config),
    generate_int(EncodedHash, Nonce, Target, ExecBinDir, Exec, ExtraArgs, Config).

generate_int(Hash, Nonce, Target, MinerBinDir, MinerBin, MinerExtraArgs,
             #config{repeats = Repeats0, edge_bits = EdgeBits}) ->
    Repeats = integer_to_list(Repeats0),
    Args = ["-h", Hash, "-n", integer_to_list(Nonce), "-r", Repeats | string:tokens(MinerExtraArgs, " ")],
    ?info("Executing cmd '~s ~s'", [MinerBin, lists:concat(lists:join(" ", Args))]),
    Old = process_flag(trap_exit, true),
    try exec_run(MinerBin, MinerBinDir, Args) of
        {ok, Port, OsPid} ->
            wait_for_result(#state{os_pid = OsPid,
                                   port = Port,
                                   buffer = [],
                                   target = Target,
                                   edge_bits = EdgeBits,
                                   parser = fun parse_generation_result/2});
        {error, _Rsn} = Err ->
            Err
    catch
        C:E ->
            {error, {unknown, {C, E}}}
    after
        process_flag(trap_exit, Old),
        receive
            {'EXIT',_From, shutdown} -> exit(shutdown)
        after 0 -> ok
        end
    end.

hex_string(S) ->
    Bin = list_to_binary(S),
    lists:flatten([io_lib:format("~2.16.0B", [B]) || <<B:8>> <= Bin]).

is_miner_instance_addressation_enabled(#config{instances = Instances}) ->
    case Instances of
        I when is_list(I) -> true;
        undefined         -> false
    end.

exec_bin_dir(#config{exec_group = ExecGroup}) ->
    case ExecGroup of
        <<"aecuckoo">>         -> aecuckoo:bin_dir();
        <<"aecuckooprebuilt">> -> code:priv_dir(aecuckooprebuilt)
    end.

-define(POW_TOO_BIG(Nonce), {error, {nonce_too_big, Nonce}}).
-define(POW_TOO_SMALL(Nonce, PrevNonce), {error, {nonces_not_ascending, Nonce, PrevNonce}}).
-define(POW_NON_MATCHING, {error, endpoints_do_not_match_up}).
-define(POW_BRANCH, {error, branch_in_cycle}).
-define(POW_DEAD_END, {error, cycle_dead_ends}).
-define(POW_SHORT_CYCLE, {error, cycle_too_short}).

%%------------------------------------------------------------------------------
%% @doc
%%   Proof of Work verification (difficulty check should be done before calling
%%   this function)
%% @end
%%------------------------------------------------------------------------------

verify_proof(Hash, Nonce, Solution, EdgeBits) ->
    %% Cuckoo has an 80 byte header, we have to use that as well
    %% packed Hash + Nonce = 56 bytes, add 24 bytes of 0:s
    Header0 = pack_header_and_nonce(Hash, Nonce),
    Header = <<(list_to_binary(Header0))/binary, 0:(8*24)>>,
    verify_proof_(Header, Solution, EdgeBits).

verify_proof_(Header, Solution, EdgeBits) ->
    {K0, K1, K2, K3} = aeminer_siphash24:create_keys(Header),

    EdgeMask = (1 bsl EdgeBits) - 1,
    try
        %% Generate Uv pairs representing endpoints by hashing the proof
        %% XOR points together: for a closed cycle they must match somewhere
        %% making one of the XORs zero.
        {Xor0, Xor1, _, Uvs} =
        lists:foldl(
          fun(N, _) when N > EdgeMask ->
                  throw(?POW_TOO_BIG(N));
             (N, {_Xor0, _Xor1, PrevN, _Uvs}) when N =< PrevN ->
                  throw(?POW_TOO_SMALL(N, PrevN));
             (N, {Xor0C, Xor1C, _PrevN, UvsC}) ->
                  Uv0 = sipnode(K0, K1, K2, K3, N, 0, EdgeMask),
                  Uv1 = sipnode(K0, K1, K2, K3, N, 1, EdgeMask),
                  {Xor0C bxor Uv0, Xor1C bxor Uv1, N, [{Uv0, Uv1} | UvsC]}
          end, {16#0, 16#0, -1, []}, Solution),
        case Xor0 bor Xor1 of
            0 ->
                %% check cycle
                case check_cycle(Uvs) of
                    ok -> true;
                    {error, E} -> throw(E)
                end;
            _ ->
                %% matching endpoints imply zero xors
                throw(?POW_NON_MATCHING)
        end
    catch
        throw:{error, Rsn} ->
            ?info("Proof verification failed for ~p: ~p", [Solution, Rsn]),
            false
    end.

sipnode(K0, K1, K2, K3, Proof, UOrV, EdgeMask) ->
    SipHash = aeminer_siphash24:hash(K0, K1, K2, K3, 2*Proof + UOrV) band EdgeMask,
    (SipHash bsl 1) bor UOrV.

check_cycle(Nodes0) ->
    Nodes = lists:keysort(2, Nodes0),
    {Evens0, Odds} = lists:unzip(Nodes),
    Evens  = lists:sort(Evens0), %% Odd nodes are already sorted...
    UEvens = lists:usort(Evens),
    UOdds  = lists:usort(Odds),
    %% Check that all nodes appear exactly twice (i.e. each node has
    %% exactly two edges).
    case length(UEvens) == (?SOLUTION_SIZE div 2) andalso
         length(UOdds) == (?SOLUTION_SIZE div 2) andalso
         UOdds == Odds -- UOdds andalso UEvens == Evens -- UEvens of
        false ->
            {error, ?POW_BRANCH};
        true  ->
            [{X0, Y0}, {X1, Y0} | Nodes1] = Nodes,
            check_cycle(X0, X1, Nodes1)
    end.

%% If we reach the end in the last step everything is fine
check_cycle(X, X, []) ->
    ok;
%% If we reach the end too early the cycle is too short
check_cycle(X, X, _)  ->
    {error, ?POW_SHORT_CYCLE};
check_cycle(XEnd, XNext, Nodes) ->
    %% Find the outbound edge for XNext and follow that edge
    %% to an odd node and back again to NewXNext
    case find_node(XNext, Nodes, []) of
        Err = {error, _}            -> Err;
        {XNext, NewXNext, NewNodes} -> check_cycle(XEnd, NewXNext, NewNodes)
    end.

find_node(_, [], _Acc) ->
    {error, ?POW_DEAD_END};
find_node(X, [{X, Y}, {X1, Y} | Nodes], Acc) ->
    {X, X1, Nodes ++ Acc};
find_node(X, [{X1, Y}, {X, Y} | Nodes], Acc) ->
    {X, X1, Nodes ++ Acc};
find_node(X, [{X, _Y} | _], _Acc) ->
    {error, ?POW_DEAD_END};
find_node(X, [N1, N2 | Nodes], Acc) ->
    find_node(X, Nodes, [N1, N2 | Acc]).

%%------------------------------------------------------------------------------
%% @doc
%%   Creates the Cuckoo buffer (hex encoded) from a base64-encoded hash and a
%%   uint64 nonce.
%%   Since this hash is purely internal, we don't use api encoding.
%% @end
%%------------------------------------------------------------------------------
-spec pack_header_and_nonce(binary(), aeminer_pow:nonce()) -> string().
pack_header_and_nonce(Hash, Nonce) when byte_size(Hash) == 32 ->
    %% Cuckoo originally uses 32-bit nonces inserted at the end of its 80-byte buffer.
    %% This buffer is hashed into the keys used by the main algorithm.
    %%
    %% We insert our 64-bit Nonce right after the hash of the block header We
    %% base64-encode both the hash of the block header and the nonce and pass
    %% the resulting command-line friendly string with the -h option to Cuckoo.
    %%
    %% The SHA256 hash is 32 bytes (44 chars base64-encoded), the nonce is 8 bytes
    %% (12 chars base64-encoded). That leaves plenty of room (80 - 56 = 24
    %% bytes) for cuckoo to put its nonce (which will be 0 in our case) in.
    %%
    %% (Base64 encoding: see RFC 3548, Section 3:
    %% https://tools.ietf.org/html/rfc3548#page-4
    %% converts every triplet of bytes to 4 characters: from N bytes to 4*ceil(N/3)
    %% bytes.)
    %%
    %% Like Cuckoo, we use little-endian for the nonce here.
    NonceStr = base64:encode_to_string(<<Nonce:64/little-unsigned-integer>>),
    HashStr  = base64:encode_to_string(Hash),
    %% Cuckoo will automatically fill bytes not given with -h option to 0, thus
    %% we need only return the two base64 encoded strings concatenated.
    %% 44 + 12 = 56 bytes
    HashStr ++ NonceStr.

%%------------------------------------------------------------------------------
%% @doc
%%   Receive and process notifications about the fate of the process and its
%%   output. The receieved stdout tends to be in large chunks, we keep a buffer
%%   for the last line fragment w/o NL.
%% @end
%%------------------------------------------------------------------------------
wait_for_result(#state{os_pid = OsPid, port = Port, buffer = Buffer} = State) ->
    receive
        {Port, {data, Msg}} ->
            Str = binary_to_list(Msg),
            {Lines, NewBuffer} = handle_fragmented_lines(Str, Buffer),
            (State#state.parser)(Lines, State#state{buffer = NewBuffer});
        {Port, {exit_status, 0}} ->
            wait_for_result(State);
        {'EXIT',_From, shutdown} ->
            %% Someone is telling us to stop
            stop_execution(OsPid),
            exit(shutdown);
        {'EXIT', Port, normal} ->
            %% Process ended but no value found
            {error, no_value};
        _Other ->
            wait_for_result(State)
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   Prepend the first new incoming line with the last line fragment stored
%%   in Buffer and replace Buffer with the possible new line fragment at the
%%   end of Str.
%% @end
%%------------------------------------------------------------------------------
handle_fragmented_lines(Str, Buffer) ->
    Lines = string:tokens(Str, "\n"),

    %% Add previous truncated line if present to first line
    Lines2 =
        case Buffer of
            [] ->
                Lines;
            _ ->
                [Line1 | More] = Lines,
                [Buffer ++ Line1 | More]
        end,

    %% Keep last fraction (w/o NL) in buffer
    case lists:last(Str) of
        $\n ->
            {Lines2, ""};
        _ ->
            {L3, [Bf]} = lists:split(length(Lines) - 1, Lines2),
            {L3, Bf}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   Parse miner output
%% @end
%%------------------------------------------------------------------------------
parse_generation_result([], State) ->
    wait_for_result(State);
parse_generation_result(["Solution" ++ NonceValuesStr | Rest],
                        #state{os_pid = OsPid, edge_bits = EdgeBits, target = Target} = State) ->
    [NonceStr | SolStrs] =  string:tokens(NonceValuesStr, " "),
    Soln = [list_to_integer(string:trim(V, both, [$\r]), 16) || V <- SolStrs],
    case {length(Soln), test_target(Soln, Target, EdgeBits)} of
        {?SOLUTION_SIZE, true} ->
            stop_execution(OsPid),
            case parse_nonce_str(NonceStr) of
                {ok, Nonce} ->
                    ?debug("Solution found: ~p", [Soln]),
                    {ok, Nonce, Soln};
                Err = {error, _} ->
                    ?debug("Bad nonce: ~p", [Err]),
                    Err
            end;
        {N, _} when N /= ?SOLUTION_SIZE ->
            ?debug("Solution has wrong length (~p) should be ~p", [N, ?SOLUTION_SIZE]),
            %% No nonce in solution, old miner exec?
            stop_execution(OsPid),
            {error, bad_miner};
        {_, false} ->
            %% failed to meet target: go on, we may find another solution
            ?debug("Failed to meet target (~p)", [Target]),
            parse_generation_result(Rest, State)
    end;
parse_generation_result([_Msg | T], State) ->
    parse_generation_result(T, State).

parse_nonce_str(S) ->
    try {ok, list_to_integer(string:trim(S, both, "()"), 16)}
    catch _:_ -> {error, bad_nonce} end.


%%------------------------------------------------------------------------------
%% @doc
%%   Stop the OS process
%% @end
%%------------------------------------------------------------------------------
stop_execution(OsPid) ->
    exec_kill(OsPid),
    ?debug("Mining OS process ~p stopped", [OsPid]),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%%   The Cuckoo solution is a list of uint32 integers unless the graph size is
%%   greater than 33 (when it needs u64 to store). Hash result for difficulty
%%   control accordingly.
%% @end
%% Refs:
%% * https://github.com/tromp/cuckoo/blob/488c03f5dbbfdac6d2d3a7e1d0746c9a7dafc48f/src/Makefile#L214-L215
%% * https://github.com/tromp/cuckoo/blob/488c03f5dbbfdac6d2d3a7e1d0746c9a7dafc48f/src/cuckoo.h#L26-L30
%%------------------------------------------------------------------------------
-spec get_node_size(pos_integer()) -> non_neg_integer().
get_node_size(EdgeBits) when is_integer(EdgeBits), EdgeBits > 31 -> 8;
get_node_size(EdgeBits) when is_integer(EdgeBits), EdgeBits > 0 -> 4.

exec_run(Cmd, Dir, Args) ->
    PortSettings = [binary,
                    exit_status,
                    hide,
                    in,
                    overlapped_io,
                    stderr_to_stdout,
                    {args, Args},
                    {cd, Dir}
                   ],
    PortName = {spawn_executable, os:find_executable(Cmd, Dir)},
    try
        Port = erlang:open_port(PortName, PortSettings),
        case erlang:port_info(Port, os_pid) of
            {os_pid, OsPid} ->
                ?debug("External mining process started with OS pid ~p", [OsPid]),
                {ok, Port, OsPid};
            undefined ->
                ?warning("External mining process finished before ~p could acquire the OS pid", [?MODULE]),
                {ok, Port, undefined}
        end
    catch
        C:E ->
            {error, {port_error, {C, E}}}
    end.

exec_kill(undefined) ->
    ok;
exec_kill(OsPid) ->
    case is_unix() of
        true ->
            os:cmd(io_lib:format("kill -9 ~p", [OsPid])),
            ok;
        false ->
            os:cmd(io_lib:format("taskkill /PID ~p /T /F", [OsPid])),
            ok
    end.

is_unix() ->
    case erlang:system_info(system_architecture) of
        "win32" ->
            false;
        _ ->
            true
    end.

%%------------------------------------------------------------------------------
%% White paper, section 9: rather than adjusting the nodes/edges ratio, a
%% hash-based target is suggested: the sha256 hash of the cycle nonces
%% is restricted to be under the target value (0 < target < 2^256).
%%------------------------------------------------------------------------------
test_target(Soln, Target, EdgeBits) ->
    test_target1(Soln, Target, get_node_size(EdgeBits)).

test_target1(Soln, Target, NodeSize) ->
    Bin = solution_to_binary(lists:sort(Soln), NodeSize * 8),
    Hash = aeminer_blake2b_256:hash(Bin),
    aeminer_pow:test_target(Hash, Target).

%%------------------------------------------------------------------------------
%% Convert solution (a list of 42 numbers) to a binary
%% in a languauge-independent way
%%------------------------------------------------------------------------------
solution_to_binary(Soln, Bits) ->
    solution_to_binary(Soln, Bits, <<>>).

solution_to_binary([], _Bits, Acc) ->
    Acc;
solution_to_binary([H | T], Bits, Acc) ->
    solution_to_binary(T, Bits, <<Acc/binary, H:Bits>>).

