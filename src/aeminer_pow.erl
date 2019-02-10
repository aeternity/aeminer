%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%    Common functions and behaviour for Proof of Work
%%% @end
%%%=============================================================================
-module(aeminer_pow).

-export([integer_to_scientific/1,
         next_nonce/2,
         pick_nonce/0,
         scientific_to_integer/1,
         target_to_difficulty/1,
         test_target/2,
         trim_nonce/2]).

-export_type([nonce/0,
              int_target/0,
              sci_target/0,
              bin_target/0,
              difficulty/0,
              instance/0
             ]).

-include("aeminer.hrl").

-type nonce()      :: 0..?MAX_NONCE.

-type int_target() :: integer().

-type sci_target() :: integer().

-type bin_target() :: <<_:256>>.

%% Difficulty: max threshold (0x00000000FFFF0000000000000000000000000000000000000000000000000000)
%% over the actual one. Always positive.
-type difficulty() :: integer().

-type instance()   :: non_neg_integer().

-type config()     :: aeminer_pow_cuckoo:config().

%% 10^24, approx. 2^80
-define(NONCE_RANGE, 1000000000000000000000000).

%%------------------------------------------------------------------------------
%%                      Target threshold and difficulty
%%
%% The mining rate is controlled by setting a target threshold. The PoW nonce
%% is accepted if a hash value (the hash of the header for SHA-256, the hash of
%% the solution graph for Cuckoo Cycleas, converted to an integer) is below
%% this target threshold.
%%
%% A lower target represents a harder task (requiers the hash to start with a
%% number of zeros).
%%
%% The target thershold relates to another value: the diifculty. This is
%% proportional to the hardness of the PoW task:
%%
%% Difficulty = <Target of difficulty 1> / Target,
%%
%% a floating point value.
%% Bitcoin uses 0x00000000FFFF0000000000000000000000000000000000000000000000000000
%% as Difficulty 1 target (0x1d00ffff in scientific notation, see below). For
%% Cuckoo Cycle we need a lighter filtering of solutions than for SHA-256 as the
%% basic algorithm is much slower than a simple hash generation, so we use the
%% largest possible value:
%% 0xFFFF000000000000000000000000000000000000000000000000000000000000 (0x2100ffff
%% in scientific notation) as difficulty 1.
%%
%% We store the current target threshold in the block header in scientific notation.
%% Difficulty is used to select the winning fork of new blocks: the difficulty of a
%% chain of blocks is the sum of the diffculty of each block.
%%
%% Integers represented in scientific notation:
%%   2^24 * <base-2 exponent + 3> + the first 3 most significant bytes (i.e.,
%%   the significand, see https://en.wikipedia.org/wiki/Significand).
%%   The + 3 corresponds to the length of the
%%   significand (i.e., the int value is 0.<significand> * 8^<exponent>).
%%   https://en.bitcoin.it/wiki/Difficulty#How_is_difficulty_stored_in_blocks.3F)
%%------------------------------------------------------------------------------

%%%=============================================================================
%%% API
%%%=============================================================================

-spec scientific_to_integer(sci_target()) -> int_target().
scientific_to_integer(S) ->
    {Exp, Significand} = break_up_scientific(S),
    E3 = Exp - 3,
    case Exp >= 0 of
        true ->
            Significand bsl (8 * E3);
        false ->
            Significand bsr (-8 * E3)
    end.

-spec integer_to_scientific(int_target()) -> sci_target().
integer_to_scientific(I) ->
    %% Find exponent and significand
    {Exp, Significand} = integer_to_scientific(I, 3),
    case Exp >= 0 of
        true ->
            %% 1st byte: exponent, next 3 bytes: significand
            (Exp bsl 24) + Significand;
        false ->
            %% flip sign bit in significand
            ((-Exp) bsl 24) + 16#800000 + Significand
    end.

%% We want difficulty to be an integer, to still have enough precision using
%% integer division we multiply by K (1 bsl 24).
-spec target_to_difficulty(sci_target()) -> difficulty().
target_to_difficulty(SciTgt) ->
    (?DIFFICULTY_INTEGER_FACTOR * ?HIGHEST_TARGET_INT)
        div scientific_to_integer(SciTgt).

-spec next_nonce(nonce(), config()) -> nonce().
next_nonce(Nonce, Cfg) ->
    Nonce + aeminer_pow_cuckoo:repeats(Cfg).

-spec pick_nonce() -> nonce().
pick_nonce() ->
    rand:uniform(?NONCE_RANGE) band ?MAX_NONCE.

-spec trim_nonce(nonce(), config()) -> nonce().
trim_nonce(Nonce, Cfg) ->
    case Nonce + aeminer_pow_cuckoo:repeats(Cfg) < ?MAX_NONCE of
        true  -> Nonce;
        false -> 0
    end.

%%------------------------------------------------------------------------------
%% Test if binary is under the target threshold
%%------------------------------------------------------------------------------
-spec test_target(bin_target(), sci_target()) -> boolean().
test_target(Bin, Target) ->
    Threshold = scientific_to_integer(Target),
    <<Val:32/big-unsigned-integer-unit:8>> = Bin,
    Val < Threshold.

%% TODO: get target
%Bin = solution_to_binary(lists:sort(Soln), NodeSize * 8, <<>>),
%%Hash = aec_hash:hash(pow, Bin),
%%<<Val:32/big-unsigned-integer-unit:8>> = Hash,
%%Val

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

integer_to_scientific(I, Exp) when I > 16#7fffff ->
    integer_to_scientific(I bsr 8, Exp + 1);
integer_to_scientific(I, Exp) when I < 16#008000, I > 16#000000 ->
    integer_to_scientific(I bsl 8, Exp - 1);
integer_to_scientific(I, Exp) ->
    %% Add the number of bytes in the significand
    {Exp, I}.

%% Return the exponent and significand of a sci_target().
break_up_scientific(S) ->
    SigMask = (1 bsl 24) - 1,
    Exp = ((S bxor SigMask) bsr 24),
    Significand = S band SigMask,
    %% Remove the sign bit, apply to exponent
    case 16#800000 band Significand of
        0 ->
            {Exp, Significand};
        _ ->
            {-Exp, Significand - 16#800000}
    end.

