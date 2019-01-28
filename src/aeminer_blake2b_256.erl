-module(aeminer_blake2b_256).

-export([hash/1]).

hash(Bin) ->
    {ok, Hash} = enacl:generichash(32, Bin),
    Hash.

