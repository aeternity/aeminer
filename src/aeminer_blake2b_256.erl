-module(aeminer_blake2b_256).

-export([hash/1]).

-export_type([hashable/0,
              hash/0
             ]).

-type hashable() :: binary().

-type hash()     :: binary().

-spec hash(hashable()) -> hash().
hash(Bin) ->
    {ok, Hash} = enacl:generichash(32, Bin),
    Hash.

