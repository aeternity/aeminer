-module(aeminer_blake2b_256).

-export([hash/1]).

-export_type([hashable/0,
              hash/0
             ]).

-define(HASH_BYTES_SIZE, 32).

-type hashable() :: binary().

-type hash()     :: <<_:(?HASH_BYTES_SIZE * 8)>>.

-spec hash(hashable()) -> hash().
hash(Bin) ->
    {ok, Hash} = enacl:generichash(?HASH_BYTES_SIZE, Bin),
    Hash.

