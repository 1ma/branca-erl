-module(branca_base62).
-import(branca_transcoder, [transcode/3]).

%% API exports
-export([encode/1, decode/1]).

-define(BASE62_ALPHABET, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").

%%====================================================================
%% API functions
%%====================================================================
encode(Data) when is_binary(Data) ->
  erlang:list_to_binary(lists:map(
    fun(Elem) -> lists:nth(Elem + 1, ?BASE62_ALPHABET) end,
    erlang:binary_to_list(transcode(Data, 256, 62))
  )).

decode(Data) when is_binary(Data) ->
  transcode(
    erlang:list_to_binary(lists:map(
      fun(Elem) -> index_of(Elem, ?BASE62_ALPHABET) end,
      erlang:binary_to_list(Data)
    )),
    62,
    256
  ).

%%====================================================================
%% Internal functions
%%====================================================================
index_of(Item, List) -> index_of(Item, List, 0).

index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).
