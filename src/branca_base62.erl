-module(branca_base62).
-import(branca_transcoder, [transcode/3]).

%% API exports
-export([encode/1, decode/1]).

%%====================================================================
%% API functions
%%====================================================================
encode(Data) when is_binary(Data) ->
  << <<(map(Num))/integer>> || <<Num>> <= transcode(Data, 256, 62) >>.

decode(Data) when is_binary(Data) ->
  transcode(<< <<(unmap(Char))/integer>> || <<Char>> <= Data >>, 62, 256).

%%====================================================================
%% Internal functions
%%====================================================================
map(Num) when Num < 10
  -> Num + $0;
map(Num) when Num < 36
  -> Num + $A - 10;
map(Num)
  -> Num + $a - 36.

unmap(Char) when $0 =< Char, Char =< $9
  -> Char - $0;
unmap(Char) when $A =< Char, Char =< $Z
  -> Char - $A + 10;
unmap(Char) when $a =< Char, Char =< $z
  -> Char - $a + 36;
unmap(_)
  -> throw(bad_encoding).
