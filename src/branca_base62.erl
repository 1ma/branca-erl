-module(branca_base62).
-import(branca_transcoder, [transcode/3]).

%% API exports
-export([encode/1, decode/1]).

%%====================================================================
%% API functions
%%====================================================================
encode(Data) when is_binary(Data) ->
  << <<(if
         Byte < 10 -> Byte + $0;
         Byte < 36 -> Byte + ($A - 10);
         true -> Byte + ($a - 36)
       end)/integer>> || <<Byte>> <= transcode(Data, 256, 62) >>.

decode(Data) when is_binary(Data) ->
  transcode(
    << <<(if
            Byte < 58 -> Byte - $0;
            Byte < 91 -> Byte - ($A - 10);
            true -> Byte - ($a - 36)
          end)/integer>> || <<Byte>> <= Data >>,
    62,
    256
  ).

%%====================================================================
%% Internal functions
%%====================================================================
