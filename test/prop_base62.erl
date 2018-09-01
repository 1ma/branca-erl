-module(prop_base62).
-import(branca_base62, [encode/1, decode/1]).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_symmetric_encoding() ->
    ?FORALL(
        Data, binary(),
        Data =:= decode(encode(Data))
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
