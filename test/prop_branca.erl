-module(prop_branca).
-import(branca, [encode/2, decode/2]).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_symmetric_encryption() ->
  ?FORALL(
    {Message, Secret}, {binary(), binary(32)},
    {ok, Message} =:= decode(encode(Message, Secret), Secret)
  ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
