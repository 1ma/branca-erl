-module(prop_transcoder).
-import(branca_transcoder, [transcode/3]).
-include_lib("proper/include/proper.hrl").


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_symmetric_transcoding() ->
  ?FORALL(
    {{Data, SrcBase}, DstBase}, {based_binary(), range(2, 256)},
    Data =:= transcode(transcode(Data, SrcBase, DstBase), DstBase, SrcBase)
  ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
based_binary() ->
  ?LET(
    {Data, Base}, {binary(), range(2, 256)},
    {erlang:list_to_binary([X rem Base || <<X>> <= Data]), Base}
  ).
