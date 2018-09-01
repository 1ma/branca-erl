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

prop_transcoded_has_target_base() ->
  ?FORALL(
    {{Data, SrcBase}, DstBase}, {based_binary(), range(2, 256)},
    lists:all(
      fun(Byte) -> Byte < DstBase end,
      erlang:binary_to_list(transcode(Data, SrcBase, DstBase))
    )
  ).

prop_transcoding_to_same_base_has_no_effect() ->
  ?FORALL(
    {Data, Base}, based_binary(),
    Data =:= transcode(Data, Base, Base)
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
    {<< <<(X rem Base)/integer>> || <<X>> <= Data >>, Base}
  ).
