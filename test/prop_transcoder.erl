-module(prop_transcoder).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_symmetric_transcoding() ->
    ?FORALL(Data, binary(),
        begin
            Data =:= branca_transcoder:transcode(branca_transcoder:transcode(Data, 256, 62), 62, 256)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
