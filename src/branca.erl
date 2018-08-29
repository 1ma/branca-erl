-module(branca).

%% API exports
-export([encode/2, decode/2]).

%%====================================================================
%% API functions
%%====================================================================
encode(PlainText, Secret) -> transcode(PlainText, Secret, 123).

decode(CipherText, Secret) -> plaintext.

%%====================================================================
%% Internal functions
%%====================================================================
transcode(Data, SrcBase, DstBase) -> transcoded.
