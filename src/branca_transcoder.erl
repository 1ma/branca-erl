-module(branca_transcoder).

%% API exports
-export([transcode/3]).

%%====================================================================
%% API functions
%%====================================================================
transcode(Data, SrcBase, DstBase)
  when
    is_binary(Data),
    is_integer(SrcBase) andalso 2 =< SrcBase andalso 256 >= SrcBase,
    is_integer(DstBase) andalso 2 =< DstBase andalso 256 >= DstBase

  -> transcode(<<>>, Data, SrcBase, DstBase).

%%====================================================================
%% Internal functions
%%====================================================================
transcode(Result, <<>>, _, _) -> Result;

transcode(Result, Data, SrcBase, DstBase) ->
  {Quotient, Rem} = transcode(Data, <<>>, 0, SrcBase, DstBase),
  transcode(<<Rem, Result/binary>>, Quotient, SrcBase, DstBase).


transcode(<<>>, Quotient, PrevRem, _, _) -> {Quotient, PrevRem};

transcode(<<H, T/binary>>, Quotient, PrevRem, SrcBase, DstBase) ->
  Accumulator = H + (PrevRem * SrcBase),
  Digit = Accumulator div DstBase,
  Rem = Accumulator rem DstBase,
  if
    0 /= byte_size(Quotient) orelse 0 /= Digit ->
      transcode(T, <<Quotient/binary, Digit>>, Rem, SrcBase, DstBase);
    true ->
      transcode(T, Quotient, Rem, SrcBase, DstBase)
  end.
