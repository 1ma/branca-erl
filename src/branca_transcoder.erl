-module(branca_transcoder).

%% API exports
-export([transcode/3]).

-define(MIN_BASE, 2).
-define(MAX_BASE, 256).

%%====================================================================
%% API functions
%%====================================================================
transcode(<<0, Data/binary>>, SrcBase, DstBase) ->
  <<0, (transcode(Data, SrcBase, DstBase))/binary>>;

transcode(Data, SrcBase, DstBase)
  when
    is_binary(Data),
    is_integer(SrcBase),
    is_integer(DstBase),
    ?MIN_BASE =< SrcBase,
    ?MIN_BASE =< DstBase,
    ?MAX_BASE >= SrcBase,
    ?MAX_BASE >= DstBase

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
