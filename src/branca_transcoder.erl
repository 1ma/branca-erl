-module(branca_transcoder).

%% API exports
-export([convert/3]).

%%====================================================================
%% API functions
%%====================================================================
convert(Data, SrcBase, DstBase) ->
  convert(<<>>, Data, SrcBase, DstBase).

%%====================================================================
%% Internal functions
%%====================================================================
convert(Result, <<>>, _, _) -> Result;

convert(Result, Data, SrcBase, DstBase) ->
  {Quotient, Rem} = convert(Data, <<>>, 0, SrcBase, DstBase),
  convert(<<Rem, Result/binary>>, Quotient, SrcBase, DstBase).


convert(<<H, T/binary>>, Quotient, PrevRem, SrcBase, DstBase) ->
  Accumulator = H + (PrevRem * SrcBase),
  Digit = Accumulator div DstBase,
  Rem = Accumulator rem DstBase,
  if
    0 /= byte_size(Quotient) orelse 0 /= Digit ->
      convert(T, <<Quotient/binary, Digit>>, Rem, SrcBase, DstBase);
    true ->
      convert(T, Quotient, Rem, SrcBase, DstBase)
  end;

convert(<<>>, Quotient, PrevRem, _, _) -> {Quotient, PrevRem}.
