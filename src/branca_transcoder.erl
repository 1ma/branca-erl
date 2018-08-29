-module(branca_transcoder).

%% API exports
-export([convert/3]).

%%====================================================================
%% API functions
%%====================================================================
convert(Data, SrcBase, DstBase) ->
  lists:reverse(convert(<<>>, Data, SrcBase, DstBase)).

%%====================================================================
%% Internal functions
%%====================================================================
convert(Result, <<>>, _, _) -> Result;

convert(Result, Data, SrBase, DstBase) ->
  Quotient = <<>>,
  Remainder = 0.


fori(<<H, T/binary>>, Quotient, Rem, SrcBase, DstBase) ->
  Accumulator = H + (Rem * SrcBase),
  Digit = Accumulator div DstBase,
  Rem2 = Accumulator rem DstBase,
  if
    0 /= byte_size(Quotient) orelse 0 /= Digit ->
      fori(T, <<Digit, Quotient/binary>>, Rem2, SrcBase, DstBase);
    true ->
      fori(T, Quotient, Rem2, SrcBase, DstBase)
  end.
