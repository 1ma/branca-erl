-module(branca).
-import(branca_base62, [encode/1, decode/1]).
-import(enacl, [aead_chacha20poly1305_ietf_decrypt/4, aead_chacha20poly1305_ietf_encrypt/4, randombytes/1]).

%% API exports
-export([encode/2, encode/3, decode/2, decode/3]).

-define(MAX_TIMESTAMP, 16#FFFFFFFF).
-define(NONCE_LENGTH, 12).
-define(SECRET_LENGTH, 32).
-define(VERSION_BYTE, 16#BA).

%%====================================================================
%% API functions
%%====================================================================
encode(PlainText, Secret) ->
  encode(PlainText, Secret, erlang:system_time(seconds)).

encode(PlainText, Secret, Timestamp)
  when
    is_binary(PlainText),
    is_binary(Secret),
    is_integer(Timestamp),
    ?SECRET_LENGTH =:= byte_size(Secret),
    0 =< Timestamp,
    Timestamp =< ?MAX_TIMESTAMP

  ->

  Nonce = randombytes(?NONCE_LENGTH),
  Header = <<?VERSION_BYTE:8, Timestamp:32, Nonce/binary>>,
  Payload = aead_chacha20poly1305_ietf_encrypt(PlainText, Header, Nonce, Secret),
  encode(<<Header/binary, Payload/binary>>).

decode(CipherText, Secret, TTL)
  when
    is_integer(TTL),
    0 =< TTL

  ->

  try
    {ok, PlainText} = decode(CipherText, Secret),
    {Timestamp, _, _, _} = decompose_token(CipherText),
    ElapsedTime = erlang:system_time(seconds) - Timestamp,
    if
      ElapsedTime < TTL -> {ok, PlainText};
      true -> {expired, PlainText}
    end
  catch
      error:{badmatch, Error} -> Error
  end.

decode(CipherText, Secret)
  when
    is_binary(CipherText),
    is_binary(Secret)

  ->

  try
    {_, Nonce, Header, Payload} = decompose_token(CipherText),
    enacl_result_adapter(aead_chacha20poly1305_ietf_decrypt(Payload, Header, Nonce, Secret))
  catch
    throw:bad_encoding -> {error, bad_encoding};
    error:{badmatch, _} -> {error, invalid_token}
  end.

%%====================================================================
%% Internal functions
%%====================================================================
decompose_token(Token) ->
  <<Header:17/bytes, Payload/binary>> = decode(Token),
  <<?VERSION_BYTE:8, Timestamp:32/integer, Nonce:12/bytes>> = Header,
  {Timestamp, Nonce, Header, Payload}.

enacl_result_adapter({error, failed_verification}) -> {error, invalid_sig};
enacl_result_adapter(PlainText) when is_binary(PlainText) -> {ok, PlainText}.
