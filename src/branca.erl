-module(branca).
-import(branca_base62, [encode/1, decode/1]).
-import(libsodium_randombytes, [buf/1]).
-import(libsodium_crypto_aead_chacha20poly1305, [ietf_encrypt/4, ietf_decrypt/4]).

%% API exports
-export([encode/3, encode/2, decode/2]).

-define(MAX_TIMESTAMP, 16#FFFFFFFF).
-define(NONCE_LENGTH, 12).
-define(SECRET_LENGTH, 32).
-define(VERSION_BYTE, 16#BA).

%%====================================================================
%% API functions
%%====================================================================
encode(PlainText, Secret) ->
  encode(PlainText, Secret, ?MAX_TIMESTAMP).

encode(PlainText, Secret, ExpirationTimestamp)
  when
    is_binary(PlainText),
    is_binary(Secret),
    ?SECRET_LENGTH =:= byte_size(Secret),
    is_integer(ExpirationTimestamp),
    0 =< ExpirationTimestamp andalso ExpirationTimestamp =< ?MAX_TIMESTAMP

  ->

  Nonce = buf(?NONCE_LENGTH),
  Header = <<?VERSION_BYTE:8, ExpirationTimestamp:32, Nonce/binary>>,
  Payload = ietf_encrypt(PlainText, Header, Nonce, Secret),
  encode(<<Header/binary, Payload/binary>>).

decode(CipherText, Secret)
  when
    is_binary(CipherText),
    is_binary(Secret)

  ->

  try
    <<Header:17/bytes, Payload/binary>> = decode(CipherText),
    <<?VERSION_BYTE:8, _:4/bytes, Nonce:12/bytes>> = Header,
    Result = ietf_decrypt(Payload, Header, Nonce, Secret),
    if
      -1 =:= Result -> {error, invalid_sig};
      true -> {ok, Result}
    end
  catch
    throw:bad_encoding -> {error, bad_encoding};
    error:{badmatch, _} -> {error, invalid_token}
  end.

%%====================================================================
%% Internal functions
%%====================================================================
