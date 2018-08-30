-module(branca).

%% API exports
-export([encode/2, decode/2]).

%%====================================================================
%% API functions
%%====================================================================
encode(PlainText, Secret) ->
  Nonce = libsodium_randombytes:buf(12),
  Header = <<16#BA, (erlang:system_time(seconds)):32, Nonce/binary>>,
  Payload = libsodium_crypto_aead_chacha20poly1305:ietf_encrypt(PlainText, Header, Nonce, Secret),
  branca_base62:encode(<<Header/binary, Payload/binary>>).

decode(CipherText, Secret) ->
  <<Header:17/bytes, Payload/binary>> = branca_base62:decode(CipherText),
  <<16#BA, _:4/bytes, Nonce:12/bytes>> = Header,
  libsodium_crypto_aead_chacha20poly1305:ietf_decrypt(Payload, Header, Nonce, Secret).

%%====================================================================
%% Internal functions
%%====================================================================
