-module(branca).
-import(branca_base62, [encode/1, decode/1]).
-import(libsodium_randombytes, [buf/1]).
-import(libsodium_crypto_aead_chacha20poly1305, [ietf_encrypt/4, ietf_decrypt/4]).

%% API exports
-export([encode/2, decode/2]).

%%====================================================================
%% API functions
%%====================================================================
encode(PlainText, Secret)
  when
    is_binary(PlainText),
    is_binary(Secret),
    32 =:= byte_size(Secret)

  ->

  Nonce = buf(12),
  Header = <<16#BA, (erlang:system_time(seconds)):32, Nonce/binary>>,
  Payload = ietf_encrypt(PlainText, Header, Nonce, Secret),
  encode(<<Header/binary, Payload/binary>>).

decode(CipherText, Secret)
  when
    is_binary(CipherText),
    is_binary(Secret)

  ->

  <<Header:17/bytes, Payload/binary>> = decode(CipherText),
  <<16#BA, _:4/bytes, Nonce:12/bytes>> = Header,
  Result = ietf_decrypt(Payload, Header, Nonce, Secret),
  if
    -1 =:= Result -> throw(invalid_token);
    true -> Result
  end.

%%====================================================================
%% Internal functions
%%====================================================================
