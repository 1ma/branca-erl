-module(branca_tests).
-include_lib("eunit/include/eunit.hrl").

happy_path_test() ->
  Secret = libsodium_randombytes:buf(32),
  Message = <<"abcd">>,
  ?assertEqual({ok, Message}, branca:decode(branca:encode(Message, Secret), Secret)).

bad_encoding_test() ->
  Secret = libsodium_randombytes:buf(32),
  NonBase62CipherText = <<"abcd%e#f">>,
  ?assertEqual({error, bad_encoding}, branca:decode(NonBase62CipherText, Secret)).

invalid_magic_byte_test() ->
  Secret = libsodium_randombytes:buf(32),
  BadCipherText = branca_base62:encode(<<16#BB, 255, 255, 255, 255, 0, 1, 2, 3>>),
  ?assertEqual({error, invalid_token}, branca:decode(BadCipherText, Secret)).

partial_timestamp_test() ->
  Secret = libsodium_randombytes:buf(32),
  BadCipherText = branca_base62:encode(<<16#BA, 255, 255>>),
  ?assertEqual({error, invalid_token}, branca:decode(BadCipherText, Secret)).

empty_payload_test() ->
  Secret = libsodium_randombytes:buf(32),
  BadCipherText = branca_base62:encode(<<16#BA, 255, 255, 255, 255>>),
  ?assertEqual({error, invalid_token}, branca:decode(BadCipherText, Secret)).

tampetered_token_test() ->
  Secret = <<146,182,47,180,112,47,49,226,12,213,203,121,70,7,165,70,102,152,113,201,33,41,56,87,152,180,187,76,249,250,191,136>>,
  OriginalCipherText = <<"DqNWDzgpNY77mVnn8FzAVCnoaX7v3ROUpx6wTtqSKNJquKjHJm">>,
  TamperedCipherText = <<"DqNWDzgpNY77mVnn8FzAVCnoaX7v3ROUpx6wTtqSKNJquKjHJn">>,
  ?assertEqual({ok, <<"abcd">>}, branca:decode(OriginalCipherText, Secret)),
  ?assertEqual({error, invalid_sig}, branca:decode(TamperedCipherText, Secret)).
