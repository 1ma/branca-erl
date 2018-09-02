# branca-erl

[![Build Status](https://travis-ci.org/1ma/branca-erl.svg?branch=master)](https://travis-ci.org/1ma/branca-erl)

An Erlang implementation of the Branca specification for authenticated and encrypted tokens.

These are symmetrically encrypted, tamper-proof strings of arbitrary contents that can be safely exposed.


## Installation

Add this to your `rebar.config` file to install the library through hex.pm:

```erlang
{deps, [
    {branca, "0.1.0"}
]}.
```


## Usage

```erlang
1> Secret = <<"supersecretkeyyoushouldnotcommit">>. % has to be exactly 32 bytes long
<<"supersecretkeyyoushouldnotcommit">>

2> Message = erlang:term_to_binary({foo, bar, baz, [1,2,3]}).
<<131,104,4,100,0,3,102,111,111,100,0,3,98,97,114,100,0,3,
  98,97,122,107,0,3,1,2,3>>

3> Token = branca:encode(Message, Secret).
<<"9GBoip8wFIboItLRutv335YmhKpa4vRX5qXKFoyABy0f8LOw9hk3Zi4I14H2AL9VKk0i6GRentlKXc9qr">>

4> {ok, Message} = branca:decode(Token, Secret).
{ok,<<131,104,4,100,0,3,102,111,111,100,0,3,98,97,114,
      100,0,3,98,97,122,107,0,3,1,2,3>>}
```

`branca:encode/2` should never fail. However, `branca:decode/2` has a few different return types:

- `{ok, OriginalData}` -> successful decryption.
- `{error, expired_timestamp, {Timestamp, OriginalData}}` -> Successful decryption, but the token expired (not yet implemented).
- `{error, bad_encoding}` -> contained at least one non-base62 character (these are [0-9A-Za-z]).
- `{error, invalid_token}` -> CipherText is base62, but it does not have the [layout] of a valid Branca token.
- `{error, invalid_sig}` -> the Secret used to decrypt the token is incorrect, or the token has been tampered.


## Testing

The library includes EUnit and PropEr test suites.

These can be run with the usual rebar3 commands (`rebar3 eunit` and `rebar3 proper`).


## Caveats

- Timestamp expiration has not been implemented yet. The next release will introduce a new `branca:encode/3` function
  that will allow the programmer to specify a timestamp at which the token must be considered as "expired".

- Branca mandates the XChaCha20 variant of the ChaCha20-Poly1305 AEAD construction. However, the
  libsodium binding for Erlang is currently built for version 1.0.11 of libsodium, and the XChaCha20 variant
  was not introduced until version 1.0.12. In the meantime branca-erl uses the IETF ChaCha20 variant,
  that differs in the nonce length (12 bytes instead of 24).


## TODO

- [ ] Travis CI build
- [ ] Timestamp expiration
- [ ] Spec annotations for Dialyzer
- [ ] Improve all modules documentation


[layout]: https://github.com/tuupola/branca-spec#token-format