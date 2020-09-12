# branca-erl

[![Build Status](https://travis-ci.org/1ma/branca-erl.svg?branch=master)](https://travis-ci.org/1ma/branca-erl)

An Erlang implementation of the [Branca specification] for authenticated and encrypted tokens.

These are symmetrically encrypted, tamper-proof strings of arbitrary contents that can be safely exposed.


## Installation

Add this to your `rebar.config` file to install the library through hex.pm:

```erlang
{deps, [
    {branca_erl, "0.1.1"}
]}.
```


## Basic Usage

```erlang
1> Secret = soda:rand(32). % the spec mandates that secret keys must be exactly 32 bytes long.
<<238,191,60,162,227,35,20,3,135,35,6,69,45,10,213,250,3,
  106,71,133,119,70,131,43,173,147,60,182,122,...>>

2> Message = erlang:term_to_binary({foo, bar, baz, [1,2,3]}).
<<131,104,4,100,0,3,102,111,111,100,0,3,98,97,114,100,0,3,
  98,97,122,107,0,3,1,2,3>>

3> Token = branca:encode(Message, Secret).
<<"9GBoip8wFIboItLRutv335YmhKpa4vRX5qXKFoyABy0f8LOw9hk3Zi4I14H2AL9VKk0i6GRentlKXc9qr">>

4> {ok, Message} = branca:decode(Token, Secret).
{ok,<<131,104,4,100,0,3,102,111,111,100,0,3,98,97,114,
      100,0,3,98,97,122,107,0,3,1,2,3>>}
```


## API

### `branca:encode/2`

Uses `Secret` to turn `PlainText` into a Branca token using the current Unix time as the [timestamp]. Returns the token as an Erlang binary.

### `branca:encode/3`

Same as above, but using a custom timestamp. If used, it must be greater than 0 and less than 2^32 (4 bytes long).

### `branca:decode/2`

Uses `Secret` to turn a Branca token into the original `PlainText`. Returns a two-valued tuple for each possible outcome:

- `{ok, PlainText}` -> successful token decryption.
- `{error, bad_encoding}` -> `CipherText` contains at least one non-base62 character.
- `{error, invalid_token}` -> `CipherText` is base62, but it does not have the [layout] of a valid Branca token.
- `{error, invalid_sig}` -> the `Secret` used to decrypt the token is incorrect, or the token has been tampered.

### `branca:decode/3`

Same as above, but using a `TTL` to determine if the token has to be considered stale. Might return any of the above tuples, plus:

- `{expired, PlainText}` -> the token was successfully decrypted, but it expired (i.e. it was minted more than `TTL` seconds ago).


## Testing

The library includes EUnit and PropEr test suites.

These can be run with the usual rebar3 commands (`rebar3 eunit` and `rebar3 proper`).


## Caveats

- The base62 encoding and decoding is based on an O(n^2) algorithm involving arithmetic division and is _dog slow_.
  On my development laptop encoding 1KB of random data takes about 100ms, and 5KB jumps to 2.5s.
  Future releases might try to mitigate this problem by implementing `branca_transcoder` as a NIF, or replace
  the base62 algorithm altogether (though that would make the tokens incompatible with the Branca spec
  and the ones produced by other implementations).

## TODO

- [X] Travis CI build
- [X] Timestamp expiration
- [ ] Spec annotations for Dialyzer
- [ ] Improve all modules documentation

[Branca specification]: https://github.com/tuupola/branca-spec
[timestamp]: https://github.com/tuupola/branca-spec#timestamp
[layout]: https://github.com/tuupola/branca-spec#token-format
