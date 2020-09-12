### [0.3.0] - 2020-09-12

  * (Improvement) Replaced the abandoned libsodium bindings with the enacl library.
  * (*BC Break*) Switched to the XChaCha20 variant of the AEAD functions, as per the spec. Encoded ciphertexts are now 16 bytes longer.


### [0.2.0] - 2018-09-08

  * (Feature) Added ability to create tokens with a custom timestamp (`branca:encode/3`).
  * (Feature) Added ability to detect expired tokens (`branca:decode/3`).

### [0.1.1] - 2018-09-03

  * (Bugfix) Renamed the library to `branca_erl` to resolve a name clash in the Hex registry.

### [0.1.0] - 2018-09-03

  * Initial release.

[0.3.0]: https://github.com/1ma/branca-erl/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/1ma/branca-erl/compare/0.1.1...0.2.0
[0.1.1]: https://github.com/1ma/branca-erl/compare/0.1.0...0.1.1
[0.1.0]: https://github.com/1ma/branca-erl/commit/4df5225a1366eb8c92e94620f63915aba6464f04
