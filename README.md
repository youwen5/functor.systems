# functor.systems

the website. see [functor.systems](https://functor.systems).

## hacking

by default, `nix build` will compile a WASM-targeting binary. However, the WASM
DOM abstraction (JSaddle) is unusable on Firefox, so efforts are being made to
build with GHCjs instead. This has proved difficult for the Nix package but is
possible in a devshell.

Simply run `nix develop`, then

```sh
cabal update && cabal build --allow-newer
```

Then
```sh
http-server $(cabal list-bin website --allow-newer).jsexe
```
