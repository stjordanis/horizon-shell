# horizon-shell

This is a shell for controlling horizon package sets.

## Running

To run from this repository.

```
nix run 'git+https://gitlab.horizon-haskell.net/shells/horizon-shell'
```

The package set will be automatically loaded under the variable `hz`.

```
let f = L.at "lens" L..~ Just (H.callHackage "lens" "5.1")

:t f
f :: (L.IxValue t ~ HaskellPackage, L.At t,
      IsString (L.Index t)) =>
     t -> t

let hz' = f hz

H.writeHorizonFile hz'
```
