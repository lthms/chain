# chain

This haskell package started as an experiment to implement a Haskell-flavour
[`error-chain`](https://crates.io/crates/error-chain). Its result is `ResultT`,
a parameterised Monad which implements an extensible, type-safe error-handling.

This package is released on GitHub as an open source software, but it is not
distributed under any particular licence, and therefore is not (yet) a free
software. Hopefully, this will be changed before publishing `chain` to Hackage.

## In a Nutshell

A typical monadic function which lives inside the `ResultT` monad will have a
type signature which looks like that:

```haskell
function :: ('[Err1, Err2] :| err, Monad m)
        => a -> b -> ResultT msg err m c
```

`'[Err1, Err2] :| err` means `function` may raise an error of type `Err1` or
`Err2` while it computes a result of type `c`. The computation is done within
the monad `m`, that is `ResultT` can be part of a monad stack a la
mtl. `ResultT` is **not** an alternative to mtl, as `Eff` can be. It is a more
flexible `EitherT`.

To escape the `ResultT` package means using the `runResultT` function, whose
type signature is:

```haskell
runResultT :: ResultT msg '[] m a -> m a
```

`runResultT` only accepts empty row of errors (`'[]`). This obliges you to
handle your error. The package provides several functions to that end.

## Status

This is still a big “work in progress” project. You can have a look at
[this blogpost](https://soap.coffee/~lthms/posts/ExtensibleTypeSafeErrorHandling.html) for an —already a bit out-dated— introduction to
the `ResultT` monad.

| Service | Status |
| ------- | ------ |
| Travis  | [![Build Status](https://travis-ci.org/lthms/chain.svg?branch=master)](https://travis-ci.org/lethom/chain) |

## References

This package could not have been written without the works of other talented
programmers, including (but not limited to):

- The [freer](https://hackage.haskell.org/package/freer) and
  [extensible-effects](https://hackage.haskell.org/package/extensible-effects)
  packages
- The [open-union](https://hackage.haskell.org/package/open-union) package
- The [error-chain](https://crates.io/crates/error-chain) crate
