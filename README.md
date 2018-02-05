# chain.hs

An experiment to implement the Rust `error-chain` crate in Haskell, with a
dedicate Monad and some facilities to work with several error types.

## Status

This is still a big “work in progress” project. You can have a look at [this
blogpost](http://lthms.xyz/blog/extensible-type-safe-error-handling) for an
introduction to the `ResultT` monad as implemented in this package.

## References

- The [freer](https://hackage.haskell.org/package/freer) and
  [extensible-effects](https://hackage.haskell.org/package/extensible-effects)
  packages
- The [open-union](https://hackage.haskell.org/package/open-union) package
- The [error-chain](https://crates.io/crates/error-chain) crate
