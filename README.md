# Precise

[![CI](https://github.com/purescript-contrib/purescript-precise/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-precise/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-precise.svg)](https://github.com/purescript-contrib/purescript-precise/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-precise/badge)](https://pursuit.purescript.org/packages/purescript-precise)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](https://github.com/thomashoneyman)

This is a library for working with numbers of arbitrarily finite size.

## Installation

Install `precise` with [Spago](https://github.com/purescript/spago):

```sh
spago install precise
```

## Quick start

JavaScript (and to some extension PureScript) has quite a few drawbacks when it comes to large numbers. For example, PureScript's `Int` primitive [is a member](https://github.com/purescript/purescript-prelude/blob/v0.1.3/src/Prelude.js#L177-L178) of the `Bounded` typeclass, with `top == 2 ^ 31 - 1` and `bottom == - (2 ^ 32)`.

The PureScript `Number` primitive is not `Bounded` in the same way; however, there are problems with manipulating large-enough `Number`s:

```
> import Prelude
> let x = 900000000000000000.0
> :t x
Number

> x + 1.0 == x
true
> x + 1.0
900000000000000000
```

In this library, correctness is prioritized above all else:

```
> import Data.HugeNum
> let x = fromNumber 900000000000000000.0
> let y = fromNumber 1.0
> x + y == x
false

> x + y
HugeNum 900000000000000001.0
```

Addition is implemented using an elementary-school method. Multiplication follows [Karatsuba](https://en.wikipedia.org/wiki/Karatsuba_algorithm).

## Documentation

`precise` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-precise).
2. Written documentation is kept in [the docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-precise/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://discord.com/invite/sMqwYUbvz6) chat.

## Contributing

You can contribute to `precise` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-precise/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
