# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:
- Export `HugeInt`'s `abs` and `neg` and `HugeNum`'s `subHugeNum`, both implemented by @Thimoteus (#25 by @JordanMartinez)

Bugfixes:

Other improvements:
- Fix warnings revealed by v0.14.1 PS release, including the removal of `fromKRep` function (#25 by @JordanMartinez)
- Install missing dependencies used in source code (#25 by @JordanMartinez)

## [v5.0.0](https://github.com/purescript-contrib/purescript-precise/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#21 by @JordanMartinez, #22 by @kl0tl)

New features:

Bugfixes:
- Fixed comparing negative `HugeNum` values and `HugeInt.toInt` returning `Nothing` for negative values (#20 by @vladciobanu)

Other improvements:
- Added tests for `HugeInt` (#19 by @cmdv)
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#17 by @JordanMartinez)

## [v4.0.0](https://github.com/purescript-contrib/purescript-precise/releases/tag/v4.0.0) - 2019-03-03

- Removed `quickcheck` as a hard dependency
- Added `MonadGen`-based generator for `HugeNum`

## [v3.0.1](https://github.com/purescript-contrib/purescript-precise/releases/tag/v3.0.1) - 2018-06-27

- Updated `bower.json` file to correctly list the repository URL as `purescript-contrib/purescript-precise`

## [v3.0.0](https://github.com/purescript-contrib/purescript-precise/releases/tag/v3.0.0) - 2018-06-27

- Updated for PureScript 0.12 (@paulyoung)
- Updated issue, pull request, and contribution templates

## [v2.0.0](https://github.com/purescript-contrib/purescript-precise/releases/tag/v2.0.0) - 2017-04-27

- Updated for PureScript 0.11

## [v1.1.0](https://github.com/purescript-contrib/purescript-precise/releases/tag/v1.1.0) - 2017-02-17

- Added `Generic` instance for `HugeInt`

## [v1.0.1](https://github.com/purescript-contrib/purescript-precise/releases/tag/v1.0.1) - 2017-02-17

- Added `Generic` instances (@chexxor)
- Fixed addition of non-integer numbers (@garyb)
- Fixed multiplication of non-integer numbers (@chexxor)

## [v1.0.0](https://github.com/purescript-contrib/purescript-precise/releases/tag/v1.0.0) - 2016-11-08

- Initial release of of `purescript-hugenums`, compatible with PureScript 0.10
