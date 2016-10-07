# purescript-pouchdb-ffi
A low-level PureScript interface to PouchDB.

## Goals

  - expose the full PouchDB API
  - support the most recent PureScript and PouchDB versions

## Non goals

  - provide a *nice* PureScript API

Instead, this is meant as the most basic FFI layer to build more pleasant, type safe libraries on top of without having to write actual JavaScript code.

# Alternatives / user-facing libraries

Here are some libraries that a PureScript developer might actually want to use:

  - [purescript-pouchdb](https://github.com/brakmic/purescript-pouchdb) uses callbacks and lots of Maybe for options. This does not use purescript-pouchdb-ffi.
  - [purescript-pouchdb-aff](https://github.com/fehrenbach/purescript-pouchdb-aff) is currently vaporware. It is meant to be nice to use with [Aff](https://github.com/slamdata/purescript-aff) and expose less options, but instead more functions, e.g. `rangeQuery` for a call to `query` with `startkey` and `endkey` options set.
