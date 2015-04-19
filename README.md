# Tombstone
[![Build Status](https://travis-ci.org/ShadowBan/tombstone.png?branch=master)](https://travis-ci.org/ShadowBan/tombstone)

An app for uploading your own personal wanted poster made using
Haskell and Polymer

## Building

```
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests -flib-werror
cabal build
```


## Running

```
./dist/build/tombstone/tombstone
```


## Running tets

```
./dist/build/test/test
```

## REPL

```
cabal repl
```
