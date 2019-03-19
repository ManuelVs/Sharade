# Sharade - In progress

This is a simple functional nondeterministic language that combines share and
no-share semantics.

This language serves as a frontend of monadic programming in Haskell and the
`explicit-sharing` library of Sebastian Fischer.

## Specifications

This language supports 

- Lazy evaluation
- Pattern matching
- High-order functions
- Non-determinism
- Share semantics

## Installation

This guide assumes that you have cabal and haskell interpreter installed.

First of all, you have to install a specific version of a package that does not
exists on Hackage, <https://github.com/ManuelVs/explicit-sharing>. The original
author, Sebastian Fischer, has not updated the library to the modern Haskell
language (as of 2019). You can install that package this way:

```bash
# Choose one of the following!
git clone git@github.com:ManuelVs/explicit-sharing.git
# git clone https://github.com/ManuelVs/explicit-sharing
cabal update
cabal sdist
cabal install dist/explicit-sharing*.tar.gz
cabal clean # this is optional
```

Now, you are prepared to clone, build and install this project:

```bash
# Choose one of the following!
git clone git@github.com:ManuelVs/Sharade.git
# git clone https://github.com/ManuelVs/Sharade.git
cabal update
cabal sdist
cabal install dist/Sharade*.tar.gz
cabal clean # this is optional
```

Cabal should put on the PATH a program called `Sharade`, which can compile
programs written in Sharade into Haskell. In case you can not execute it, you
maybe can find it on `/home/username/.cabal/bin`.

## Usage

The workflow is very simple:
- Write a program in this language
- Compile it to Haskell by running
  ```bash
  Sharade file
  ```
- Launch a Haskell interpreter and load it! In this earlier versions, the module
  generated does not export friendly-functions to work with. You should use it
  this way:
  ```Haskell
  -- Imagine you have in Sharade the following function:
  mRepeat a = a ? mRepeat (a + 1) ;

  -- In Haskell, you can use the function this way:
  > unsafeResults $ mRepeat <#> (return 1)
  ```

## Examples

The examples can be found on /code_examples folder.

## Issues report

You can report issues by using the GitHub - standard way or by sending me an
email to [manuel.velascosuarez@outlook.com](mailto:manuel.velascosuarez@outlook.com).
