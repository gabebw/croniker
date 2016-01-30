# Croniker

It's Buffer for Twitter names.

## Setup

Make sure you have [Stack] installed. On OS X, you can do `brew install
haskell-stack`.

Then run `./bin/setup`.

[Stack]: http://docs.haskellstack.org/en/stable/README.html

## Running the server

    stack exec -- yesod devel

Then visit http://localhost:3000.

## Adding a new executable

If you add a new `executable` in `croniker.cabal`, run `stack build` so that you
can run it with `stack exec new-executable`.
