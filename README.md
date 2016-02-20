# Croniker

It's Buffer for Twitter names.

## Setup

Make sure you have [Stack] installed. On OS X, you can do `brew install
haskell-stack`.

Then run `./bin/setup`.

[Stack]: http://docs.haskellstack.org/en/stable/README.html

## Running the server

    ./bin/run

Then visit http://localhost:3000.

## Adding a new executable

If you add a new `executable` in `croniker.cabal`, run `stack build` so that you
can run it with `stack exec new-executable`.

## Sass

* In a separate terminal, run `sass --watch sass:static/css`
* Edit files under `sass/`
* Commit both the Sass files and the generated CSS files

## Static Files

If you add a new file in `static/`, run `touch Settings/StaticFiles.hs` to have
Yesod pick up the routes for the new files.

## Deploying

To deploy to staging:

    ./bin/deploy staging

To deploy to production:

    ./bin/deploy production
