# Croniker

It's Buffer for Twitter names.

## Setup

Make sure you have [Stack] installed. On OS X, you can do `brew install
haskell-stack`.

Then run `./bin/setup`.

[Stack]: http://docs.haskellstack.org/en/stable/README.html

## Running the server

    heroku local -f Procfile.local

This will start Yesod and tell Sass to recompile the CSS whenever a file in
`sass/` changes.

Now visit http://localhost:3000.

## Running a task

To update all of the unsent profiles scheduled for today:

    stack exec todays-profiles

(This can be run every few hours via the Heroku Scheduler add-on with just
`./todays-profiles`.)

To update all of the profiles, regardless of when they're scheduled or whether
they've been sent:

    stack exec all-profiles

`all-profiles` is meant to be used for local testing.

## Running tests

    stack test

## Running ghci

    ./bin/ghci

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

Circle will auto-deploy to staging and production after a successful build on
the master branch.

To deploy manually:

  ./bin/deploy [ staging | production ]
