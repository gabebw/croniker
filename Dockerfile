FROM fpco/stack-build:lts-13.0 AS fpco

# Everything will be put in this directory
RUN mkdir -p /app/croniker
WORKDIR /app/croniker

COPY stack.yaml .
COPY *.cabal .

# Install GHC in its own layer so it gets cached
RUN stack setup --install-ghc

RUN stack install yesod-bin
RUN stack install cabal-install

RUN stack build --dependencies-only --no-test

COPY . /app/croniker
RUN stack --local-bin-path=. install --no-test

FROM ubuntu:16.04

RUN apt-get update && apt-get install -y libpq-dev netbase ca-certificates
RUN mkdir -p /app/croniker
WORKDIR /app/croniker

COPY --from=fpco /app/croniker/config ./config
COPY --from=fpco /app/croniker/static ./static
COPY --from=fpco /app/croniker/templates ./templates
COPY --from=fpco /app/croniker/croniker .
COPY --from=fpco /app/croniker/todays-profiles .
COPY --from=fpco /app/croniker/all-profiles .

# Run the image as a non-root user for local testing, because Heroku does:
#
# > When deployed to Heroku, we also run your container as a non-root user
# > (although we do not use the USER specified in the Dockerfile).
# - https://devcenter.heroku.com/articles/container-registry-and-runtime
RUN useradd -m myuser
USER myuser

CMD ./croniker
