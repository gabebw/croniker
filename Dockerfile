FROM fpco/stack-build:lts-8.0

# Everything will be put in this directory
RUN mkdir -p /app/croniker
WORKDIR /app/croniker

COPY stack.yaml .
COPY *.cabal .

# Install GHC in its own layer so it gets cached
RUN stack setup --install-ghc

RUN stack install yesod-bin
RUN stack install cabal-install

RUN stack build --dependencies-only

COPY . /app/croniker
RUN stack --local-bin-path=. install

# Clean up
RUN rm -rf /app/croniker/.stack-work

# Run the image as a non-root user for local testing, because Heroku does:
#
# > When deployed to Heroku, we also run your container as a non-root user
# > (although we do not use the USER specified in the Dockerfile).
RUN useradd -m myuser
USER myuser

CMD ./croniker
