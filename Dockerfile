FROM thoughtbot/heroku-haskell-stack

# Add a /app/GIT_HEAD_REF file with the git commit SHA that is currently
# deployed.
RUN mkdir -p .git
COPY .git/HEAD .git/refs .git/
RUN cat ".git/$(cut -d' ' -f2 .git/HEAD)" > /app/GIT_HEAD_REF
RUN rm -rf .git

CMD ./croniker
