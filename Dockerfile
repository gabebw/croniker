FROM thoughtbot/heroku-haskell-stack

# The application files are in /app/user.
# This section adds a /app/GIT_HEAD_REF file with the git commit SHA that is
# currently deployed.
COPY .git .git
RUN cat ".git/$(cut -d' ' -f2 .git/HEAD)" > /app/GIT_HEAD_REF
RUN rm -rf .git

CMD ./croniker
