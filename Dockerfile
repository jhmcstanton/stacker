FROM docker.io/haskell:9.2.8-slim-buster

# Building dependencies to cache them
WORKDIR /opt/
RUN cabal update
ADD . .
RUN cabal install
RUN cabal build
ENV HOST="0.0.0.0"

ENTRYPOINT ["cabal", "run", "stacker"]
