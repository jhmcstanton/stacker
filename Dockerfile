FROM docker.io/haskell:9.8.1-slim-buster

# Building dependencies to cache them
WORKDIR /opt/
ADD stacker.cabal .
# RUN cabal update && cabal install --only-dependencies
RUN cabal update

# Build stacker now
ADD . .
RUN cabal install
ENV HOST="0.0.0.0"

ENTRYPOINT ["cabal", "run", "stacker"]
