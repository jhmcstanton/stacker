FROM docker.io/haskell:9.8.1-slim-buster

# Building dependencies to cache them
WORKDIR /opt/
ADD stacker.cabal stack.yaml stack.yaml.lock .
RUN stack install --system-ghc --only-dependencies

# Build stacker now
ADD . .
RUN stack install --system-ghc
ENV HOST="0.0.0.0"

ENTRYPOINT ["stack", "exec", "stacker"]
