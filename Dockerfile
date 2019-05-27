FROM haskell:8.6.5

# Install tools
RUN stack update
RUN stack setup
RUN stack install hindent
RUN stack install stylish-haskell

WORKDIR /app

# cache stack in a layer for faster builds
COPY ./stack.yaml /app/stack.yaml
RUN stack setup

# cache dependencies in a layer for faster builds
COPY ./package.yaml /app/package.yaml
RUN stack build --dependencies-only

# build and install binary
COPY . /app
RUN stack build && stack install

CMD ["kfcron"]
