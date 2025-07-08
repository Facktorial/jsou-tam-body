FROM haskell:9.4.8-slim

WORKDIR /app

RUN apt-get update && apt-get install -y libtinfo-dev

COPY stack.yaml package.yaml ./
RUN stack setup && stack build --only-dependencies

COPY . .

RUN stack exec -- hpack
RUN stack build --copy-bins

CMD ["./.stack-work/install/x86_64-linux/*/8.*/bin/data-api"]
