FROM haskell:9.4.8-slim

WORKDIR /app

RUN apt-get update && apt-get install -y inotify-tools libtinfo-dev

COPY stack.yaml package.yaml ./
RUN stack setup && stack build --only-dependencies

RUN stack install hpack
COPY . .

RUN stack exec -- hpack
RUN stack build --copy-bins --fast --ghc-options="-j4 -O0"

CMD ["./.stack-work/install/x86_64-linux/*/8.*/bin/data-api"]
