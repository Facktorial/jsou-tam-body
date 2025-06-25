FROM haskell:9.4-slim as builder

WORKDIR /app
COPY stack.yaml package.yaml ./
RUN stack build --only-dependencies --system-ghc

COPY . .
RUN stack build --system-ghc --copy-bins --local-bin-path /app/bin

FROM haskell:9.4-slim
RUN apt-get update && apt-get install -y ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/bin/data-api .

EXPOSE 8000
CMD ["./data-api"]
