# Build stage - this will be discarded
FROM haskell:9.6.7-slim-bullseye as builder
WORKDIR /app
RUN apt-get update && apt-get install -y libtinfo-dev

# Copy dependency files
COPY stack.yaml data-api.cabal ./
RUN stack setup && stack build --only-dependencies --jobs=4 --fast

# Copy source and build
COPY . .
RUN stack build --copy-bins --fast --ghc-options="-j4 -O0"

# Runtime stage - ONLY the binary
FROM ubuntu:22.04 as runtime
WORKDIR /app

# Install only runtime dependencies
RUN apt-get update && apt-get install -y \
    libtinfo6 \
    libgmp10 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy ONLY the compiled binary from builder stage
#COPY --from=builder /app/.stack-work/install/x86_64-linux/*/8.*/bin/data-api ./data-api
COPY --from=builder /root/.local/bin/data-api ./data-api

# Make sure it's executable
RUN chmod +x ./data-api

CMD ["./data-api"]
