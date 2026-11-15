FROM haskell:9.10.3-slim-bookworm AS build
RUN apt-get update && apt-get install -y --no-install-recommends \
    liblz4-dev libsnappy-dev pkg-config zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /build
COPY telemetry.cabal cabal.project cabal.project.freeze ./
COPY app/ ./app/
COPY src/ ./src/
RUN cabal update \
    && cabal build exe:telemetry --jobs=1 --disable-tests --disable-benchmarks \
    && install -D "$(cabal list-bin exe:telemetry)" /out/telemetry

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
    libffi8 libgmp10 liblz4-1 libnuma1 libsnappy1v5 libstdc++6 libtinfo6 zlib1g \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /out/telemetry /usr/local/bin/telemetry
USER 10001:10001
ENTRYPOINT ["/usr/local/bin/telemetry"]
CMD ["/run/secrets/signing.key", "/data"]
