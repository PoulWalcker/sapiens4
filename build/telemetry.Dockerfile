FROM haskell:9.10.3-slim-bookworm@sha256:683f74f48dcc5a40081628d12ebe8f3bb379bf3fefa46d74e785b5fbb9f21b32 AS build
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates libgmp-dev zlib1g-dev liblz4-dev pkg-config && rm -rf /var/lib/apt/lists/*
ENV CABAL_DIR=/cache
WORKDIR /app/remote/telemetry
COPY remote/telemetry/cabal.project remote/telemetry/telemetry.cabal ./
COPY remote/telemetry/Main.hs remote/telemetry/Collector.hs ./
COPY corpora/telemetry/TelemetryProtocol.hs /app/corpora/telemetry/
RUN cabal user-config init && sed -i 's|url: http://hackage.haskell.org/|url: https://hackage-content.haskell.org/|' /cache/config && cabal update && cabal build exe:sapiens-telemetry -j1 && cp "$(cabal list-bin exe:sapiens-telemetry)" /usr/local/bin/sapiens-telemetry

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends libgmp10 zlib1g liblz4-1 && rm -rf /var/lib/apt/lists/* && mkdir /data && chown 10001:10001 /data
COPY --from=build /usr/local/bin/sapiens-telemetry /usr/local/bin/sapiens-telemetry
WORKDIR /data
USER 10001:10001
ENTRYPOINT ["/usr/local/bin/sapiens-telemetry"]
CMD ["/etc/telemetry/config.json"]
