FROM haskell:9.12 AS build

WORKDIR /app

COPY cabal.project ish.cabal ./
RUN cabal update && cabal build --only-dependencies

COPY . .
RUN cabal build exe:ish \
 && cp "$(cabal list-bin ish | tail -1)" /app/ish-server

FROM debian:bookworm-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends libsqlite3-0 libgmp10 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /app/ish-server /usr/local/bin/ish

ENV ISH_PORT=7333
ENV ISH_DB_PATH=/data/ish.db
EXPOSE 7333

VOLUME ["/data"]

ENTRYPOINT ["ish"]