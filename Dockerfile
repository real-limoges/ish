FROM haskell:9.12 AS build

WORKDIR /app

COPY cabal.project ish.cabal ./
RUN cabal update && cabal build --only-dependencies

COPY . .
RUN cabal build exe:ish \
 && cp "$(cabal list-bin ish | tail -1)" /app/ish-server

FROM debian:bookworm-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libsqlite3-0 libgmp10 libffi8 libnuma1 python3 \
 && rm -rf /var/lib/apt/lists/*

COPY data/ish_data.csv data/load.py /data/
RUN cd /data && python3 load.py && rm -f load.py ish_data.csv

COPY --from=build /app/ish-server /usr/local/bin/ish

ENV ISH_PORT=7333
ENV ISH_DB_PATH=/data/ish.db
EXPOSE 7333

ENTRYPOINT ["ish"]