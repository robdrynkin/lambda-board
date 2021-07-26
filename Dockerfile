FROM haskell:8.10.4-buster as builder

WORKDIR /lambda-board

RUN set -ex; \
    apt-get update  -yq; \
    apt-get install -y --no-install-recommends libpq-dev; \
    rm -rf /var/lib/apt/lists/*

COPY package.yaml .

RUN awk '/dependencies/{a=1;next} a && /^$/ {exit} a {print $2}' package.yaml | \
        xargs stack build --system-ghc

COPY . .
RUN stack install --system-ghc


FROM haskell:8.10.4-buster

RUN set -ex; \
    apt-get update  -yq; \
    apt-get install -y --no-install-recommends libpq-dev; \
    rm -rf /var/lib/apt/lists/*

RUN mkdir /static
COPY static /static
COPY --from=builder /root/.local/bin/lambda-board-exe /lambda

ENTRYPOINT ["/lambda"]
CMD ["-d", "./test.db", "-p", "3000", "-s", "/static"]

