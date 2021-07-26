FROM haskell:8.10.4-buster as deps

RUN set -ex; \
    apt-get update  -yq; \
    apt-get install -y --no-install-recommends libpq-dev; \
    rm -rf /var/lib/apt/lists/*


FROM deps as builder

WORKDIR /lambda-board

COPY package.yaml .

RUN awk '/dependencies/{a=1;next} a && /^$/ {exit} a {print $2}' package.yaml | \
        xargs stack build --system-ghc

COPY . .
RUN stack install --system-ghc


FROM deps

RUN set -ex; \
    apt-get update  -yq; \
    apt-get install -y --no-install-recommends libpq-dev; \
    rm -rf /var/lib/apt/lists/*

RUN mkdir /static
COPY static /static
WORKDIR /static
RUN base64 -Dd s.b | tar xf -
WORKDIR /
COPY --from=builder /root/.local/bin/lambda-board-exe /lambda

ENTRYPOINT ["/lambda"]
CMD ["-d", "./test.db", "-p", "3000", "-s", "/static"]

