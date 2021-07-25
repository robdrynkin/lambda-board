FROM haskell:8.10 as builder

WORKDIR /lambda-board

COPY package.yaml .
RUN awk '/dependencies/{a=1;next} a && /^$/{exit} a{print $2}' package.yaml | \
		xargs stack build --system-ghc

COPY . .
RUN stack install --system-ghc


FROM haskell:8.10

COPY static /
COPY --from=builder /root/.local/bin/lambda-board-exe /lambda

ENTRYPOINT ["/lambda"]
CMD ["-d", "./test.db", "-p", "3000", "-s", "/static"]

