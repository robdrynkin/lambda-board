FROM haskell:8.10

WORKDIR /lambda-board

COPY stack.yaml .
COPY stack.yaml.lock .
RUN stack install

COPY . .
RUN stack build --system-ghc

ENTRYPOINT ["stack", "run", "--"]
CMD ["-d", "./test.db", "-p", "3000"]

