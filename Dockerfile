FROM haskell:8.10

WORKDIR /lambda-board

COPY . /lambda-board

RUN stack build

CMD ["bash", "run.sh"]
