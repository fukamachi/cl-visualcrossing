FROM fukamachi/qlot AS build-env

WORKDIR /app
COPY qlfile* ./

RUN set -x; \
  qlot install --no-deps

FROM fukamachi/sbcl

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends \
    libssl-dev \
    ca-certificates \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . .
COPY --from=build-env /app/.qlot /app/.qlot

ENTRYPOINT ["sbcl", "--noinform", "--non-interactive"]
CMD ["--load", ".qlot/setup.lisp", "--eval", "(ql:quickload (list :visualcrossing/tests :rove))", "--eval", "(or (rove:run :visualcrossing/tests) (uiop:quit -1))"]
