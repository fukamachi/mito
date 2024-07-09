ARG LISP=sbcl

FROM fukamachi/qlot AS build-env

WORKDIR /app
COPY qlfile /app
COPY qlfile.lock /app

RUN qlot install --no-deps

FROM fukamachi/${LISP} AS base

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libsqlite3-dev \
      default-libmysqlclient-dev \
      default-mysql-client && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN set -x; \
  ros -e '(ql:update-dist "quicklisp" :prompt nil)' && \
  ros install "fukamachi/qlot"

COPY --from=build-env /app/.qlot /app/.qlot

COPY . /app
RUN qlot install

ENTRYPOINT "/app/docker/entrypoint.sh"
