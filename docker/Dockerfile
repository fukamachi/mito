ARG LISP=sbcl
FROM fukamachi/${LISP} AS base

RUN ros install prove
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      libsqlite3-dev \
      default-libmysqlclient-dev \
      default-mysql-client && \
    rm -rf /var/lib/apt/lists/*

COPY . /app
WORKDIR /app

ENV CL_SOURCE_REGISTRY "/app/"

ENTRYPOINT "/app/docker/entrypoint.sh"
