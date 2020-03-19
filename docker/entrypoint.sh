#!/bin/bash

set -x
set -e

while ! mysql -u "$MYSQL_USER" \
        -h "$MYSQL_HOST" \
        -P "$MYSQL_PORT" \
        -p"$MYSQL_PASS" \
        -e 'CREATE DATABASE IF NOT EXISTS `mito`'; do \
      sleep 1
done

ros install fukamachi/cl-dbi

ros -s mito-test
run-prove mito-test.asd
