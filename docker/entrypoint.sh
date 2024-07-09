#!/bin/bash

set -x
set -e

qlot exec ros -s mito-test

while ! mysql -u "$MYSQL_USER" \
        -h "$MYSQL_HOST" \
        -P "$MYSQL_PORT" \
        -p"$MYSQL_PASS" \
        -e 'CREATE DATABASE IF NOT EXISTS `mito`'; do \
      sleep 1
done

.qlot/bin/rove mito-test.asd
