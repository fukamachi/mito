#!/bin/sh

mysql -uroot -e 'GRANT ALL ON *.* TO nobody@"localhost" IDENTIFIED BY "nobody"'
mysql -uroot -e 'CREATE DATABASE `mito_test`'
psql -c 'create database "mito_test";' -U postgres
psql -c "CREATE USER nobody WITH PASSWORD 'nobody';" -U postgres

echo "Done"
