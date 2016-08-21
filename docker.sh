#!/bin/sh

docker run -d --name oracle --shm-size=2g -p 1521:1521 -v $(pwd)/sql:/etc/entrypoint-initdb.d alexeiled/docker-oracle-xe-11g

# user: system/oracle@localhost:1521/xe
