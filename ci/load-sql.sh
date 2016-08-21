#!/bin/sh
set -xe

export ORACLE_HOME=/u01/app/oracle/product/11.2.0/xe
export PATH=$ORACLE_HOME/bin:$PATH
export ORACLE_SID=XE

echo "Database init..."
for f in sql/*; do
    case "$f" in
        *.sh)  echo "$0: running ${f}"; . "${f}" ;;
        *.sql) echo "$0: running ${f}"; echo "@${f} ;" | sqlplus -S SYSTEM/oracle ;;
        *)     echo "No volume sql script, ignoring ${f}" ;;
    esac
    echo
done
echo "End init."
