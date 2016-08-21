#!/bin/sh
set -ex

cd ~
wget https://storage.googleapis.com/oracle.fpinsight.com/instantClient/ocilib-4.2.1.tar.gz
tar xvf ocilib-4.2.1.tar.gz
cd ocilib-4.2.1 \
    && ./configure --with-oracle-import=linkage \
                   --with-oracle-charset=ansi \
                   --with-oracle-headers-path=$HOME/instantclient_12_1/sdk/include \
                   --with-oracle-lib-path=$HOME/instantclient_12_1 \
    && make \
    && sudo make install
