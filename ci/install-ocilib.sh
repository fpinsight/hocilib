#!/bin/sh
set -ex

cd /tmp
wget https://github.com/vrogier/ocilib/archive/v4.3.2.tar.gz
tar xvf v4.3.2.tar.gz
cd /tmp/ocilib-4.3.2

./configure --with-oracle-import=linkage \
            --with-oracle-charset=ansi \
            --with-oracle-headers-path=/usr/include/oracle/12.1/client64 \
            --with-oracle-lib-path=/usr/lib/oracle/12.1/client64/lib
make
sudo make install
cd
rm -rf /tmp/ocilib-4.3.2
echo "/usr/local/lib" | sudo tee /etc/ld.so.conf.d/usr-local.conf
sudo ldconfig
