#!/bin/sh

set -ex

cd /tmp

wget https://storage.googleapis.com/oracle.fpinsight.com/instantClient/oracle-instantclient12.1-basiclite_12.1.0.2.0-2_amd64.deb
sudo dpkg -i oracle-instantclient12.1-basiclite_12.1.0.2.0-2_amd64.deb
rm -f oracle-instantclient12.1-basiclite_12.1.0.2.0-2_amd64.deb
wget https://storage.googleapis.com/oracle.fpinsight.com/instantClient/oracle-instantclient12.1-devel_12.1.0.2.0-2_amd64.deb
sudo dpkg -i oracle-instantclient12.1-devel_12.1.0.2.0-2_amd64.deb
rm -f oracle-instantclient12.1-devel_12.1.0.2.0-2_amd64.deb
sudo echo "/usr/lib/oracle/12.1/client64/lib" > /etc/ld.so.conf.d/oracle-client.conf
sudo ldconfig
