#!/bin/sh

set -ex

cd ~
wget https://storage.googleapis.com/oracle.fpinsight.com/instantClient/instantclient-basiclite-linux.x64-12.1.0.2.0.zip
wget https://storage.googleapis.com/oracle.fpinsight.com/instantClient/instantclient-sdk-linux.x64-12.1.0.2.0.zip
unzip instantclient-basiclite-linux.x64-12.1.0.2.0.zip
sudo cp instantclient_12_1/lib* /usr/local/lib/
sudo ln -s /usr/local/lib/libclntsh.so.12.1 /usr/local/lib/libclntsh.so
ln -s $HOME/instantclient_12_1/libclntsh.so.12.1 $HOME/instantclient_12_1/libclntsh.so
unzip instantclient-sdk-linux.x64-12.1.0.2.0.zip
