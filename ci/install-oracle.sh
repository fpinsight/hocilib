#!/bin/sh

set -xe

sudo cp ci/oracle/chkconfig /sbin/chkconfig
sudo chmod 755 /sbin/chkconfig

sudo ln -s /usr/bin/awk /bin/awk
sudo mkdir /var/lock/subsys
sudo touch /var/lock/subsys/listener
sudo ln -sf /proc/mounts /etc/mtab

sudo cp ci/oracle/60-oracle.conf /etc/sysctl.d/60-oracle.conf
sudo service procps start

sudo cp ci/oracle/oracle-shm /etc/init.d/oracle-shm
sudo chmod 755 /etc/init.d/oracle-shm
sudo /etc/init.d/oracle-shm start

wget https://storage.googleapis.com/oracle.fpinsight.com/xe/oracle-xe_11.2.0-2_amd64.deb
sudo dpkg --install oracle-xe_11.2.0-2_amd64.deb

sudo cp ci/oracle/init.ora /u01/app/oracle/product/11.2.0/xe/config/scripts
sudo cp ci/oracle/initXETemp.ora /u01/app/oracle/product/11.2.0/xe/config/scripts

sudo printf 8080\\n1521\\noracle\\noracle\\ny\\n | sudo /etc/init.d/oracle-xe configure

ls -al /etc/bash.bashrc

sudo tee -a /etc/bash.bashrc > /dev/null << EOF
export ORACLE_HOME=/u01/app/oracle/product/11.2.0/xe
export PATH=$ORACLE_HOME/bin:$PATH
export ORACLE_SID=XE
EOF
