FROM ubuntu:16.04

RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y \
    build-essential \
    wget \
    curl \
    unzip \
 && rm -rf /var/lib/apt/lists/*

COPY instantclient-basiclite-linux.x64-12.1.0.2.0.zip /opt/
COPY instantclient-sdk-linux.x64-12.1.0.2.0.zip /opt/
RUN cd /opt/ \
    && unzip instantclient-basiclite-linux.x64-12.1.0.2.0.zip \
    && rm instantclient-basiclite-linux.x64-12.1.0.2.0.zip \
    && unzip instantclient-sdk-linux.x64-12.1.0.2.0.zip \
    && rm instantclient-sdk-linux.x64-12.1.0.2.0.zip \
    && ln -s /opt/instantclient_12_1/libclntsh.so.12.1 /opt/instantclient_12_1/libclntsh.so

ADD ocilib-4.3.2.tar.gz /usr/local/src/

RUN cd /usr/local/src/ocilib-4.3.2 \
    && ./configure --with-oracle-import=linkage \
                   --with-oracle-charset=ansi \
                   --with-oracle-headers-path=/opt/instantclient_12_1/sdk/include \
                   --with-oracle-lib-path=/opt/instantclient_12_1 \
    && make \
    && make install \
    && rm -rf /usr/local/src/ocilib-4.3.2

CMD "/bin/bash"
