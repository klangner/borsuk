#!/bin/sh
java -Dlogback.configurationFile=/root/gelf.xml  -jar /root/borsuk.jar --data-url=$DATA_URL -Xmx3892m

