#!/bin/sh
java -Dlogback.configurationFile=/root/gelf.xml  -jar /root/borsuk.jar --data-url=$DATA_URL -XX:MaxRAM=3892m -Xmx3892m -Xss3500m -XX:+UseParNewGC -XX:+UseConcMarkSweepGC


