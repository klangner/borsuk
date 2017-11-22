#!/bin/sh
java -jar /root/borsuk.jar --kafka=$KAFKA_BROKER --statsd-host=$STATSD_HOST --data-path=$DATA_PATH

