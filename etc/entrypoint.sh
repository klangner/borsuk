#!/bin/sh
java -jar /root/borsuk.jar --kafka=$KAFKA_BROKER --statsd-host=$STATSD_HOST --statsd-host=$DATA_PATH

