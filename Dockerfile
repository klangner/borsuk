FROM openjdk:jdk-alpine

ENV SCALA_VERSION 2.12.3
ENV KAFKA_BROKER  localhost:9092
ENV STATSD_HOST localhost
ENV DATA_PATH /tmp

WORKDIR /root
ADD target/scala-2.12/borsuk.jar /root/borsuk.jar
ADD etc/entrypoint.sh /root/entrypoint.sh
ENTRYPOINT ["/bin/sh","/root/entrypoint.sh"]

