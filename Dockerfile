FROM openjdk:jdk-alpine

ENV SCALA_VERSION 2.12.7

WORKDIR /root
ADD target/scala-2.12/borsuk.jar /root/borsuk.jar
ADD etc/entrypoint.sh /root/entrypoint.sh
ADD etc/gelf.xml /root/gelf.xml
ADD src/main/resources/application.conf /root/application.conf
ENTRYPOINT ["/bin/sh","/root/entrypoint.sh"]

