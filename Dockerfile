FROM openjdk:jdk-alpine

ENV SCALA_VERSION 2.12.3
ENV PROJECTS_URL https://storage.googleapis.com/argo-projects/

WORKDIR /root
ADD target/scala-2.12/borsuk.jar /root/borsuk.jar
ADD etc/entrypoint.sh /root/entrypoint.sh
ENTRYPOINT ["/bin/sh","/root/entrypoint.sh"]

