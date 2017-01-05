FROM ubuntu:16.04
MAINTAINER Dan Fithian
RUN apt-get update -qq -y
RUN apt-get install -y libgmp10 ca-certificates netbase libcurl3 sqlite3 libsqlite3-dev
EXPOSE 3000
COPY slack-emoji settings.yml /
ENTRYPOINT ["/slack-emoji"]
