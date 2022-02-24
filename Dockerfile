# dockerfile for build and deploy
# note: stack not supported on ARM, so check if my AWS is running on ARM
FROM ubuntu:20.04

USER root
RUN apt-get update 
RUN apt-get install -y curl 

WORKDIR /root/

RUN curl -sSL https://get.haskellstack.org/ | sh

COPY . .

WORKDIR /root/sshServer/

RUN stack build

CMD ["stack", "run", "main"]

# I think this needs to be NAT or something: docker network create --subnet=172.20.0.0/20 sshservernetwork
# docker build -t sshserverrunimg .
# docker run --net sshservernetwork --ip 172.20.0.10 -p 2023:2023 -d --name sshServerRunCntr sshserverrunimg 
