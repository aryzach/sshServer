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

# docker build -t sshserverrunimg .
# docker run -p 2023:2023 -d --name sshServerRunCntr sshserverrunimg 
