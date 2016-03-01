FROM debian:jessie
EXPOSE 80
EXPOSE 6969
RUN apt-get update
RUN apt-get install --no-install-recommends -y git make build-essential gcc libc6-dev zlib1g-dev libowfat-dev

RUN mkdir -p /home/opentracker
RUN groupadd opentracker && useradd -g opentracker opentracker
RUN chown -R opentracker:opentracker /home/opentracker
USER opentracker
RUN git clone git://erdgeist.org/opentracker /home/opentracker/src
WORKDIR /home/opentracker/src
RUN make

RUN mv /home/opentracker/src/opentracker.debug /home/opentracker/opentracker.debug
RUN mv /home/opentracker/src/opentracker.conf.sample /home/opentracker/opentracker.conf
RUN rm -rf /home/opentracker/src

WORKDIR /home/opentracker
EXPOSE 6969 80
ENTRYPOINT ["./opentracker.debug"]
CMD ["--help"]
