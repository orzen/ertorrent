FROM debian:jessie
RUN apt-get update
RUN apt-get install --no-install-recommends -y git make vim

RUN apt-get install --no-install-recommends -y \
	erlang-base \
	erlang-eunit \
	erlang-inets \
	erlang-os-mon \
	erlang-ssl
RUN mkdir -p /home/ertorrent
RUN groupadd ertorrent && useradd -g ertorrent ertorrent
RUN chown -R ertorrent:ertorrent /home/ertorrent
USER ertorrent
RUN git clone git://github.com/orzen/ertorrent.git /home/ertorrent/src
WORKDIR /home/ertorrent/src
CMD /bin/bash
