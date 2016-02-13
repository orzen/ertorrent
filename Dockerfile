FROM debian:stretch
RUN apt-get update && apt-get install --no-install-recommends -y \
	erlang-base \
	erlang-eunit \
	rebar

ENTRYPOINT ["/bin/sh", "-c"]
CMD ["rebar eunit"]
WORKDIR /usr/src/ertorrent

ADD src/ /usr/src/ertorrent
