FROM docker.io/erlang:26.1.2-alpine AS build

# We need git for rebar3 get-deps.
RUN apk add --no-cache git

WORKDIR /build

# Fetch deps into a separate layer; should improve caching.
COPY rebar.config rebar.config
COPY rebar.lock rebar.lock

RUN rebar3 get-deps
RUN rebar3 compile --deps_only

# Copy the rest and compile it
COPY . .

# We need RELEASE_VSN for rebar.config.script, etc.
ARG RELEASE_VSN
RUN rebar3 as prod release

####
FROM docker.io/alpine:3.18.4

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Use 'tini' to avoid Erlang running as PID 0.
RUN apk add --no-cache tini

COPY --from=build /build/_build/prod/rel/copy_node_labels /copy_node_labels

RUN addgroup -g 10000 copy_node_labels && adduser -u 10000 -D -G copy_node_labels -h /copy_node_labels copy_node_labels
RUN chown -R copy_node_labels.copy_node_labels /copy_node_labels/releases/${RELEASE_VSN}
USER copy_node_labels
ENV HOME /copy_node_labels

EXPOSE 8443

ENTRYPOINT ["/sbin/tini", "--"]
CMD ["/copy_node_labels/bin/copy_node_labels", "foreground"]
