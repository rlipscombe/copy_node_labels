IMAGE_NAME := copy-node-labels
DOCKER_REGISTRY := docker.k3s.differentpla.net

all: build-image push-image

rebar3.lock:
	rebar3 lock

build-image: rebar3.lock
	podman build -f Dockerfile -t $(IMAGE_NAME)

push-image:
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME)
