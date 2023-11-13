IMAGE_NAME := copy-node-labels
DOCKER_REGISTRY := docker.k3s.differentpla.net

all: build-image push-image

rebar.lock:
	rebar3 lock

build-image: rebar.lock
	podman build -f Dockerfile -t $(IMAGE_NAME)

push-image:
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME)
