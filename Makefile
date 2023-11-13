IMAGE_NAME := copy-node-labels
DOCKER_REGISTRY := docker.k3s.differentpla.net

RELEASE_VSN ?= $(shell scripts/git-vsn)
BRANCH_NAME ?= $(shell git branch --show-current)
export RELEASE_VSN BRANCH_NAME

IMAGE_TAG := $(subst +,_,$(RELEASE_VSN))

all: build-image push-image

rebar.lock:
	rebar3 lock

build-image: rebar.lock
	podman build -f Dockerfile -t $(IMAGE_NAME)

push-image:
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME)
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):latest
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(IMAGE_TAG)
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME):$(BRANCH_NAME)
