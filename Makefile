IMAGE_NAME := copy-node-labels
DOCKER_REGISTRY := docker.k3s.differentpla.net

GIT_VSN := $(shell scripts/git-vsn)
RELEASE_VSN ?= $(GIT_VSN)

BRANCH_NAME ?= $(shell git branch --show-current)
export RELEASE_VSN BRANCH_NAME

IMAGE_TAG := $(subst +,_,$(RELEASE_VSN))

all: assert-git-vsn build-image push-image

build-image:
	podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) -t $(IMAGE_NAME)

push-image: assert-git-vsn
	podman push $(IMAGE_NAME) $(DOCKER_REGISTRY)/$(IMAGE_NAME)

assert-git-vsn:
	@ ./scripts/git-vsn >/dev/null
