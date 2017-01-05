lts=7.12
ghc=8.0.1
# ghc=8.0.1.20161117 # for stack bug fix on macos (no docker)
docker=docker-container
stack_bin_dir=.stack-work/install/x86_64-linux-*/lts-$(lts)/$(ghc)/bin

docker_tag ?= local

all: clean setup build container

clean:
	stack --verbosity silent --no-docker build --dry-run --prefetch && stack clean

setup:
	stack --verbosity silent --no-docker build --dry-run --prefetch && stack --install-ghc setup

build:
	stack --verbosity silent --no-docker build --dry-run --prefetch && stack build

container: build
	cp $(stack_bin_dir)/slack-emoji $(docker)
	cp settings.yml $(docker)
	cd $(docker) && docker build -t dfithian/slack-emoji:$(docker_tag) .

publish: container
	docker push dfithian/slack-emoji:$(docker_tag)
