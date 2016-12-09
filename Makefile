KERL_VERSION = 19.1

ifdef KERL_VERSION
  SHELL := /bin/bash
  KERL_PATH = $(shell kerl list installations | grep ${KERL_VERSION} | perl -pe 's/^[^ ]* //')
  PATH := $(KERL_PATH)/bin:$(PATH)
endif

default: compile

compile:
	rebar3 compile

shell:
	epmd -daemon
	rebar3 shell --name=inventory@127.0.0.1 --setcookie=DEVELOPMENT --config=config/sys.config
