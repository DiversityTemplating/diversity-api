PROJECT = ds_api
PROJECT_DESCRIPTION = A CDN for Diversity components
PROJECT_VERSION = 0.5.0
PROJECT_REGISTERED = ds_api_component_mgr

DEPS = cowboy jiffy lager raven
dep_raven      = git https://github.com/soundrop/raven-erlang.git master

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master

LOCAL_DEPS = ssl inets

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

CURRENT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

all:: priv/sassc

distclean::
	@rm -rf deps/libsass
	@rm -rf deps/sassc
	@rm -rf priv/sassc

deps/libsass:
	@git clone https://github.com/sass/libsass.git $@
	@make -C $@

deps/sassc: deps/libsass
	@git clone https://github.com/sass/sassc.git $@
	@make SASS_LIBSASS_PATH=$(CURRENT_DIR)/$< -C $@

priv/sassc: deps/sassc priv
	@cp $</bin/sassc $@

priv:
	@mkdir -p priv

include erlang.mk
