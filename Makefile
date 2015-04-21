PROJECT = divapi

DEPS = cowboy jiffy libsass sassc raven lager
dep_libsass = git https://github.com/sass/libsass.git master
dep_sassc = git https://github.com/sass/sassc.git master
dep_lager = git https://github.com/basho/lager.git master
dep_raven = git https://github.com/soundrop/raven-erlang.git master

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master

export SASS_LIBSASS_PATH = $(DEPS_DIR)/libsass

include erlang.mk

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

all:: deps priv/
	cp $(DEPS_DIR)/sassc/bin/sassc priv/

priv/:
	mkdir priv
