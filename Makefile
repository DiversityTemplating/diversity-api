PROJECT = divapi

DEPS = cowboy jiffy raven lager
dep_lager = git https://github.com/basho/lager.git master
dep_raven = git https://github.com/soundrop/raven-erlang.git master

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)


include erlang.mk

$(DEPS_DIR)/libsass:
	@git clone https://github.com/sass/libsass.git $@
	@make -C $@

$(DEPS_DIR)/sassc: $(DEPS_DIR)/libsass
	@git clone https://github.com/sass/sassc.git $@
	@make SASS_LIBSASS_PATH=$< -C $@

priv/sassc: $(DEPS_DIR)/sassc priv
	@cp $</bin/sassc $@

priv:
	@mkdir -p priv

all:: priv/sassc
