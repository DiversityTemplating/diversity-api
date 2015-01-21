PROJECT = divapi

DEPS = cowboy jiffy libsass sassc
dep_libsass = git https://github.com/sass/libsass.git
dep_sassc = git https://github.com/sass/sassc.git

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master

export SASS_LIBSASS_PATH = $(DEPS_DIR)/libsass

all::
	cp $(DEPS_DIR)/sassc/bin/sassc priv/

include erlang.mk
