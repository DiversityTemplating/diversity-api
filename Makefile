PROJECT = divapi
DEPS = cowboy
dep_cowboy = git https://github.com/extend/cowboy master
TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master
include erlang.mk
