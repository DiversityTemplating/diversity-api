PROJECT = divapi
DEPS = cowboy jiffy
TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master
include erlang.mk
