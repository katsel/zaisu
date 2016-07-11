PROJECT = zaisu
PROJECT_DESCRIPTION = CouchDB mock replication endpoint
PROJECT_VERSION = 0.0.1

DEPS = cowboy jiffy gun
dep_cowboy_commit = master
dep_jiffy_commit = master

TEST_DEPS = gun
dep_gun_commit = master

include erlang.mk
