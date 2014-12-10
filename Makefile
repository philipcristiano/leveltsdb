PROJECT=leveltsdb
CT_OPTS = -create_priv_dir auto_per_tc


DEPS = eleveldb
dep_eleveldb = git https://github.com/basho/eleveldb.git 2.0

.PHONY: release clean-release

release: clean-release all projects
	relx -o rel/$(PROJECT)

clean-release:
	rm -rf rel/$(PROJECT)

include erlang.mk
