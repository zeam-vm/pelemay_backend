SUBDIRS :=	\
	utilities/backend_decorator \
	utilities/node_activator \
	utilities/spawn_co_elixir \
	utilities/http_downloader \
	backends/pelemay_backend \
	backends/logging_backend \
	benchmarks/onnx_to_axon_bench \
	benchmarks/distributed_computing_bench

all:	setup
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix compile); \
	done

setup:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix deps.get); \
	done

test:	all
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix test); \
	done

format:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix format); \
	done

credo:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix credo); \
	done

dialyzer:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix dialyzer); \
	done

update:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix deps.update --all); \
	done

clean:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && echo "Cleaning in $${_dir}" && mix clean); \
	done
