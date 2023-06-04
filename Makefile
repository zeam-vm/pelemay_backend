SUBDIRS :=	\
	utilities/backend_decorator \
	backends/pelemay_backend \
	backends/logging_backend \
	benchmarks/onnx_to_axon_bench

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

clean:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && echo "Cleaning in $${_dir}" && mix clean); \
	done

publish:
	./publish.exs ${SUBDIRS}