SUBDIRS :=	\
	backends/pelemay_backend \
	backends/decorating_backend \
	backends/logging_backend

all:	setup
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix compile); \
	done

setup:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && mix deps.get); \
	done

test:
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
