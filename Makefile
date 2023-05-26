SUBDIRS :=	backends/pelemay_backend

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

clean:
	@for _dir in ${SUBDIRS}; do \
		(cd $${_dir} && echo "Cleaning in $${_dir}" && mix clean); \
	done
