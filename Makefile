#!/usr/bin/make -f

package = $(shell grep ^Name: *.spec | awk '{print $$2}')
version = $(shell grep ^Version: *.spec | awk '{print $$2}')

all: doc

doc:
	rm -rf $(shell find -type d -name doc)
	raco setup --no-zo --only "$(notdir ${PWD})"

link:
	raco pkg install --no-setup --link "${PWD}"

unlink:
	raco pkg remove --no-setup "$(notdir ${PWD})"

dist:
	git clone --depth=1 "file://${PWD}/.git" "${package}-${version}"
	rm -rf "${package}-${version}/.git"
	tar -cvpzf "${package}-${version}.tar.gz" "${package}-${version}"
	rm -rf "${package}-${version}"

clean:
	rm -rf $(shell find -type d -name doc)
	rm -rf $(shell find -type d -name compiled)

# EOF
