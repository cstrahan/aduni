
# We have a subordinate makefile for reach target package.
#
# This is to make it clearer how each individual package gets made.

MAKEFLAGS += --no-print-directory

MKFILES= \
	buttons.gmk \
	eventmonitor.gmk \
	jelly.gmk \
	juggler.gmk \
	methodtracer.gmk \
	misc.gmk \
	molecule.gmk \
	quote.gmk \
	select.gmk \
	test.gmk \
	transitional.gmk \
	sort.gmk \
	wrapper.gmk

all:
	@for file in $(MKFILES) ; do \
	    echo "" ; \
	    echo $(MAKE) -f $$file all ; \
	    $(MAKE) -f $$file all || exit 1; \
	done

clean:
	@for file in $(MKFILES) ; do \
	    echo "" ; \
	    echo $(MAKE) -f $$file clean ; \
	    $(MAKE) -f $$file clean ; \
	done
	/bin/rm -f manifest.tmp
