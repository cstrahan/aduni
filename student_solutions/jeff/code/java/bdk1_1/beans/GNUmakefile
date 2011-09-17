
SUBDIRS= demo beanbox

MAKEFLAGS += --no-print-directory

all: $(SUBDIRS)


$(SUBDIRS) : jars lib FORCE
	cd $@ ; $(MAKE) all
	@echo ""

run:
	cd beanbox ; $(MAKE) run

jars:
	mkdir -p $@

lib:
	mkdir -p $@

clean:
	@for dir in $(SUBDIRS) ; do \
	    echo "" ; \
	    echo "cd $$dir; make clean" ; \
	    cd $$dir ; $(MAKE) clean ; cd .. ; \
	done
	@echo ""
	find . -name '*.class' -exec /bin/rm {} \;
	/bin/rm -f -r classes jars lib
	@echo ""

FORCE:
