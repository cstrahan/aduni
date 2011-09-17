-include version.mk

APP=source2html
DEST=/home/jim/bin
PKGFILES=$(APP) Makefile todo *.rb *.rc *.blurb *.mk

TOOLSDIR=web-public/tools
UPLOADDIR=$(TOOLSDIR)/$(APP)
UPLOADHOST=one.net

test: ver ut at			# Run the unit and acceptance tests (default)

ut: FORCE			# Run unit tests
	@echo
	@echo "Unit Tests ----------------------------"
	@runtests.rb 

at: pre-at FORCE		# Run acceptance tests
	@runtests.rb -t acceptance

at1: pre-at FORCE		# Run one Accept Test (use TEST=testname)
	@runtests.rb -t acceptance -r $(TEST)

ver: version.mk FORCE		# Update the Version Info

web: nohtml ver html man FORCE	# Create all the files for a web installation
	./$(APP) --dir html $(APP) *.rb Makefile
	./$(APP) --dir html/accept --init accept.rc \
		acceptance/*.rb \
		acceptance/*.e \
		acceptance/*.pl \
		acceptance/*.el \
		acceptance/*.sh \
		acceptance/*.py \
		acceptance/*.java \
		acceptance/*_implicit

package: ver			# Create a distribution package
	rm -rf pkg
	mkdir -p pkg/$(APP)-$(REV)
	mkdir -p pkg/$(APP)-$(REV)/acceptance
	for f in $(PKGFILES); do \
		echo Linking $$f; \
		ln $$f pkg/$(APP)-$(REV)/$$f; \
	done
	for ext in .e .el .java .pl .py .sh .rb .rc _implicit; do \
		for f in acceptance/*$$ext; do \
			echo Linking $$f; \
			ln $$f pkg/$(APP)-$(REV)/acceptance/`basename $$f`; \
		done; \
	done
	( cd pkg; tar zcvf $(APP)-$(REV).tgz $(APP)-$(REV) )
	@echo "Made $(APP) version $(REV)"

clean:				# Clean up all temporary/intermediate files
	for d in . acceptance; do \
		rm -f $$d/*~ $$d/*.bak $$d/core $$d/*.pyc; \
	done
	rm -f acceptance/*.html


clobber: clean			# Remove all generated files
	rm -rf html pkg acceptance/tmp

publish: ver package web upload		# Publish the Application

upload:	upload-html upload-tgz	# Upload a package and docs to the internet

install:			# Install the application
	cp $(APP) $(DEST)
	chmod 555 $(DEST)/$(APP)

help: FORCE			# Display make targets
	@egrep '^[-A-Za-z0-9]+:' Makefile \
		| grep "#" \
		| sed 's/:.*\(#\)/:/' \
		| awk -F: '{printf "%-10s --%s\n", $$1, $$2}' \
		| sort


# Minor Targets ======================================================

version.mk: source2html
	@echo "REV=" `./source2html -V | cut -f3 -d' '` > version.mk

pre-at: FORCE
	@echo
	@echo "Acceptance Tests ----------------------"
	@rm -rf acceptance/*.html acceptance/tmp acceptance/tmp_color

nohtml: FORCE
	rm -rf html/*

html:   FORCE
	mkdir -p html
	mkdir -p html/accept

man: html FORCE			# Create the man page
	rd2 <$(APP) \
	| sed -e 's/<BODY>/<BODY bgcolor="#FFFFFF">/'  >html/index.html

upload-web upload-html: FORCE
	ssh $(UPLOADHOST) rm -rf $(UPLOADDIR)
	ssh $(UPLOADHOST) mkdir -p $(UPLOADDIR)
	scp -rq html/* $(UPLOADHOST):$(UPLOADDIR)
	scp -q $(APP).blurb $(UPLOADHOST):$(TOOLSDIR)

upload-tgz:
	ftpupload \
		-cd tools \
		-set ok -md $(APP) \
		pkg/$(APP)-$(REV).tgz $(APP)/$(APP)-$(REV).tgz


FORCE:
