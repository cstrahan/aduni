
SUBDIRS = demo beanbox

ACTIONS = all clean

all: jars lib $(SUBDIRS)
	@$(MAKE) -nologo $(SUBDIRS) action=all

run:
	cd beanbox
	$(MAKE) -nologo run
	@cd ..

jars:
	mkdir $@

lib:
	mkdir $@

clean:
	@$(MAKE) -nologo $(SUBDIRS) action=clean
	@echo.
!if "$(OS)" == "Windows_NT"
        if exist classes/$(NULL) rmdir /s/q classes
        if exist jars/$(NULL) rmdir /s/q jars
        if exist lib/$(NULL) rmdir /s/q lib
!else
	-deltree/y jars
	-deltree/y classes
	-deltree/y lib
!endif

$(SUBDIRS) : FORCE
	@echo.
	cd $@
	$(MAKE) -nologo $(action)
	@cd ..

FORCE :
