
MKFILES= \
	buttons.mk \
	eventmonitor.mk \
	jelly.mk \
	juggler.mk \
	methodtracer.mk \
	misc.mk \
	molecule.mk \
	quote.mk \
	select.mk \
	test.mk \
	transitional.mk \
	sort.mk \
	wrapper.mk

ACTIONS = all clean

default: $(MKFILES)

$(ACTIONS) :
	@nmake -nologo action=$@

$(MKFILES) : FORCE
	@echo.
	nmake -f $@ -nologo $(action)

FORCE:
