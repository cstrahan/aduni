
SRCDIR = sunw\demo\encapsulatedEvents

CLASSFILES= 	$(SRCDIR)\EventMonitor.class				\
	 	$(SRCDIR)\EventMonitorBeanInfo.class			\
		$(SRCDIR)\EncapsulatedEvent.class			\
		$(SRCDIR)\EncapsulatedEventException.class		\
		$(SRCDIR)\EncapsulatedEventListener.class		\
		$(SRCDIR)\EncapsulatedEventManager.class		\
		$(SRCDIR)\EncapsulatedEventAdaptorGenerator.class

JARFILE= ..\jars\eventmonitor.jar

.SUFFIXES: .java .class

all: sunw.demo.classfile $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): sunw/demo/classfile $(CLASSFILES)
	jar cfm $(JARFILE) <<manifest.tmp $(SRCDIR)\*.class sunw\demo\classfile\*.class
Name: sunw/demo/encapsulatedEvents/EventMonitor.class
Java-Bean: True
<<

# make the (shared) sunw.demo.classfile classes.
sunw.demo.classfile:
	$(MAKE) -f classfile.mk -nologo all

# Rule for compiling a normal .java file

{$(SRCDIR)}.java{$(SRCDIR)}.class :
	set CLASSPATH=.
	javac $<


clean:
	$(MAKE) -f classfile.mk -nologo clean
	-del $(SRCDIR)\*.class
	-del $(JARFILE)

