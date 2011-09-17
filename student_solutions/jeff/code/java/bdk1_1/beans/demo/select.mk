# The makefile for the world's simplest Java bean.

CLASSFILES= \
	sunw\demo\select\Select.class \
	sunw\demo\select\SelectCustomizer.class \
	sunw\demo\select\SelectBeanInfo.class \
	sunw\demo\select\Util.class

DATAFILES=

JARFILE= ..\jars\select.jar

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\select\*.class $(DATAFILES)
Name: sunw/demo/select/Select.class
Java-Bean: True
<<

.SUFFIXES: .java .class

{sunw\demo\select}.java{sunw\demo\select}.class :
	set CLASSPATH=.
	javac $<

clean:
	-del sunw\demo\select\*.class
	-del $(JARFILE)

