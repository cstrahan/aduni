
CLASSFILES= \
	sunw\demo\test\BridgeTester.class \
	sunw\demo\test\BridgeTesterCustomizer.class \
	sunw\demo\test\BridgeTesterBeanInfo.class \
	sunw\demo\test\BridgeTesterListener.class \
	sunw\demo\test\BridgeTesterEvent.class

DATAFILES= \
	sunw\demo\test\BridgeTesterIconColor16.gif \
	sunw\demo\test\BridgeTesterIconColor32.gif \
	sunw\demo\test\BridgeTesterIconMono16.gif \
	sunw\demo\test\BridgeTesterIconMono32.gif

JARFILE= ..\jars\test.jar

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\test\*.class $(DATAFILES)
Name: sunw/demo/test/BridgeTester.class
Java-Bean: True
<<


.SUFFIXES: .java .class

{sunw\demo\test}.java{sunw\demo\test}.class :
	set CLASSPATH=.
	javac $<

clean:
	-del sunw\demo\test\*.class
	-del $(JARFILE)

