# The makefile for the world's simplest Java bean.

CLASSFILES= \
	sunw\demo\jelly\JellyBean.class \
	sunw\demo\jelly\JellyBeanBeanInfo.class

DATAFILES= \
	sunw\demo\jelly\JellyBeanIconColor16.gif \
	sunw\demo\jelly\JellyBeanIconColor32.gif \
	sunw\demo\jelly\JellyBeanIconMono16.gif \
	sunw\demo\jelly\JellyBeanIconMono32.gif

JARFILE= ..\jars\jelly.jar

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\jelly\*.class $(DATAFILES)
Name: sunw/demo/jelly/JellyBean.class
Java-Bean: True
<<

.SUFFIXES: .java .class

{sunw\demo\jelly}.java{sunw\demo\jelly}.class :
	set CLASSPATH=.
	javac $<

clean:
	-del sunw\demo\jelly\*.class
	-del $(JARFILE)

