
CLASSFILES= \
	sunw\demo\juggler\Juggler.class \
	sunw\demo\juggler\JugglerBeanInfo.class

DATAFILES= \
	sunw\demo\juggler\Juggler0.gif \
	sunw\demo\juggler\Juggler1.gif \
	sunw\demo\juggler\Juggler2.gif \
	sunw\demo\juggler\Juggler3.gif \
	sunw\demo\juggler\Juggler4.gif \
	sunw\demo\juggler\JugglerIcon.gif

JARFILE= ..\jars\juggler.jar

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\juggler\*.class $(DATAFILES)
Name: sunw/demo/juggler/Juggler.class
Java-Bean: True
<<


.SUFFIXES: .java .class

{sunw\demo\juggler}.java{sunw\demo\juggler}.class :
	set CLASSPATH=.
	javac $<

# Run the Juggler as an applet inside AppletViewer.
view: all
	appletviewer html\juggler.html

clean:
	-del sunw\demo\juggler\*.class
	-del $(JARFILE)
