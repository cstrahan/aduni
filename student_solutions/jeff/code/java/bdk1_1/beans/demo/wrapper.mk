
CLASSFILES= \
	sunw\wrapper\BeanWrapper.class

all: $(CLASSFILES)

.SUFFIXES: .java .class

{sunw\wrapper}.java{sunw\wrapper}.class :
	set CLASSPATH=.
	javac $<

# Run the Juggler as a wrapped applet inside AppletViewer.
view: all
	appletviewer html\wrapper.html

clean:
	-del  sunw\wrapper\*.class

