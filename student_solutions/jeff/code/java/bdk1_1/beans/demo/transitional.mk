
CLASSFILES= \
	sunw\demo\transitional\ButtonPushEvent.class \
	sunw\demo\transitional\ButtonPushListener.class \
	sunw\demo\transitional\Flipper.class \
	sunw\demo\transitional\OurButton.class \
	sunw\demo\transitional\TransitionalBean.class

DATAFILES=

JARFILE= ..\jars\transitional.jar

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\transitional\*.class $(DATAFILES)
Name: sunw/demo/transitional/TransitionalBean.class
Java-Bean: True
<<


.SUFFIXES: .java .class

{sunw\demo\transitional}.java{sunw\demo\transitional}.class :
	set CLASSPATH=.
	javac -nowarn $<

clean:
	-del sunw\demo\transitional\*.class
	-del $(JARFILE)

