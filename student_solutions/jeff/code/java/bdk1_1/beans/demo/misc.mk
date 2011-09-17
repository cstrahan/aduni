
CLASSFILES= \
	sunw\demo\misc\Voter.class \
	sunw\demo\misc\TickTock.class \
	sunw\demo\misc\ChangeReporter.class

DATAFILES=

JARFILE= ..\jars\misc.jar

.SUFFIXES: .java .class

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\misc\*.class $(DATAFILES)
Name: sunw/demo/misc/Voter.class
Java-Bean: True

Name: sunw/demo/misc/TickTock.class
Java-Bean: True

Name: sunw/demo/misc/ChangeReporter.class
Java-Bean: True
<<

# Rule for compiling a normal .java file

{sunw\demo\misc}.java{sunw\demo\misc}.class :
	set CLASSPATH=.
	javac $<

clean:
	-del sunw\demo\misc\*.class
	-del $(JARFILE)

