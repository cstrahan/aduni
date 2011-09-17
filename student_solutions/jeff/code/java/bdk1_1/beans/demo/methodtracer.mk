
SRCDIR = sunw\demo\methodtracer

CLASSFILES= 	$(SRCDIR)\MethodTracer.class

JARFILE= ..\lib\methodtracer.jar

.SUFFIXES: .java .class

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): sunw/demo/classfile $(CLASSFILES)
	jar cfm $(JARFILE) <<manifest.tmp $(SRCDIR)\*.class
Name: sunw/demo/methodtracer/MethodTracer.class
Java-Bean: True
<<

# Rule for compiling a normal .java file

{$(SRCDIR)}.java{$(SRCDIR)}.class :
	set CLASSPATH=.
	javac $<


clean:
	-del $(SRCDIR)\*.class
	-del $(JARFILE)

