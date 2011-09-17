
CLASSFILES= \
	sunw\demo\buttons\ExplicitButton.class \
	sunw\demo\buttons\ExplicitButtonBeanInfo.class \
	sunw\demo\buttons\ExternalizableButton.class \
	sunw\demo\buttons\OurButton.class \
	sunw\demo\buttons\ExplicitButtonCustomizer.class 

DATAFILES= \
	sunw\demo\buttons\ExplicitButtonIcon16.gif \
	sunw\demo\buttons\ExplicitButtonIcon32.gif \
	sunw\demo\buttons\BlueButton.ser \
	sunw\demo\buttons\OrangeButton.ser

JARFILE= ..\jars\buttons.jar

.SUFFIXES: .java .class

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\buttons\*.class $(DATAFILES)
Name: sunw/demo/buttons/ExplicitButton.class
Java-Bean: True

Name: sunw/demo/buttons/OurButton.class
Java-Bean: True

Name: sunw/demo/buttons/OrangeButton.ser
Java-Bean: True

Name: sunw/demo/buttons/BlueButton.ser
Java-Bean: True
<<

# Rule for compiling a normal .java file

{sunw\demo\buttons}.java{sunw\demo\buttons}.class :
	set CLASSPATH=.
	javac $<

# Rule for running a program to create a serialized orange buton.
sunw\demo\buttons\OrangeButton.ser: sunw\demo\buttons\OrangeButtonWriter.class
	set CLASSPATH=.
	java sunw.demo.buttons.OrangeButtonWriter $@

# Rule for running a program to create a serialized Blue buton.
sunw\demo\buttons\BlueButton.ser: sunw\demo\buttons\BlueButtonWriter.class
	set CLASSPATH=.
	java sunw.demo.buttons.BlueButtonWriter $@

clean:
	-del sunw\demo\buttons\*.class
	-del sunw\demo\buttons\*.ser
	-del $(JARFILE)

