# The makefile for the world's simplest Java bean.

CLASSFILES= \
	sunw\demo\molecule\Atom.class \
	sunw\demo\molecule\Matrix3D.class \
	sunw\demo\molecule\Molecule.class \
	sunw\demo\molecule\MoleculeNameEditor.class \
	sunw\demo\molecule\MoleculeBeanInfo.class \
	sunw\demo\molecule\XYZChemModel.class

DATAFILES=

JARFILE= ..\jars\molecule.jar

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\molecule\*.class sunw\demo\molecule\*.xyz $(DATAFILES)
Name: sunw/demo/molecule/Molecule.class
Java-Bean: True
<<

.SUFFIXES: .java .class

{sunw\demo\molecule}.java{sunw\demo\molecule}.class :
	set CLASSPATH=.
	javac $<

clean:
	-del sunw\demo\molecule\*.class
	-del $(JARFILE)

