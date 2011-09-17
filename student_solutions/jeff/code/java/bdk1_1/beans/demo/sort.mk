
CLASSFILES= \
	sunw\demo\sort\BidirBubbleSortAlgorithm.class \
	sunw\demo\sort\QSortAlgorithm.class \
	sunw\demo\sort\SortItem.class \
	sunw\demo\sort\BubbleSortAlgorithm.class \
	sunw\demo\sort\SortAlgorithm.class

DATAFILES= \
	sunw\demo\sort\SorterBean.ser

JARFILE= ..\jars\sort.jar

all: $(JARFILE)

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\sort\*.class $(DATAFILES)
Name: sunw/demo/sort/SorterBean.ser
Java-Bean: True
<<


.SUFFIXES: .java .class

{sunw\demo\sort}.java{sunw\demo\sort}.class :
	set CLASSPATH=.
	javac $<

# Rule for running a program to create a serialized SorterBean.
sunw\demo\sort\SorterBean.ser: $(CLASSFILES) sunw\demo\sort\SorterBeanWriter.class
	set CLASSPATH=.
	java sunw.demo.sort.SorterBeanWriter $@

clean:
	-del sunw\demo\sort\*.class
	-del sunw\demo\sort\*.ser
	-del $(JARFILE)

