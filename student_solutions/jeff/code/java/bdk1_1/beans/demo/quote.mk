
CLASSFILES= \
	sunw\demo\quote\PropertyPanel.class \
	sunw\demo\quote\HttpProxy.class \
	sunw\demo\quote\YahooQuote.class \
	sunw\demo\quote\LocalQuote.class \
	sunw\demo\quote\QuoteServer.class \
	sunw\demo\quote\QuoteServerImpl.class \
	sunw\demo\quote\QuoteServerGUI.class \
	sunw\demo\quote\QuoteServerApp.class \
	sunw\demo\quote\QuoteListener.class \
	sunw\demo\quote\QuoteListenerImpl.class \
	sunw\demo\quote\QuoteEvent.class \
	sunw\demo\quote\QuoteMonitor.class \
	sunw\demo\quote\QuoteServerImpl_Stub.class \
	sunw\demo\quote\QuoteServerImpl_Skel.class \
	sunw\demo\quote\QuoteListenerImpl_Stub.class \
	sunw\demo\quote\QuoteListenerImpl_Skel.class

DATAFILES= \
	sunw\demo\quote\YahooQuoteLogo.gif

JARFILE= ..\jars\quote.jar

.SUFFIXES: .java .class

all: $(JARFILE)

run: all
	java sunw.demo.quote.QuoteServerApp

# Create a JAR file with a suitable manifest.

$(JARFILE): $(CLASSFILES) $(DATAFILES)
	jar cfm $(JARFILE) <<manifest.tmp sunw\demo\quote\*.class $(DATAFILES)
Name: sunw/demo/quote/QuoteMonitor.class
Java-Bean: True
<<

# Rule for compiling a normal .java file

{sunw\demo\quote}.java{sunw\demo\quote}.class :
	set CLASSPATH=.
	javac $<

# Create RMI stub .class files
sunw\demo\quote\QuoteServerImpl_Skel.class \
	sunw\demo\quote\QuoteServerImpl_Stub.class: \
		sunw\demo\quote\QuoteServerImpl.class
	set CLASSPATH=.
	rmic -d . sunw.demo.quote.QuoteServerImpl

# Create RMI stub .class files
sunw\demo\quote\QuoteObserverImpl_Skel.class \
	sunw\demo\quote\QuoteObserverImpl_Stub.class: \
		sunw\demo\quote\QuoteObserverImpl.class
	set CLASSPATH=.
	rmic -d . sunw.demo.quote.QuoteObserverImpl

# Create RMI stub .class files
sunw\demo\quote\QuoteListenerImpl_Skel.class \
	sunw\demo\quote\QuoteListenerImpl_Stub.class: \
		sunw\demo\quote\QuoteListenerImpl.class
	set CLASSPATH=.
	rmic -d . sunw.demo.quote.QuoteListenerImpl

clean:
	-del sunw\demo\quote\*.class
	-del $(JARFILE)

