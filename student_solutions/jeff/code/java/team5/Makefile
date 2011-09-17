# Footella makefile
JFLAGS = -classpath ./src

CLASSES = MessageServer.class \
	MessageObject.class \
	ClientTest.class \
	Utility.class \
	Ping.class \
	PingHandler.class \
	Pong.class \
	PongHandler.class \
	Gateway.class \

# main target
all: 	$(CLASSES)

%.class : %.java
	javac $(JFLAGS) @<

# A target to remove non-source files
clean:
	rm: *.class


