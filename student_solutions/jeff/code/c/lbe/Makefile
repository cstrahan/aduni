#
# Makefile
#
LDFLAGS :=

# Switch the comment characters on the next two lines if
# you want to optimize
CFLAGS := -g $(CPPFLAGS)
#CFLAGS := -O2 $(CPPFLAGS)

PROGS =		\
	hello \
	pedant \
	newhello \
	newhello-lib \
	noret \
	inline \
		packme

all: $(PROGS)

.c.o:
	$(CC) $(CFLAGS) -c $*.c

.SUFFIXES: .c .o

hello: hello.c

pedant: pedant.c

newhello: showit.c msg.c
	$(CC) $(CFLAGS) $^ -o $@

newhello-lib: showit.c libmsg.a
	$(CC) $(CFLAGS) $< -o $@ -I. -L. -lmsg

libmsg.a: msg.c
	$(CC) $(CFLAGS) -c $< -o libmsg.o
	$(AR) rcs libmsg.a libmsg.o

noret: noret.c

inline: inline.c
	$(CC) -O2 $< -o $@

packme: packme.c

.PHONY : clean zip

clean:
	$(RM) $(PROGS) *.o *.a *~ *.out

zip: clean
	zip 215101cd.zip *.c *.h Makefile


