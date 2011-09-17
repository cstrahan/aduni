scoredir=$(localstatedir)/games
pixmapdir = $(datadir)/pixmaps/same-gnome
Gamesdir = $(datadir)/gnome/apps/Games

SUBDIRS = help

INCLUDES = -I.. -I$(srcdir)/..		\
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\" \
	-I$(includedir)			\
	$(GNOME_INCLUDEDIR)

bin_PROGRAMS = same-gnome

same_gnome_SOURCES = 	\
	same-gnome.c

same_gnome_LDADD =		\
	$(GNOME_LIBDIR)         \
	$(GNOMEUI_LIBS)		\
	$(INTLLIBS)

pixmap_DATA = planets.png blink-sync.png marbles.png \
	stones.png rotate.png
EXTRA_DIST = same-gnome.desktop $(pixmap_DATA)

Games_DATA = same-gnome.desktop

install-data-local:
	-$(mkinstalldirs) $(DESTDIR)$(scoredir)
	touch $(DESTDIR)$(scoredir)/same-gnome.scores
	-chown games.games $(DESTDIR)$(scoredir)/same-gnome.scores
	-chmod 664 $(DESTDIR)$(scoredir)/same-gnome.scores
	-chgrp games $(DESTDIR)$(bindir)/same-gnome && chmod 2111 $(DESTDIR)$(bindir)/same-gnome
