same_gnome_helpdir = $(datadir)/gnome/help/samegnome/ru

same_gnome_help_DATA = 	\
	index.html		\
	topic.dat

EXTRA_DIST = same-gnome.sgml $(same_gnome_help_DATA)

all: index.html

# No-op if db2html failed
index.html: same-gnome/book1.html
	-cd $(srcdir) && cp same-gnome/book1.html index.html

same-gnome/book1.html: same-gnome.sgml
	-cd $(srcdir) && db2html same-gnome.sgml

dist-hook:
	-mkdir $(distdir)/same-gnome
	cp $(srcdir)/same-gnome/*.html $(distdir)/same-gnome
	cp $(srcdir)/same-gnome/*.css $(distdir)/same-gnome

# No-op if db2html failed
install-data-local: index.html
	-$(mkinstalldirs) $(DESTDIR)$(same_gnome_helpdir)
	files=`echo $(srcdir)/same-gnome/*.html $(srcdir)/same-gnome/*.css`; \
	if test "$$files" != '$(srcdir)/same-gnome/*.html $(srcdir)/same-gnome/*.css'; then \
	  for file in $$files; do \
	    basefile=`echo $$file | sed -e 's,^.*/,,'`; \
	    $(INSTALL_DATA) $$file $(DESTDIR)$(same_gnome_helpdir)/$$basefile; \
	  done; \
	fi

same-gnome.ps: same-gnome.sgml
	db2ps $<

same-gnome.rtf: same-gnome.sgml
	db2rtf $<
