=begin
= html-parser-19990912p1

== What is html-parser

The html-parser package is a variant language implementation
of the Python's SGML parser (sgmllib.py), HTML parser (htmllib.py)
and Formatter (formatter.py).

== Files

* README.rd - RD document
* README.html - HTML document ganerated from README.rd by the rd2.
* install.rb - Installer

* sgml-parser.rb - SGML Parser Class
* html-parser.rb - HTML Parser Class
* formatter.rb - Formatter Class
* htmltest.rb - HTML Parser Test Script

=== sgml-parser.rb - SGML Parser Class

The sgml-parser.rb defines a class SGMLParser which serves as the basis
for parsing text files formatted in SGML (Standard Generalized Mark-up
Language).  In fact, it does not provide a full SGML parser -- it only
parses SGML insofar as it is used by HTML, and the module only exists
as a base for the HTMLParser class.

Please see
((<URL:http://www.python.org/doc/current/lib/module-sgmllib.html>))
for detail.

=== html-parser.rb - HTML Parser Class

The html-parser.rb defines a class HTMLParser which is a parser for
HTML documents.

Please see
((<URL:http://www.python.org/doc/current/lib/module-htmllib.html>))
for detail.

=== formatter.rb - Formatter Class

The formatter.rb defines 4 classes -- NullFormatter, AbstractFormatter,
NullWriter and DumbWriter -- which is a generic output formatter and
device interface.

Please see
((<URL:http://www.python.org/doc/current/lib/module-formatter.html>))
for detail.

=== htmltest.rb - HTML Parser Test Script

The htmltest.rb is a sample script using html-parser package.

Usage: htmltest.rb [HTML_FILE]

ex.) (({htmltest.rb index.html}))

== How to install

(({ruby install.rb}))

  or

(({cp -p formatter.rb html-parser.rb sgml-parser.rb ((*SOMEWHERE*))}))

== Author

Takahiro Maebashi <maebashi@iij.ad.jp>

== Packager

Katsuyuki Komatsu <komatsu@sarion.co.jp>

== History

: 2000-04-28
  [ruby-list:22188] Incorporated html-parser.rb and sgml-parser.rb patch
  contributed by Ryunosuke Ohshima <ryu@jaist.ac.jp>.
  Add README.rd and install.rb by the packager.

: 1999-11-10
  [ruby-dev:8302] Avoid arity check error of NullFormatter#push_font
  in formatter.rb by the packager.

: 1999-09-12
  html-parser-19990912.tar.gz released by the author.

: 1999-04-06
  [ruby-list:13345] html-parser-19990406.tar.gz released by the author.

: 1999-03-03
  [ruby-list:12521] html-parser-19990303.tar.gz released by the author.

: 1998-01-22
  [ruby-list:5974] html-parser.tar.gz (first release) released by the author.

=end
